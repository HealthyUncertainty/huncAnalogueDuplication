################################################################################
# CISS COHORT MODEL - BSC vs 5 targeted therapies (16-week cycles, 17 cycles, 5 years)
# Induction (cycle 1) -> maintenance (cycles 2-17) Markov model, US payer.
# Run from this folder:  Rscript model_ciss.R     (N_SIMS=200 Rscript model_ciss.R for a quick test)
################################################################################
source("model_parameters.R", local = TRUE)
n_sims <- as.integer(Sys.getenv("N_SIMS", n_sims))
run_psa <- as.logical(Sys.getenv("RUN_PSA", run_psa))
if (use_seed) set.seed(seed_value)

# ==============================================================================
# SHARED HELPERS (distribution moment-matching, PSA sampling, incremental analysis)
# ==============================================================================
bdist <- function(mean, se) {                  # Beta: probabilities
  a <- mean * ((mean * (1 - mean) / se^2) - 1)
  if (!is.finite(a) || a <= 0) stop("Beta moment-matching failed (mean=", mean, ", se=", se, ")")
  c(alpha = a, beta = (1 - mean) / mean * a)
}
gdist <- function(mean, se) c(shape = mean^2 / se^2, scale = se^2 / mean)   # Gamma
PtoR  <- function(p, t) -1 / t * log(1 - p)   # probability -> rate
RtoP  <- function(r, t) 1 - exp(-r * t)       # rate -> probability

round_counts <- function(p, N) {               # integer counts summing to N (largest remainder)
  x <- p / sum(p) * N; a <- floor(x); k <- N - sum(a)
  if (k > 0) { i <- order(x - a, decreasing = TRUE)[seq_len(k)]; a[i] <- a[i] + 1 }
  a
}
rdirichlet <- function(n, alpha) {
  g <- sapply(alpha, function(a) if (a > 0) rgamma(n, a) else rep(0, n))
  g / rowSums(g)
}

# Sample every PSA parameter once (vectorised, outside the PSA loop).
# groups: list(list(cols = <parameter names>, alpha = <integer counts>)) for Dirichlet blocks.
build_psa <- function(df_in, n_sims, groups = list()) {
  psa <- matrix(NA_real_, n_sims, nrow(df_in), dimnames = list(NULL, df_in$Parameter))
  for (i in seq_len(nrow(df_in))) {
    m <- df_in$Mean[i]; s <- df_in$SE[i]
    psa[, i] <- switch(df_in$Distribution[i],
      Beta      = { p <- bdist(m, s); rbeta(n_sims, p["alpha"], p["beta"]) },
      Gamma     = { p <- gdist(m, s); rgamma(n_sims, p["shape"], scale = p["scale"]) },
      Utility   = { p <- gdist(1 - m, s); 1 - rgamma(n_sims, p["shape"], scale = p["scale"]) },
      LogNormal = exp(rnorm(n_sims, log(m), s)),
      Normal    = rnorm(n_sims, m, s),
      Dirichlet = NA_real_,                        # filled jointly below
      Fixed     = rep(m, n_sims),
      stop("Unknown distribution: ", df_in$Distribution[i]))
  }
  for (g in groups) psa[, g$cols] <- rdirichlet(n_sims, g$alpha)
  psa
}
make_l_params <- function(df_in, n_sims, groups = list())
  list(varname = df_in$Parameter, varmean = df_in$Mean,
       df_params = as.data.frame(build_psa(df_in, n_sims, groups)))
get_params_deterministic <- function(l) { p <- as.list(l$varmean); names(p) <- l$varname; p }
get_params_iteration     <- function(l, j) { p <- as.list(unlist(l$df_params[j, ])); names(p) <- l$varname; p }

# Incremental analysis on the efficiency frontier (strict + extended dominance)
icer_table <- function(cost, qaly) {
  d <- data.frame(Strategy = names(cost), Cost = as.numeric(cost), QALY = as.numeric(qaly))
  d <- d[order(d$Cost, -d$QALY), ]; rownames(d) <- NULL
  keep <- rep(TRUE, nrow(d))
  for (i in seq_len(nrow(d))[-1]) if (any(d$QALY[seq_len(i - 1)] >= d$QALY[i])) keep[i] <- FALSE
  f <- d[keep, ]
  repeat {
    if (nrow(f) < 3) break
    ic <- diff(f$Cost) / diff(f$QALY)
    bad <- which(diff(ic) < 0)
    if (!length(bad)) break
    f <- f[-(bad[1] + 1), ]
  }
  d$Status <- ifelse(!keep, "Dominated", ifelse(d$Strategy %in% f$Strategy, "Frontier", "Ext. dominated"))
  d$ICER <- NA_real_
  if (nrow(f) > 1) d$ICER[match(f$Strategy[-1], d$Strategy)] <- diff(f$Cost) / diff(f$QALY)
  d
}

# PSA summary: mean cost/QALY per arm, incremental vs a reference arm, P(max net benefit)
summarise_psa <- function(cost_mat, qaly_mat, reference, wtp, det_cost = NULL, det_qaly = NULL) {
  # cost_mat / qaly_mat: n_sims x n_arms (columns named by arm)
  arms <- colnames(cost_mat)
  nmb  <- qaly_mat * wtp - cost_mat
  best <- arms[max.col(nmb, ties.method = "first")]
  out <- data.frame(
    Arm        = arms,
    PSA_cost   = colMeans(cost_mat),
    PSA_QALY   = colMeans(qaly_mat),
    Inc_cost   = colMeans(cost_mat) - mean(cost_mat[, reference]),
    Inc_QALY   = colMeans(qaly_mat) - mean(qaly_mat[, reference]),
    P_best_NMB = sapply(arms, function(a) mean(best == a)),
    row.names  = NULL)
  out$ICER_vs_ref <- ifelse(out$Arm == reference, NA, out$Inc_cost / out$Inc_QALY)
  if (!is.null(det_cost)) {
    out$Det_cost <- as.numeric(det_cost[arms]); out$Det_QALY <- as.numeric(det_qaly[arms])
    out$Cost_PSA_vs_Det_pct <- 100 * (out$PSA_cost / out$Det_cost - 1)
    out$QALY_PSA_vs_Det_pct <- 100 * (out$PSA_QALY / out$Det_QALY - 1)
  }
  out
}

n_states <- length(state_names)
alive    <- setdiff(state_names, "Death")
resp_st  <- c("Resp_Low", "Resp_Moderate", "Resp_High")

# Discount factors - uniform cycle steps; HCC is applied to the trace, not the exponent
cycle_idx <- 0:(n_cycles - 1)
disc_c <- (1 / (1 + discount_rate_costs)) ^ (cycle_idx * cycle_length_years)
disc_q <- (1 / (1 + discount_rate_qalys)) ^ (cycle_idx * cycle_length_years)
hcc_w  <- rep(1, n_cycles); if (apply_hcc) hcc_w[c(1, n_cycles)] <- 0.5

# ==============================================================================
# STRUCTURE
# ==============================================================================
induction_dist <- function(p, trt) {
  pl <- p[[paste0("p_rlow_", trt)]]; pm <- p[[paste0("p_rmod_", trt)]]; ph <- p[[paste0("p_rhigh_", trt)]]
  s <- pl + pm + ph
  if (s > 1) { pl <- pl / s; pm <- pm / s; ph <- ph / s; s <- 1 }      # guard for PSA draws
  c(NonResponder = 1 - s, Resp_Low = pl, Resp_Moderate = pm, Resp_High = ph, Death = 0)
}

# Maintenance transition matrix (cycles 2-17); survivors may discontinue to NonResponder
build_tmat_maint <- function(p, trt, cycle) {
  pdisc <- if (cycle <= disc_year1_last_cycle) p[[paste0("p_disc1_", trt)]] else p[[paste0("p_disc2_", trt)]]
  tm <- matrix(0, n_states, n_states, dimnames = list(state_names, state_names))
  tm["NonResponder", ] <- c(1 - p_death_cycle, 0, 0, 0, p_death_cycle)         # absorbing alive state
  for (r in resp_st) {
    tm[r, "NonResponder"] <- (1 - p_death_cycle) * pdisc
    tm[r, r]              <- (1 - p_death_cycle) * (1 - pdisc)
    tm[r, "Death"]        <- p_death_cycle
  }
  tm["Death", "Death"] <- 1
  if (any(abs(rowSums(tm) - 1) > 1e-10)) warning("Row sums != 1")
  tm
}

generate_trace <- function(p, trt) {
  trace <- matrix(0, n_cycles, n_states, dimnames = list(NULL, state_names))
  row1  <- induction_dist(p, trt)
  if (deaths_in_induction_cycle) { row1[alive] <- row1[alive] * (1 - p_death_cycle); row1["Death"] <- p_death_cycle }
  trace[1, ] <- row1
  disc_flow <- numeric(n_cycles)                          # responders lost to discontinuation
  for (t in 2:n_cycles) {
    tm <- build_tmat_maint(p, trt, t)
    trace[t, ] <- trace[t - 1, ] %*% tm
    pdisc <- if (t <= disc_year1_last_cycle) p[[paste0("p_disc1_", trt)]] else p[[paste0("p_disc2_", trt)]]
    disc_flow[t] <- sum(trace[t - 1, resp_st]) * (1 - p_death_cycle) * pdisc
  }
  list(trace = trace, disc_flow = disc_flow)
}

get_utilities <- function(p, first_cycle = FALSE) {
  u <- c(NonResponder = p$util_NonResponder, Resp_Low = p$util_Resp_Low,
         Resp_Moderate = p$util_Resp_Moderate, Resp_High = p$util_Resp_High, Death = 0)
  if (first_cycle) u[resp_st] <- (p$util_NonResponder + u[resp_st]) / 2   # induction: mean of baseline & new state
  u
}
get_state_costs_cycle <- function(p)                                    # annual -> per cycle
  c(NonResponder = p$cost_annual_NonResponder, Resp_Low = p$cost_annual_Resp_Low,
    Resp_Moderate = p$cost_annual_Resp_Moderate, Resp_High = p$cost_annual_Resp_High, Death = 0) * cycle_length_years

calculate_outcomes <- function(p, trt) {
  g  <- generate_trace(p, trt); tr <- g$trace
  trh <- tr * hcc_w                                                    # half-cycle-corrected trace
  sc  <- get_state_costs_cycle(p)
  drug_cycle <- p[[paste0("cost_drug_annual_", trt)]] * cycle_length_years
  n_on_drug  <- rowSums(trh[, resp_st, drop = FALSE])                  # maintenance: responders only
  n_on_drug[1] <- sum(trh[1, alive])                                   # cycle 1: ALL alive patients
  c_state <- sum((trh %*% sc) * disc_c)
  c_drug  <- sum(n_on_drug * drug_cycle * disc_c)
  c_train <- if (trt %in% injectables) p$cost_sc_training * disc_c[1] else 0      # one-off, all pts, cycle 1
  u_mat   <- rbind(get_utilities(p, TRUE), matrix(get_utilities(p), n_cycles - 1, n_states, byrow = TRUE))
  alive_h <- rowSums(trh[, alive, drop = FALSE])
  list(trace = tr,
       ly_disc   = sum(alive_h * cycle_length_years * disc_q),
       ly_undisc = sum(alive_h * cycle_length_years),
       cost      = c_state + c_drug + c_train,
       qaly      = sum(rowSums(trh * u_mat) * cycle_length_years * disc_q),
       cost_parts = c(Drug = c_drug, State_mgmt = c_state, Injection_training = c_train),
       discontinuations_per_person = sum(g$disc_flow))
}

# ==============================================================================
# DETERMINISTIC ANALYSIS + SANITY CHECKS
# ==============================================================================
l_params   <- make_l_params(df_params_input, n_sims)
params_det <- get_params_deterministic(l_params)
stopifnot(!any(sapply(params_det, is.null)), n_cycles == ceiling(time_horizon_years / cycle_length_years))
cat(sprintf("Per-cycle background death probability (age %d, NCD table): %.5f\n", ciss_mortality_age, p_death_cycle))

det <- setNames(lapply(treatment_names, function(a) calculate_outcomes(params_det, a)), treatment_names)
for (a in treatment_names) {
  tr <- det[[a]]$trace
  stopifnot(all(abs(rowSums(tr) - 1) < 1e-9), all(tr >= -1e-12), all(diff(tr[, "Death"]) >= -1e-12))
}
cat("\n=== SANITY CHECKS: cohort conserved, no negatives, Death non-decreasing -> PASS ===\n")
cat("(No published targets in this report version; Life Years -> Costs -> QALYs gates not applicable.)\n")
# Cycle-1 cost must exceed a later cycle's (induction applies drug to ALL alive patients)
for (a in setdiff(treatment_names, "BSC")) {
  tr <- det[[a]]$trace; dc <- params_det[[paste0("cost_drug_annual_", a)]] * cycle_length_years
  stopifnot(sum(tr[1, alive]) * dc > sum(tr[2, resp_st]) * dc)
}

det_res <- data.frame(
  Arm = treatment_names,
  LY_discounted   = sapply(det, `[[`, "ly_disc"),
  LY_undiscounted = sapply(det, `[[`, "ly_undisc"),
  Cost = sapply(det, `[[`, "cost"),
  QALY = sapply(det, `[[`, "qaly"),
  Discontinuations_per_person = sapply(det, `[[`, "discontinuations_per_person"),
  row.names = NULL)
cat("\n=== DETERMINISTIC RESULTS ===\n"); print(det_res, digits = 6)
cat("\nCost breakdown by component:\n"); print(sapply(det, `[[`, "cost_parts"), digits = 6)
cat("\nIncremental analysis (efficiency frontier):\n")
print(icer_table(setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm)), digits = 6)

# ==============================================================================
# PSA
# ==============================================================================
results_list <- list(det = det_res)
if (run_psa) {
  cost_psa <- qaly_psa <- matrix(NA_real_, n_sims, length(treatment_names), dimnames = list(NULL, treatment_names))
  for (j in seq_len(n_sims)) {
    pj <- get_params_iteration(l_params, j)
    for (a in treatment_names) { r <- calculate_outcomes(pj, a); cost_psa[j, a] <- r$cost; qaly_psa[j, a] <- r$qaly }
  }
  cat("\n=== PSA SUMMARY (", n_sims, " simulations; WTP ", wtp_threshold, ") ===\n", sep = "")
  psa_summary <- summarise_psa(cost_psa, qaly_psa, "BSC", wtp_threshold,
                               setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm))
  print(psa_summary, digits = 5)
  if (any(abs(c(psa_summary$Cost_PSA_vs_Det_pct, psa_summary$QALY_PSA_vs_Det_pct)) > 5))
    warning("PSA mean differs from deterministic by >5% - check parameter extraction.")
  results_list <- list(det = det_res, psa_cost = cost_psa, psa_qaly = qaly_psa, psa_summary = psa_summary)
  # saveRDS(results_list, "ciss_results.rds")
}
cat("\n=== CISS analysis complete ===\n")
