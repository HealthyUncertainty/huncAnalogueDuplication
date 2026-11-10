################################################################################
# NCD COHORT MODEL - Zelravine vs Usual Care
# Year-1 decision tree + annual Markov model (9 states), age 45-100, US payer.
# Run from this folder:  Rscript model_ncd.R     (N_SIMS=200 Rscript model_ncd.R for a quick test)
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

# Counts check (Table 3.1) -------------------------------------------------------
for (g in dir_counts) {
  p <- df_params_input$Mean[match(g$cols, df_params_input$Parameter)]
  stopifnot(all(round_counts(p, g$N) == g$alpha), sum(g$alpha) == g$N)
}

# ==============================================================================
# STRUCTURE
# ==============================================================================
sev_names   <- c("Remission", "Mild", "Moderate", "Severe")
sev_key     <- c("rem", "mild", "mod", "sev")
state_names <- c(sev_names, paste0(sev_names, "_R"), "Death")
n_states    <- length(state_names)
alive       <- setdiff(state_names, "Death")

cycle_idx <- 0:(n_cycles - 1)
disc_c <- (1 / (1 + discount_rate_costs)) ^ (cycle_idx * cycle_length_years)   # cycle t -> 1/1.03^(t-1)
disc_q <- (1 / (1 + discount_rate_qalys)) ^ (cycle_idx * cycle_length_years)

get_dist <- function(p, grp) unlist(p[paste0("p_sev_", grp, "_", sev_key)])

run_arm <- function(p, arm) {
  key      <- arm_key[[arm]]
  arm_dist <- get_dist(p, key)
  base_d   <- get_dist(p, "base")
  zel      <- (arm == "Zelravine")

  trace <- matrix(0, n_cycles, n_states, dimnames = list(NULL, state_names))
  trace[1, sev_names] <- arm_dist                       # all alive at age 45 (Sec 2.5)
  retreat_flow <- numeric(n_cycles)                     # patients entering R states, by entry cycle
  mult <- c(1, rep(p$rr_mort_symptomatic, 3))           # Remission RR=1; Mild/Mod/Severe RR
  mult8 <- rep(mult, 2)

  for (t in seq_len(n_cycles - 1)) {
    rate  <- bg_rate(start_age + t - 1)                 # age at start of cycle t
    pd    <- 1 - exp(-rate * mult8)
    occ   <- trace[t, ]
    surv  <- occ[alive] * (1 - pd)                      # deaths removed BEFORE re-treatment
    naive <- surv[1:4]; ret <- surv[5:8]
    if (zel && t <= retreat_last_cycle) {
      flow  <- p$p_retreat * naive
      naive <- naive - flow
      ret   <- ret + sum(flow) * arm_dist               # redistributed by post-treatment split
      retreat_flow[t + 1] <- sum(flow)
    }
    trace[t + 1, 1:4] <- naive
    trace[t + 1, 5:8] <- ret
    trace[t + 1, "Death"] <- occ["Death"] + sum(occ[alive] * pd)
  }

  # Valuation occupancy: optional scenario values first w weeks of year 1 on baseline split
  occ_val <- trace
  w <- year1_baseline_weeks / 52
  if (w > 0) occ_val[1, sev_names] <- w * base_d + (1 - w) * arm_dist

  state_cost <- rep(c(p$cost_state_rem, p$cost_state_mild, p$cost_state_mod, p$cost_state_sev), 2)
  util       <- rep(c(p$util_rem, p$util_mild, p$util_mod, p$util_sev), 2)

  c_state   <- sum((occ_val[, alive] %*% state_cost) * disc_c) * cycle_length_years
  c_initial <- if (zel) p$cost_course_initial * disc_c[1] else 0        # all zelravine pts, cycle 1
  c_retreat <- sum(retreat_flow * p$cost_course_retreat * disc_c)
  alive_occ <- rowSums(trace[, alive, drop = FALSE])

  list(
    trace = trace, retreat_flow = retreat_flow,
    ly_disc   = sum(alive_occ * disc_q) * cycle_length_years,
    ly_undisc = sum(alive_occ) * cycle_length_years,
    qaly      = sum((occ_val[, alive] %*% util) * disc_q) * cycle_length_years,
    cost      = c_state + c_initial + c_retreat,
    cost_parts = c(State = c_state, Initial_course = c_initial, Retreatment = c_retreat),
    retreated_per_person = sum(retreat_flow)
  )
}

# ==============================================================================
# DETERMINISTIC ANALYSIS + SANITY CHECKS
# ==============================================================================
l_params   <- make_l_params(df_params_input, n_sims, dir_counts)
params_det <- get_params_deterministic(l_params)
stopifnot(!any(sapply(params_det, is.null)))

det <- setNames(lapply(treatment_names, function(a) run_arm(params_det, a)), treatment_names)

for (a in treatment_names) {
  tr <- det[[a]]$trace
  stopifnot(all(abs(rowSums(tr) - 1) < 1e-9), all(tr >= -1e-12), all(diff(tr[, "Death"]) >= -1e-12))
}
cat("\n=== SANITY CHECKS: cohort conserved, no negatives, Death non-decreasing -> PASS ===\n")
cat("(No published targets in this report version; Life Years -> Costs -> QALYs gates not applicable.)\n")

det_res <- data.frame(
  Arm = treatment_names,
  LY_discounted   = sapply(det, `[[`, "ly_disc"),
  LY_undiscounted = sapply(det, `[[`, "ly_undisc"),
  Cost  = sapply(det, `[[`, "cost"),
  QALY  = sapply(det, `[[`, "qaly"),
  Retreated_per_person = sapply(det, `[[`, "retreated_per_person"),
  row.names = NULL)
cat("\n=== DETERMINISTIC RESULTS ===\n"); print(det_res, digits = 6)
cat("\nCost breakdown by component:\n"); print(sapply(det, `[[`, "cost_parts"), digits = 6)
cat("\nIncremental analysis:\n")
print(icer_table(setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm)), digits = 6)

# ==============================================================================
# PSA
# ==============================================================================
results_list <- list(det = det_res)
if (run_psa) {
  cost_psa <- qaly_psa <- ly_psa <- matrix(NA_real_, n_sims, length(treatment_names),
                                           dimnames = list(NULL, treatment_names))
  for (j in seq_len(n_sims)) {
    pj <- get_params_iteration(l_params, j)
    for (a in treatment_names) {
      r <- run_arm(pj, a)
      cost_psa[j, a] <- r$cost; qaly_psa[j, a] <- r$qaly; ly_psa[j, a] <- r$ly_disc
    }
  }
  cat("\n=== PSA SUMMARY (", n_sims, " simulations; WTP ", wtp_threshold, ") ===\n", sep = "")
  psa_summary <- summarise_psa(cost_psa, qaly_psa, "UsualCare", wtp_threshold,
                               setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm))
  print(psa_summary, digits = 5)
  # Skill check: PSA means should be within ~5% of deterministic
  if (any(abs(c(psa_summary$Cost_PSA_vs_Det_pct, psa_summary$QALY_PSA_vs_Det_pct)) > 5))
    warning("PSA mean differs from deterministic by >5% - check parameter extraction.")
  results_list <- list(det = det_res, psa_cost = cost_psa, psa_qaly = qaly_psa, psa_summary = psa_summary)
  # saveRDS(results_list, "ncd_results.rds")
}
cat("\n=== NCD analysis complete ===\n")
