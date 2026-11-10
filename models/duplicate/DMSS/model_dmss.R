################################################################################
# DMSS COHORT MODEL - 5 strategies, population-proportion (frozen grade distribution) structure
# 4-week cycles, 1,040 cycles (80 years), start age 55, US payer, no half-cycle correction.
# Run from this folder:  Rscript model_dmss.R     (N_SIMS=200 Rscript model_dmss.R for a quick test)
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

cycle_idx <- 0:(n_cycles - 1)
disc_c <- (1 / (1 + discount_rate_costs)) ^ (cycle_idx * cycle_length_years)
disc_q <- (1 / (1 + discount_rate_qalys)) ^ (cycle_idx * cycle_length_years)
age_t  <- start_age + cycle_idx * cycle_length_years
p_bg   <- 1 - exp(-bg_rate(age_t) * cycle_length_years)          # background death prob per cycle (by cycle)
state_names <- c(grade_names, "Dead")

# ==============================================================================
# STRUCTURE
# ==============================================================================
# Frozen distribution among survivors at each cycle (n_cycles x 3)
grade_dist <- function(arm) {
  fz <- frozen[[arm]]; k <- freeze_cycle[[arm]]
  t  <- seq_len(n_cycles)
  if (pre_freeze_method == "linear") {
    w <- if (k == 1) rep(1, n_cycles) else pmin(1, (t - 1) / (k - 1))
    return(outer(1 - w, baseline_dist) + outer(w, fz))
  }
  d <- matrix(fz, n_cycles, 3, byrow = TRUE)
  if (k > 1) d[t < k, ] <- matrix(baseline_dist, sum(t < k), 3, byrow = TRUE)
  d
}

# Survival of the cohort (alive proportion at the start of each cycle)
survival <- function(arm) {
  s <- numeric(n_cycles); s[1] <- 1
  for (t in 2:n_cycles) {
    s[t] <- s[t - 1] * (1 - p_bg[t - 1])
    if (t == periop_cycle) s[t] <- s[t] * (1 - periop_mort[[arm]])      # one-time perioperative mortality
  }
  s
}

scan_vector <- function(arm) {                                     # 1 in cycles with a cardiac scan
  v <- numeric(n_cycles)
  if (arm == "Cardivex") v[scan_cycles_cardivex] <- 1 else v[seq(1, n_cycles, by = scan_interval_other)] <- 1
  v
}

util_key <- function(arm) if (arm == "Cardivex") "cardivex" else if (arm == "SoC") "soc" else "other"

calculate_outcomes <- function(p, arm) {
  d <- grade_dist(arm); S <- survival(arm)
  occ <- d * S                                                    # n_cycles x 3 (state occupancy)
  key <- util_key(arm)
  u3  <- c(p[[paste0("util_I_", key)]], p[[paste0("util_II_", key)]], p[[paste0("util_III_", key)]])
  c3  <- c(p$cost_state_I, p$cost_state_II, p$cost_state_III)

  # Utility: grade utility - age decrement (relative to start age) - surgical disutilities
  u_t <- as.vector(d %*% u3) - age_decrement * (age_t - age_decrement_ref)
  surg <- numeric(n_cycles)
  if (arm %in% c("Septoplasty", "Ablatherm")) {
    idx <- disutil_post_first_cycle + seq_len(disutil_post_n[[arm]]) - 1
    surg[idx[idx <= n_cycles]] <- disutil_post[[arm]]
    surg[surgery_cycle:n_cycles] <- surg[surgery_cycle:n_cycles] + pacemaker_disutil * pacemaker_prob[[arm]]
  }
  u_t <- u_t - surg

  # Costs
  c_state <- sum((occ %*% c3) * disc_c)
  c_drug  <- switch(arm,
    Cardivex  = sum(S * p$cost_cardivex_cycle * disc_c),
    Zonapride = sum(S * p$cost_zonapride_cycle * disc_c), 0)
  c_proc  <- switch(arm, Septoplasty = p$cost_septoplasty, Ablatherm = p$cost_ablatherm, 0) * S[surgery_cycle] * disc_c[surgery_cycle]
  c_hosp  <- if (arm == "Zonapride") p$cost_zona_hosp * S[1] * disc_c[1] else 0
  c_scan  <- sum(scan_vector(arm) * S * p$cost_scan * disc_c)
  c_bg    <- sum(S * cost_bgmed_cycle[[arm]] * disc_c)

  trace <- cbind(occ, Dead = 1 - S); colnames(trace) <- state_names
  list(trace = trace,
       ly_disc   = sum(S * cycle_length_years * disc_q),
       ly_undisc = sum(S * cycle_length_years),
       qaly      = sum(S * u_t * cycle_length_years * disc_q),
       cost      = c_state + c_drug + c_proc + c_hosp + c_scan + c_bg,
       cost_parts = c(Drug = c_drug, Grade_state = c_state, Procedure = c_proc,
                      Zona_hospitalisation = c_hosp, Cardiac_scans = c_scan, Background_meds = c_bg))
}

# ==============================================================================
# DETERMINISTIC ANALYSIS + SANITY CHECKS
# ==============================================================================
l_params   <- make_l_params(df_params_input, n_sims)
params_det <- get_params_deterministic(l_params)
stopifnot(!any(sapply(params_det, is.null)))

det <- setNames(lapply(treatment_names, function(a) calculate_outcomes(params_det, a)), treatment_names)
for (a in treatment_names) {
  tr <- det[[a]]$trace
  stopifnot(all(abs(rowSums(tr) - 1) < 1e-9), all(tr >= -1e-12), all(diff(tr[, "Dead"]) >= -1e-12))
}
# Frozen-distribution check: grade split among survivors equals the Table 3.1 split after the freeze cycle
for (a in treatment_names) {
  tr <- det[[a]]$trace; k <- freeze_cycle[[a]] + 1
  stopifnot(all(abs(tr[k, grade_names] / sum(tr[k, grade_names]) - frozen[[a]]) < 1e-9))
}
cat("\n=== SANITY CHECKS: cohort conserved, Dead non-decreasing, frozen split reproduced -> PASS ===\n")
cat("(No published targets in this report version; Life Years -> Costs -> QALYs gates not applicable.)\n")
cat("Cycle-1 mortality check: surgical arms lose", sprintf("%.1f%% / %.1f%%", 100 * periop_mort[["Septoplasty"]],
    100 * periop_mort[["Ablatherm"]]), "additional at cycle 2 (see Dead column)\n")

det_res <- data.frame(
  Arm = treatment_names,
  LY_discounted   = sapply(det, `[[`, "ly_disc"),
  LY_undiscounted = sapply(det, `[[`, "ly_undisc"),
  Cost = sapply(det, `[[`, "cost"),
  QALY = sapply(det, `[[`, "qaly"),
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
  psa_summary <- summarise_psa(cost_psa, qaly_psa, "SoC", wtp_threshold,
                               setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm))
  print(psa_summary, digits = 5)
  if (any(abs(c(psa_summary$Cost_PSA_vs_Det_pct, psa_summary$QALY_PSA_vs_Det_pct)) > 5))
    warning("PSA mean differs from deterministic by >5% - check parameter extraction.")
  results_list <- list(det = det_res, psa_cost = cost_psa, psa_qaly = qaly_psa, psa_summary = psa_summary)
  # saveRDS(results_list, "dmss_results.rds")
}
cat("\n=== DMSS analysis complete ===\n")
