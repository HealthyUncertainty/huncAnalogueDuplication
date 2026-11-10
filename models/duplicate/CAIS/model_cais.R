################################################################################
# CAIS COHORT MODEL - Infectaguard vs Usual Care (7 states, annual cycles, 100 years)
# Age-dependent background mortality x SMRs (multiplicative, on probability) embedded in the transition matrix.
# Run from this folder:  Rscript model_cais.R     (N_SIMS=200 Rscript model_cais.R for a quick test)
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
alive    <- setdiff(state_names, "Dead")
exac_st  <- c("Stable_Exac", "PathA_Exac", "PathB_Exac")

cycle_idx <- 0:(n_cycles - 1)
disc_c <- (1 / (1 + discount_rate_costs)) ^ (cycle_idx * cycle_length_years)
disc_q <- (1 / (1 + discount_rate_qalys)) ^ (cycle_idx * cycle_length_years)
hcc_w  <- rep(1, n_cycles); if (apply_hcc) hcc_w[c(1, n_cycles)] <- 0.5

# ==============================================================================
# STRUCTURE
# ==============================================================================
# SMR multiplier by alive state (order = alive states): SMRs MULTIPLY (Sec 3.4, revised)
smr_multiplier <- function(p) {
  has_exac  <- c(0, 1, 0, 1, 0, 1)
  has_pathA <- c(0, 0, 1, 1, 0, 0)
  has_pathB <- c(0, 0, 0, 0, 1, 1)
  p$smr_base * p$smr_exac^has_exac * p$smr_pathA^has_pathA * p$smr_pathB^has_pathB
}

# Arm-specific event probabilities (annual)
arm_probs <- function(p, arm) {
  inf <- (arm == "Infectaguard")
  e0 <- p$p_exac_uc  * if (inf) p$rr_exac  else 1
  a  <- p$p_pathA_uc * if (inf) p$rr_pathA else 1
  b  <- p$p_pathB_uc * if (inf) p$rr_pathB else 1
  list(e0 = e0,
       eA = min(1, e0 * rr_exac_pathA_mult),
       eB = min(1, e0 * rr_exac_pathB_mult),
       a = a, b = b)
}

# 7x7 transition matrix for one cycle at a given age (death applied first, then transitions)
build_tmat <- function(p, arm, age, smr_m) {
  q   <- arm_probs(p, arm)
  if (q$a + q$b >= 1) stop("Infection acquisition probabilities sum to >= 1")
  pd  <- pmin(1, (1 - exp(-bg_rate(age))) * smr_m)      # SMRs scale the background PROBABILITY
  names(pd) <- alive
  s   <- 1 - pd
  r   <- p$p_recover
  tm  <- matrix(0, n_states, n_states, dimnames = list(state_names, state_names))
  # Stable: exacerbation and infection acquisition are joint, competing PathA/PathB (mutually exclusive)
  ninf <- 1 - q$a - q$b
  tm["Stable", "Stable"]     <- s["Stable"] * (1 - q$e0) * ninf
  tm["Stable", "Stable_Exac"]<- s["Stable"] * q$e0       * ninf
  tm["Stable", "PathA"]      <- s["Stable"] * (1 - q$e0) * q$a
  tm["Stable", "PathA_Exac"] <- s["Stable"] * q$e0       * q$a
  tm["Stable", "PathB"]      <- s["Stable"] * (1 - q$e0) * q$b
  tm["Stable", "PathB_Exac"] <- s["Stable"] * q$e0       * q$b
  # Exacerbation states: recover with p=0.85, else remain
  tm["Stable_Exac", "Stable"]      <- s["Stable_Exac"] * r
  tm["Stable_Exac", "Stable_Exac"] <- s["Stable_Exac"] * (1 - r)
  # Chronic infection states: clearance -> Stable; exacerbation onset with elevated risk
  for (x in c("A", "B")) {
    st <- paste0("Path", x); ex <- paste0(st, "_Exac")
    cl <- if (x == "A") p$p_clear_A else p$p_clear_B
    e  <- if (x == "A") q$eA else q$eB
    tm[st, "Stable"]      <- s[st] * cl * (1 - e)
    tm[st, "Stable_Exac"] <- s[st] * cl * e
    tm[st, ex]            <- s[st] * (1 - cl) * e
    tm[st, st]            <- s[st] * (1 - cl) * (1 - e)
    tm[ex, st]            <- s[ex] * r
    tm[ex, ex]            <- s[ex] * (1 - r)
  }
  tm[alive, "Dead"] <- pd
  tm["Dead", "Dead"] <- 1
  if (any(abs(rowSums(tm) - 1) > 1e-10)) warning("Row sums != 1")
  tm
}

get_utilities <- function(p) {
  u0 <- p$util_stable
  c(Stable = u0, Stable_Exac = u0 * p$util_mult_exac,
    PathA = u0 * p$util_mult_pathA, PathA_Exac = u0 * p$util_mult_pathA * p$util_mult_exac,
    PathB = u0 * p$util_mult_pathB, PathB_Exac = u0 * p$util_mult_pathB * p$util_mult_exac, Dead = 0)
}

calculate_outcomes <- function(p, arm) {
  smr_m <- smr_multiplier(p)
  q <- arm_probs(p, arm)
  trace <- matrix(0, n_cycles, n_states, dimnames = list(NULL, state_names))
  trace[1, "Stable"] <- 1
  for (t in 2:n_cycles) {
    tm <- build_tmat(p, arm, start_age + (t - 2) * cycle_length_years, smr_m)   # age at start of cycle t-1
    trace[t, ] <- trace[t - 1, ] %*% tm
  }
  trh <- trace * hcc_w
  u   <- get_utilities(p)
  u_eff <- matrix(u, n_cycles, n_states, byrow = TRUE)

  alive_h <- rowSums(trh[, alive, drop = FALSE])
  c_bg    <- sum(alive_h * p$cost_uc_background * disc_c)
  c_drug  <- if (arm == "Infectaguard") sum(alive_h * p$cost_drug_inf * disc_c) else 0
  if (arm == "Infectaguard" && !drug_is_addon) c_bg <- 0
  c_inf   <- sum((rowSums(trh[, c("PathA", "PathA_Exac")]) * p$cost_pathA_extra +
                  rowSums(trh[, c("PathB", "PathB_Exac")]) * p$cost_pathB_extra) * disc_c)
  # Exacerbation cost accrues for every cycle spent in an exacerbation state (Sec 3.3, revised)
  c_event <- sum(rowSums(trh[, exac_st]) * p$cost_exac_event * disc_c)
  onsets  <- trh[, "Stable"] * q$e0 + trh[, "PathA"] * q$eA + trh[, "PathB"] * q$eB   # expected onsets (diagnostic)

  list(trace = trace,
       ly_disc   = sum(alive_h * cycle_length_years * disc_q),
       ly_undisc = sum(alive_h * cycle_length_years),
       cost      = c_bg + c_drug + c_inf + c_event,
       qaly      = sum(rowSums(trh * u_eff) * cycle_length_years * disc_q),
       cost_parts = c(Background_UC = c_bg, Drug = c_drug, Chronic_infection = c_inf, Exacerbation_events = c_event),
       exac_per_person = sum(onsets), exac_cycles_per_person = sum(rowSums(trh[, exac_st])),
       pathA_pct_end = trace[n_cycles, "PathA"] + trace[n_cycles, "PathA_Exac"])
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
cat("\n=== SANITY CHECKS: cohort conserved, no negatives, Dead non-decreasing -> PASS ===\n")
cat("(No published targets in this report version; Life Years -> Costs -> QALYs gates not applicable.)\n")

det_res <- data.frame(
  Arm = treatment_names,
  LY_discounted   = sapply(det, `[[`, "ly_disc"),
  LY_undiscounted = sapply(det, `[[`, "ly_undisc"),
  Cost = sapply(det, `[[`, "cost"),
  QALY = sapply(det, `[[`, "qaly"),
  Exacerbations_per_person = sapply(det, `[[`, "exac_per_person"),
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
  cost_psa <- qaly_psa <- matrix(NA_real_, n_sims, length(treatment_names), dimnames = list(NULL, treatment_names))
  for (j in seq_len(n_sims)) {
    pj <- get_params_iteration(l_params, j)
    for (a in treatment_names) { r <- calculate_outcomes(pj, a); cost_psa[j, a] <- r$cost; qaly_psa[j, a] <- r$qaly }
  }
  cat("\n=== PSA SUMMARY (", n_sims, " simulations; WTP ", wtp_threshold, ") ===\n", sep = "")
  psa_summary <- summarise_psa(cost_psa, qaly_psa, "UsualCare", wtp_threshold,
                               setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm))
  print(psa_summary, digits = 5)
  if (any(abs(c(psa_summary$Cost_PSA_vs_Det_pct, psa_summary$QALY_PSA_vs_Det_pct)) > 5))
    warning("PSA mean differs from deterministic by >5% - check parameter extraction.")
  results_list <- list(det = det_res, psa_cost = cost_psa, psa_qaly = qaly_psa, psa_summary = psa_summary)
  # saveRDS(results_list, "cais_results.rds")
}
cat("\n=== CAIS analysis complete ===\n")
