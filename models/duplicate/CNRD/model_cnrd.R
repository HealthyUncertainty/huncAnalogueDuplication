################################################################################
# CNRD COHORT MODEL - Velonix vs Doralene first-line (3 treatment lines x 5 alive states + Dead)
# Quarterly cycles, 320 cycles (80 years), start age 42, Canadian public payer.
# Cycle 1 = acute stabilisation (hospitalisation + line allocation, no accrual); Markov from cycle 2.
# Run from this folder:  Rscript model_cnrd.R     (N_SIMS=100 Rscript model_cnrd.R for a quick test)
################################################################################
source("model_parameters.R", local = TRUE)
n_sims  <- as.integer(Sys.getenv("N_SIMS", n_sims))
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

n_h <- length(health_states); n_l <- length(line_names)
# 18-state vector: 5 alive + Dead per line (Dead_Lx records the line at death)
state_names <- as.vector(sapply(line_names, function(l) paste0(c(health_states, "Dead"), "_", l)))

cycle_idx <- 0:(n_cycles - 1)
disc_c <- (1 / (1 + discount_rate_costs)) ^ (cycle_idx * cycle_length_years)
disc_q <- (1 / (1 + discount_rate_qalys)) ^ (cycle_idx * cycle_length_years)

# Accrual weights: cycle 1 (stabilisation) accrues no LY / QALY / state costs; half-cycle weight on last row
acc_w <- rep(1, n_cycles); acc_w[1] <- 0
if (apply_hcc) { acc_w[n_cycles] <- 0.5; if (hcc_half_first_markov_row) acc_w[2] <- 0.5 }

# Quarterly background death PROBABILITY by cycle (age at start of cycle, floor); SMRs scale this probability
p_bg_q <- 1 - exp(-bg_rate(start_age + cycle_idx * cycle_length_years) * cycle_length_years)

# ==============================================================================
# STRUCTURE
# ==============================================================================
smr_mult5 <- function(p, method = smr_method) {                # by health state (5 alive states)
  fd <- c(0, 0, 1, 0, 1); fc <- c(0, 0, 0, 1, 1)
  switch(method,
    additive_full   = p$smr_cnrd + fd * p$smr_diab + fc * p$smr_cardio,          # report: full SMR x background, additive
    multiplicative  = p$smr_cnrd * p$smr_diab^fd * p$smr_cardio^fc,
    additive_excess = 1 + (p$smr_cnrd - 1) + fd * (p$smr_diab - 1) + fc * (p$smr_cardio - 1),
    stop("Unknown smr_method"))
}

# Within-line health transition matrix (5x5), applied to SURVIVORS; no age dependence
build_H <- function(p_metab, p_d, p_c) {
  H <- matrix(0, n_h, n_h, dimnames = list(health_states, health_states))
  H["Stable", "Stable"] <- 1 - p_metab; H["Stable", "MetabX"] <- p_metab
  stay <- 1 - p_d - p_c - p_d * p_c
  H["MetabX", "MetabX"] <- stay; H["MetabX", "DiabX"] <- p_d; H["MetabX", "CardioY"] <- p_c
  H["MetabX", "DiabX_CardioY"] <- p_d * p_c
  H["DiabX", "DiabX"] <- 1 - p_c; H["DiabX", "DiabX_CardioY"] <- p_c
  H["CardioY", "CardioY"] <- 1 - p_d; H["CardioY", "DiabX_CardioY"] <- p_d
  H["DiabX_CardioY", "DiabX_CardioY"] <- 1
  if (any(abs(rowSums(H) - 1) > 1e-10) || any(H < 0)) stop("Invalid health transition matrix")
  H
}

# Line-specific inputs for an arm. Line 2 / 3 relapse and MetabX probabilities are blends of the
# component regimens (recomputed from sampled components in the PSA).
arm_line_params <- function(p, arm, blend = relapse_blend_after_80) {
  rel_comp <- c(a2 = p$p_relapse_2a, b2 = p$p_relapse_2b, cloz = p$p_relapse_cloz)
  met_comp <- c(a2 = p$p_metab_2a,   b2 = p$p_metab_2b,   cloz = p$p_metab_cloz)
  rel <- c(p[[paste0("p_relapse_L1_", arm)]], sum(w_line2 * rel_comp[names(w_line2)]), sum(w_line3 * rel_comp[names(w_line3)]))
  met <- c(p[[paste0("p_metab_L1_", arm)]],   sum(w_line2 * met_comp[names(w_line2)]), sum(w_line3 * met_comp[names(w_line3)]))
  # per-cycle relapse probability; after cycle 80 blended with the no-treatment probability
  rel_t <- matrix(rel, n_cycles, n_l, byrow = TRUE)
  if (blend) rel_t[cycle_idx + 1 > disc_last_cycle, ] <- blend_weight_line * rel_t[cycle_idx + 1 > disc_last_cycle, ] +
                                                         blend_weight_notx * p_relapse_notx
  r1 <- p[[paste0("resp_L1_", arm)]]; r2 <- p$resp_L2
  list(p_relapse = rel, rel_t = rel_t, p_metab = met,
       p_disc = c(p[[paste0("p_disc_L1_", arm)]], p$p_disc_L2),
       alloc  = c(r1, (1 - r1) * r2, (1 - r1) * (1 - r2)),
       drug_annual = c(if (arm == "Velonix") p$cost_drug_velonix_annual else p$cost_drug_doralene_annual,
                       cost_drug_L2_annual, cost_drug_L3_annual))
}

run_arm <- function(p, arm, smr_m = smr_mult5(p), blend = relapse_blend_after_80) {
  lp <- arm_line_params(p, arm, blend)
  H  <- lapply(1:n_l, function(l) build_H(lp$p_metab[l], p$p_diabx, p$p_cardioy))
  pd <- outer(p_bg_q, smr_m); pd[pd > 1] <- 1                       # n_cycles x 5 death prob by health state (capped at 1)

  occ  <- array(0, c(n_cycles, n_h, n_l), dimnames = list(NULL, health_states, line_names))
  dead <- matrix(0, n_cycles, n_l, dimnames = list(NULL, line_names))
  hosp_flow <- numeric(n_cycles); n_sw1 <- n_sw2 <- 0
  hosp_flow[1] <- 1                                                     # stabilisation: every patient (cycle 1)
  occ[1, "Stable", ] <- lp$alloc                                        # allocation by response
  occ[2, "Stable", ] <- lp$alloc                                        # Markov starts at cycle 2; no mortality in cycle 1

  for (t in 2:(n_cycles - 1)) {
    new <- vector("list", n_l); dnew <- dead[t, ]
    for (l in 1:n_l) {
      o <- occ[t, , l]
      dnew[l] <- dnew[l] + sum(o * pd[t, ])
      new[[l]] <- as.vector((o * (1 - pd[t, ])) %*% H[[l]])
    }
    if (t <= disc_last_cycle) {                                         # cascade L1->L2->L3 (alive pts, same health state)
      f1 <- lp$p_disc[1] * new[[1]]; f2 <- lp$p_disc[2] * new[[2]]      # f2 uses pre-transfer L2 occupancy
      new[[1]] <- new[[1]] - f1
      new[[2]] <- new[[2]] + f1 - f2
      new[[3]] <- new[[3]] + f2
      hosp_flow[t + 1] <- sum(f1) + sum(f2); n_sw1 <- n_sw1 + sum(f1); n_sw2 <- n_sw2 + sum(f2)
    }
    for (l in 1:n_l) occ[t + 1, , l] <- new[[l]]
    dead[t + 1, ] <- dnew
  }

  # Costs / utilities per state (5 health states), line-specific drug and relapse
  c5 <- p$cost_stable_q + c(0, 0, p$cost_diab_q, p$cost_cardio_q, p$cost_diab_q + p$cost_cardio_q)
  u5 <- p$util_stable - c(0, p$disutil_metab, p$disutil_diab, p$disutil_cardio, p$disutil_diab + p$disutil_cardio)
  c_state <- c_drug <- c_relapse <- qaly <- ly_d <- ly_u <- n_relapse <- 0
  for (l in 1:n_l) {
    oh <- occ[, , l] * acc_w                                            # accrual-weighted occupancy
    al <- rowSums(oh); pr <- lp$rel_t[, l]
    c_state   <- c_state   + sum((oh %*% c5) * disc_c)
    c_drug    <- c_drug    + sum(al * lp$drug_annual[l] * cycle_length_years * disc_c)
    c_relapse <- c_relapse + sum(al * pr * p$cost_relapse * disc_c)
    qaly      <- qaly      + sum(((oh %*% u5) - al * p$disutil_relapse * pr) * cycle_length_years * disc_q)
    ly_d      <- ly_d      + sum(al * cycle_length_years * disc_q)
    ly_u      <- ly_u      + sum(al * cycle_length_years)
    n_relapse <- n_relapse + sum(al * pr)
  }
  c_hosp <- sum(hosp_flow * p$cost_hosp_line_entry * disc_c)
  trace <- cbind(occ[, , 1], dead[, 1], occ[, , 2], dead[, 2], occ[, , 3], dead[, 3])
  colnames(trace) <- state_names
  list(trace = trace, ly_disc = ly_d, ly_undisc = ly_u, qaly = qaly,
       cost = c_state + c_drug + c_relapse + c_hosp,
       cost_parts = c(Drug = c_drug, State_mgmt = c_state, Relapse = c_relapse, Hospitalisation = c_hosp),
       relapses_per_person = n_relapse, switch_L1_L2 = n_sw1, switch_L2_L3 = n_sw2,
       alloc = lp$alloc, blends = c(rel_L2 = lp$p_relapse[2], rel_L3 = lp$p_relapse[3],
                                    metab_L2 = lp$p_metab[2], metab_L3 = lp$p_metab[3]))
}

# ==============================================================================
# DETERMINISTIC ANALYSIS + SANITY CHECKS
# ==============================================================================
l_params   <- make_l_params(df_params_input, n_sims)
params_det <- get_params_deterministic(l_params)
stopifnot(!any(sapply(params_det, is.null)))

det <- setNames(lapply(treatment_names, function(a) run_arm(params_det, a)), treatment_names)
# Component blends must reproduce the report's published Line 2 / Line 3 values (Tables 3.1, 3.2)
b <- det[[1]]$blends
stopifnot(abs(b["rel_L2"] - 0.1148) < 5e-5, abs(b["rel_L3"] - 0.1073) < 5e-5,
          abs(b["metab_L2"] - 0.0417) < 5e-5, abs(b["metab_L3"] - 0.0606) < 5e-5)
for (a in treatment_names) {
  tr <- det[[a]]$trace
  dead_cols <- grep("^Dead", colnames(tr))
  stopifnot(all(abs(rowSums(tr) - 1) < 1e-9), all(tr >= -1e-12),
            all(diff(rowSums(tr[, dead_cols])) >= -1e-12), ncol(tr) == 18,
            sum(tr[1:2, dead_cols]) == 0)                              # no deaths in stabilisation quarter
}
cat("\n=== SANITY CHECKS: 18-state cohort conserved, no negatives, deaths non-decreasing, ",
    "no deaths in cycle 1, L2/L3 blends match report -> PASS ===\n", sep = "")
cat("(No published targets in this report; Life Years -> Costs -> QALYs gates not applicable.)\n")
cat("SMR combination method (base case):", smr_method, "| relapse blend after cycle 80:", relapse_blend_after_80, "\n")
cat("Line allocation at cycle 1 (L1/L2/L3), Velonix:", round(det$Velonix$alloc, 3),
    "| Doralene:", round(det$Doralene$alloc, 3), "\n")

det_res <- data.frame(
  Arm = treatment_names,
  LY_discounted   = sapply(det, `[[`, "ly_disc"),
  LY_undiscounted = sapply(det, `[[`, "ly_undisc"),
  Cost = sapply(det, `[[`, "cost"),
  QALY = sapply(det, `[[`, "qaly"),
  Relapses_per_person = sapply(det, `[[`, "relapses_per_person"),
  Switches_L1_L2 = sapply(det, `[[`, "switch_L1_L2"),
  Switches_L2_L3 = sapply(det, `[[`, "switch_L2_L3"),
  row.names = NULL)
cat("\n=== DETERMINISTIC RESULTS ===\n"); print(det_res, digits = 6)
cat("\nCost breakdown by component:\n"); print(sapply(det, `[[`, "cost_parts"), digits = 6)
cat("\nIncremental analysis (Velonix vs Doralene):\n")
print(icer_table(setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm)), digits = 6)

cat("\n=== SENSITIVITY: SMR convention and post-cycle-80 relapse blend (placeholder input) ===\n")
sens_cases <- list(list("additive_full", TRUE), list("multiplicative", TRUE), list("additive_excess", TRUE),
                   list("additive_full", FALSE))
sens <- do.call(rbind, lapply(sens_cases, function(s) {
  o <- lapply(treatment_names, function(a) run_arm(params_det, a, smr_mult5(params_det, s[[1]]), s[[2]]))
  data.frame(SMR_method = s[[1]], Relapse_blend = s[[2]], LY_Vel = o[[1]]$ly_disc, LY_Dor = o[[2]]$ly_disc,
             QALY_Vel = o[[1]]$qaly, QALY_Dor = o[[2]]$qaly, Cost_Vel = o[[1]]$cost, Cost_Dor = o[[2]]$cost,
             ICER = (o[[1]]$cost - o[[2]]$cost) / (o[[1]]$qaly - o[[2]]$qaly))
}))
print(sens, digits = 6, row.names = FALSE)

# ==============================================================================
# PSA
# ==============================================================================
results_list <- list(det = det_res)
if (run_psa) {
  cost_psa <- qaly_psa <- matrix(NA_real_, n_sims, length(treatment_names), dimnames = list(NULL, treatment_names))
  for (j in seq_len(n_sims)) {
    pj <- get_params_iteration(l_params, j)
    for (a in treatment_names) { r <- run_arm(pj, a); cost_psa[j, a] <- r$cost; qaly_psa[j, a] <- r$qaly }
  }
  cat("\n=== PSA SUMMARY (", n_sims, " simulations; WTP ", wtp_threshold, ") ===\n", sep = "")
  psa_summary <- summarise_psa(cost_psa, qaly_psa, "Doralene", wtp_threshold,
                               setNames(det_res$Cost, det_res$Arm), setNames(det_res$QALY, det_res$Arm))
  print(psa_summary, digits = 5)
  if (any(abs(c(psa_summary$Cost_PSA_vs_Det_pct, psa_summary$QALY_PSA_vs_Det_pct)) > 5))
    warning("PSA mean differs from deterministic by >5% - check parameter extraction.")
  results_list <- list(det = det_res, psa_cost = cost_psa, psa_qaly = qaly_psa, psa_summary = psa_summary)
  # saveRDS(results_list, "cnrd_results.rds")
}
cat("\n=== CNRD analysis complete ===\n")
