# Bridge: apply the differences one at a time, from the Original (common life table) to the Duplicate.
source("common.R")
dup <- readRDS(file.path(OUT_DIR, "dup_out.rds"))
snap <- function(x) sapply(x, function(a) c(LY_disc=a$LY_disc, LY_undisc=a$LY_undisc, QALY=a$QALY, Cost=a$Cost))
wf <- list()

## ================= CAIS =================
zero_except <- function(p, all, keep) { p[setdiff(all, keep)] <- 0; p }
cais_run <- function(e, p) lapply(c(UsualCare="Usual_Care", Infectaguard="Infectaguard"), function(a) {
  tr <- e$generate_trace(p, a); o <- e$calculate_outcomes(tr, p, a)
  p0 <- p; p0$discount_rate_qalys <- 0
  list(LY_disc=o$total_ly, LY_undisc=e$calculate_outcomes(tr, p0, a)$total_ly, QALY=o$total_qaly, Cost=o$total_cost)
})
e <- load_pkg("huncMarkovCAIS"); p <- e$get_default_parameters()
steps <- list(); steps[["0 Original"]] <- snap(cais_run(e, p))
e$get_background_mortality <- function(age, prop_female, cycle_length_years) 1 - exp(-ncd_rate(age) * cycle_length_years)
steps[["1 Mortality table"]] <- snap(cais_run(e, p))
p$rr_exac_pathA <- 1.25; p$rr_exac_pathB <- 1.30
steps[["2 Exac. RR in PathA/PathB swapped"]] <- snap(cais_run(e, p))
orig_tmat <- e$build_tmat
e$build_tmat <- function(params, treatment, age) {
  inf <- treatment == "Infectaguard"
  e0 <- params$p_exac_uc * (if (inf) params$rr_exac_ifg else 1)
  a <- params$p_pathA_uc * (if (inf) params$rr_pathA_ifg else 1); b <- params$p_pathB_uc * (if (inf) params$rr_pathB_ifg else 1)
  eA <- min(1, e0 * params$rr_exac_pathA); eB <- min(1, e0 * params$rr_exac_pathB)
  smr <- params$rr_mort_cais * params$rr_mort_exac^c(0,1,0,1,0,1) * params$rr_mort_pathA^c(0,0,1,1,0,0) * params$rr_mort_pathB^c(0,0,0,0,1,1)
  nm <- c("Stable","Stable_Exac","PathA","PathA_Exac","PathB","PathB_Exac","Dead")
  pd <- pmin(1, e$get_background_mortality(age, 0, 1) * smr); names(pd) <- nm[1:6]; s <- 1 - pd; r <- params$p_exac_recovery
  tm <- matrix(0, 7, 7, dimnames = list(nm, nm)); ninf <- 1 - a - b
  tm["Stable","Stable"] <- s["Stable"]*(1-e0)*ninf; tm["Stable","Stable_Exac"] <- s["Stable"]*e0*ninf
  tm["Stable","PathA"] <- s["Stable"]*(1-e0)*a; tm["Stable","PathA_Exac"] <- s["Stable"]*e0*a
  tm["Stable","PathB"] <- s["Stable"]*(1-e0)*b; tm["Stable","PathB_Exac"] <- s["Stable"]*e0*b
  tm["Stable_Exac","Stable"] <- s["Stable_Exac"]*r; tm["Stable_Exac","Stable_Exac"] <- s["Stable_Exac"]*(1-r)
  for (x in c("A","B")) { st <- paste0("Path", x); ex <- paste0(st, "_Exac")
    cl <- if (x=="A") params$p_pathA_clear else params$p_pathB_clear; ee <- if (x=="A") eA else eB
    tm[st,"Stable"] <- s[st]*cl*(1-ee); tm[st,"Stable_Exac"] <- s[st]*cl*ee; tm[st,ex] <- s[st]*(1-cl)*ee; tm[st,st] <- s[st]*(1-cl)*(1-ee)
    tm[ex,st] <- s[ex]*r; tm[ex,ex] <- s[ex]*(1-r) }
  tm[1:6,"Dead"] <- pd; tm["Dead","Dead"] <- 1; tm }
steps[["3 Transition structure"]] <- snap(cais_run(e, p))
wf$CAIS <- steps

## ================= CISS =================
e <- load_pkg("huncMarkovCISS"); p <- e$get_default_parameters(); tx <- c("BSC","Dravokimab","Barisetinib","Tremacept","Uvestinib","Dorilumab")
ciss_calc <- function(e, p, a, hcc, mid, undisc = FALSE) {
  n <- ceiling(p$time_horizon_years / p$cycle_length_years); cl <- p$cycle_length_years
  trace <- matrix(0, n + 1, 5); trace[1, 1] <- 1
  for (cyc in 1:n) trace[cyc + 1, ] <- trace[cyc, ] %*% e$build_tmat(cyc, a, p)
  R <- trace[2:(n + 1), ]; w <- rep(1, n); if (hcc) w[c(1, n)] <- 0.5; Rh <- R * w
  tt <- if (mid) (seq_len(n) - 0.5) * cl else (seq_len(n) - 1) * cl
  dq <- if (undisc) 1 else (1 + p$discount_rate_qalys)^(-tt); dc <- (1 + p$discount_rate_costs)^(-tt)
  sc <- c(p$cost_state_NonResponder, p$cost_state_Resp_Low, p$cost_state_Resp_Moderate, p$cost_state_Resp_High) * cl
  ndrug <- rowSums(Rh[, 2:4, drop = FALSE]); ndrug[1] <- sum(Rh[1, 1:4])
  u <- c(p$util_NonResponder, p$util_Resp_Low, p$util_Resp_Moderate, p$util_Resp_High, 0)
  U <- matrix(u, n, 5, byrow = TRUE); u1 <- c((p$util_NonResponder + c(p$util_NonResponder, p$util_Resp_Low, p$util_Resp_Moderate, p$util_Resp_High)) / 2, 0); U[1, ] <- u1
  cost <- sum(ndrug * p[[paste0("cost_drug_", a)]] * cl * dc) + sum((Rh[, 1:4] %*% sc) * dc) + (if (a %in% c("Tremacept","Dorilumab")) p$sc_training_cost * dc[1] else 0)
  list(LY_disc = sum(rowSums(Rh[, 1:4]) * cl * dq), QALY = sum(rowSums(Rh * U) * cl * dq), Cost = cost)
}
ciss_run <- function(e, p, hcc, mid) lapply(setNames(tx, tx), function(a) { o <- ciss_calc(e, p, a, hcc, mid); o$LY_undisc <- ciss_calc(e, p, a, hcc, mid, TRUE)$LY_disc; o })
steps <- list(); steps[["0 Original"]] <- snap(ciss_run(e, p, FALSE, TRUE))
chk <- e$run_model()$results_summary; stopifnot(max(abs(chk$total_cost - steps[[1]]["Cost", ])) < 1e-6, max(abs(chk$total_qaly - steps[[1]]["QALY", ])) < 1e-9)
p$p_death_annual <- ncd_rate(45)
steps[["1 Background mortality (NCD @45)"]] <- snap(ciss_run(e, p, FALSE, TRUE))
orig_tmat <- e$build_tmat
e$build_tmat <- function(cycle, treatment, params) { if (cycle == 1) { q <- params; q$p_death_annual <- 0; return(orig_tmat(cycle, treatment, q)) }; orig_tmat(cycle, treatment, params) }
steps[["2 No deaths in induction cycle"]] <- snap(ciss_run(e, p, FALSE, TRUE))
steps[["3 Start-of-cycle discounting"]] <- snap(ciss_run(e, p, FALSE, FALSE))
steps[["4 Half-cycle correction (first/last row)"]] <- snap(ciss_run(e, p, TRUE, FALSE))
wf$CISS <- steps

## ================= DMSS =================
e <- load_pkg("huncMarkovDMSS"); p <- e$get_default_parameters(); dm <- c("Cardivex","SoC","Zonapride","Septoplasty","Ablatherm")
dmss_run <- function(e, p) lapply(setNames(dm, dm), function(a) { o <- e$.run_single_arm(a, p); p0 <- p; p0$discount_rate_annual <- 0
  list(LY_disc=o$ly, LY_undisc=e$.run_single_arm(a, p0)$ly, QALY=o$qaly, Cost=o$cost) })
steps <- list(); steps[["0 Original"]] <- snap(dmss_run(e, p))
e$.bg_mort_cycle <- function(cycle, params) { age <- params$age_start + (cycle - 1) * params$cycle_length_years; 1 - exp(-ncd_rate(age) * params$cycle_length_years) }
steps[["1 Mortality table"]] <- snap(dmss_run(e, p))
e$.bg_mort_cycle <- function(cycle, params) { age <- params$age_start + (cycle - 2) * params$cycle_length_years; 1 - exp(-ncd_rate(age) * params$cycle_length_years) }
steps[["2 Mortality age lag (cycle t-1)"]] <- snap(dmss_run(e, p))
fz <- list(Cardivex=8, SoC=8, Zonapride=1, Septoplasty=1, Ablatherm=1)
e$generate_trace <- function(params, treatment) {
  n <- ceiling(params$time_horizon_years / params$cycle_length_years)
  tr <- matrix(0, n, 4, dimnames = list(NULL, c("Grade_I","Grade_II","Grade_III","Dead")))
  fr <- e$.frozen_dist(treatment, params); k <- fz[[treatment]]; base <- c(0, .68, .32)
  d <- matrix(fr, n, 3, byrow = TRUE); if (k > 1) d[seq_len(k - 1), ] <- matrix(base, k - 1, 3, byrow = TRUE)
  pm <- if (treatment == "Septoplasty") params$p_mort_septoplasty_proc else if (treatment == "Ablatherm") params$p_mort_ablatherm_proc else 0
  S <- numeric(n); S[1] <- 1
  for (i in 2:n) { S[i] <- S[i-1] * (1 - e$.bg_mort_cycle(i, params)); if (i == 2) S[i] <- S[i] * (1 - pm) }
  tr[, 1:3] <- d * S; tr[, 4] <- 1 - S; tr }
steps[["3 Grade split before freeze cycle"]] <- snap(dmss_run(e, p))
p$cost_betablock_c1 <- p$cost_betablock_cycle <- p$cost_calcblock_c1 <- p$cost_calcblock_cycle <- 0
steps[["4 Background medication cost excluded"]] <- snap(dmss_run(e, p))
p$cost_zonapride_c1 <- p$cost_zonapride_cycle
steps[["5 Zonapride cycle-1 drug cost"]] <- snap(dmss_run(e, p))
orig_u <- e$.get_utilities_cycle
e$.get_utilities_cycle <- function(params, treatment, cycle) {
  if (treatment == "Septoplasty") { # disutility cycles 2-7 instead of 1-6
    u <- orig_u(params, "Cardivex", cycle)[0]; }
  age_dec <- params$age_decrement_per_year * (params$age_start + (cycle - 1) * params$cycle_length_years - params$age_start)
  u <- switch(treatment, Cardivex = c(params$u_grade1_cardivex, params$u_grade2_cardivex, params$u_grade3_cardivex),
              SoC = c(params$u_grade1_soc, params$u_grade2_soc, params$u_grade3_soc),
              c(params$u_grade1_other, params$u_grade2_other, params$u_grade3_other))
  u <- pmax(0, u - age_dec)
  if (treatment == "Septoplasty") { if (cycle >= 2 && cycle <= 7) u <- pmax(0, u - params$disutility_septoplasty_proc); u <- u - params$disutility_pacemaker * params$pct_pacemaker_septoplasty }
  if (treatment == "Ablatherm") { if (cycle == 2) u <- pmax(0, u - params$disutility_ablatherm_proc); u <- u - params$disutility_pacemaker * params$pct_pacemaker_ablatherm }
  c(u, 0) }
steps[["6 Septoplasty disutility timing"]] <- snap(dmss_run(e, p))
orig_fd <- e$.frozen_dist
e$.frozen_dist <- function(treatment, params) { if (treatment %in% c("Septoplasty","Ablatherm")) { p1 <- params$p_grade1_septoplasty; return(c(Grade_I = p1, Grade_II = 0.19, Grade_III = 0.09)) }; orig_fd(treatment, params) }
steps[["7 Surgical-arm grade II/III split (0.19/0.09 vs 0.1904/0.0896)"]] <- snap(dmss_run(e, p))
wf$DMSS <- steps

## ================= CNRD =================
source("cnrd_flex.R")
p <- e0$get_default_parameters(); o <- o_all_orig; steps <- list()
steps[["0 Original"]] <- snap(run_flex(p, o))
o$mort <- "dup"; steps[["1 Mortality table"]] <- snap(run_flex(p, o))
p$p_relapse_no_tx <- 0.12; steps[["2 No-treatment relapse prob (0.40 -> 0.12)"]] <- snap(run_flex(p, o))
p$cost_drug_mb2a <- p$cost_drug_mb2b <- p$cost_drug_mb3_clz_analogue <- 38; steps[["3 Line 2/3 drug cost placeholder ($38/yr)"]] <- snap(run_flex(p, o))
o$hosp <- "dup"; steps[["4 Hospitalisation costing"]] <- snap(run_flex(p, o))
o$order <- "dup"; steps[["5 Discontinuation timing within cycle"]] <- snap(run_flex(p, o))
o$timeline <- "dup"; steps[["6 Stabilisation cycle / age offset"]] <- snap(run_flex(p, o))
o$disc <- "dup"; steps[["7 Discounting timing"]] <- snap(run_flex(p, o))
o$cutoff <- "dup"; steps[["8 Discontinuation/blend cutoff cycle"]] <- snap(run_flex(p, o))
o$cap <- "dup"; steps[["9 Mortality cap / SMR combination form"]] <- snap(run_flex(p, o))
o$split <- "dup"; steps[["10 MetabX -> DiabX/CardioY split"]] <- snap(run_flex(p, o))
o$hcc <- "dup"; steps[["11 Half-cycle weight on last row"]] <- snap(run_flex(p, o))
wf$CNRD <- steps

saveRDS(wf, file.path(OUT_DIR, "waterfall.rds"))
options(width = 220)
chk <- function(m, mapnames) { f <- wf[[m]][[length(wf[[m]])]]; d <- dup[[m]]$det
  cat("\n####", m, "final bridged step vs Duplicate\n")
  dd <- t(d[, c("LY_discounted","LY_undiscounted","QALY","Cost")]); colnames(dd) <- d$Arm
  print(rbind(bridged = NULL)); print(f, digits = 8); print(dd, digits = 8); print(f - dd[c(1,2,3,4), colnames(f)], digits = 4) }
chk("CAIS"); chk("CISS"); chk("DMSS"); chk("CNRD")
