# Original packages as supplied (each with its own mortality table).
source("common.R")
zero_except <- function(p, all, keep) { p[setdiff(all, keep)] <- 0; p }
mk <- function(model, arm, ly_d, ly_u, qaly, cost, parts) list(model=model, arm=arm, LY_disc=ly_d, LY_undisc=ly_u, QALY=qaly, Cost=cost, parts=parts)
res <- list()

## ---- CAIS
e <- load_pkg("huncMarkovCAIS"); p <- e$get_default_parameters()
cn <- c("cost_ifg_annual","cost_uc_annual","cost_exac_event","cost_pathA_annual","cost_pathB_annual")
cais_orig <- function(e, p) {
  lapply(c(UsualCare="Usual_Care", Infectaguard="Infectaguard"), function(a) {
    tr <- e$generate_trace(p, a); o <- e$calculate_outcomes(tr, p, a)
    p0 <- p; p0$discount_rate_qalys <- 0; lu <- e$calculate_outcomes(tr, p0, a)$total_ly
    cp <- function(keep) e$calculate_outcomes(tr, zero_except(p, cn, keep), a)$total_cost
    list(LY_disc=o$total_ly, LY_undisc=lu, QALY=o$total_qaly, Cost=o$total_cost,
         parts=c(Background_UC=cp("cost_uc_annual"), Drug=cp("cost_ifg_annual"),
                 Chronic_infection=cp(c("cost_pathA_annual","cost_pathB_annual")), Exacerbation_events=cp("cost_exac_event")))
  })
}
res$CAIS <- cais_orig(e, p)

## ---- CISS
e <- load_pkg("huncMarkovCISS"); p <- e$get_default_parameters()
tx <- c("BSC","Dravokimab","Barisetinib","Tremacept","Uvestinib","Dorilumab")
scn <- c("cost_state_NonResponder","cost_state_Resp_Low","cost_state_Resp_Moderate","cost_state_Resp_High")
res$CISS <- lapply(setNames(tx, tx), function(a) {
  o <- e$calculate_outcomes(a, p); p0 <- p; p0$discount_rate_qalys <- 0; lu <- e$calculate_outcomes(a, p0)$total_lys
  dn <- c(paste0("cost_drug_", tx)); allc <- c(dn, scn, "sc_training_cost")
  cp <- function(keep) e$calculate_outcomes(a, zero_except(p, allc, keep))$total_costs
  list(LY_disc=o$total_lys, LY_undisc=lu, QALY=o$total_qalys, Cost=o$total_costs,
       parts=c(Drug=cp(paste0("cost_drug_", a)), State_mgmt=cp(scn), Injection_training=cp("sc_training_cost")))
})

## ---- CNRD
e <- load_pkg("huncMarkovCNRD"); p <- e$get_default_parameters()
cnrd_parts <- function(e, p, a) {
  drugs <- c("cost_drug_velonix","cost_drug_doralene","cost_drug_mb2a","cost_drug_mb2b","cost_drug_mb3_clz_analogue")
  e2 <- e  # overrides go in a copy of the environment
  mkenv <- function(base, relapse0, stable0) { en <- new.env(parent = globalenv()); for (n in ls(base, all.names=TRUE)) assign(n, get(n, base), en)
    environment(en$run_arm) <- en; environment(en$build_tmat) <- en; environment(en$get_tx_params) <- en; environment(en$run_acute_phase) <- en
    if (relapse0) en$get_relapse_cost <- function(params) 0
    if (stable0) en$get_stable_base_cost <- function(params) 0
    en }
  val <- function(en, pp) en$run_arm(pp, a)$total_cost
  pz <- function(pp, nm) { pp[nm] <- 0; pp }
  # Drug only
  pD <- pz(pz(p, c("cost_diabx_quarterly","cost_cardioy_quarterly","cost_inpatient_daily")), character(0))
  drug <- val(mkenv(e, TRUE, TRUE), pD)
  # State mgmt only (stable base + comorbidity mgmt)
  pS <- pz(p, c(drugs, "cost_inpatient_daily"))
  state <- val(mkenv(e, TRUE, FALSE), pS)
  # Relapse only
  pR <- pz(p, c(drugs, "cost_inpatient_daily","cost_diabx_quarterly","cost_cardioy_quarterly"))
  relapse <- val(mkenv(e, FALSE, TRUE), pR)
  # Hospitalisation (acute phase) only
  pH <- pz(p, c(drugs, "cost_diabx_quarterly","cost_cardioy_quarterly"))
  hosp <- val(mkenv(e, TRUE, TRUE), pH)
  c(Drug=drug, State_mgmt=state, Relapse=relapse, Hospitalisation=hosp)
}
res$CNRD <- lapply(c(Velonix="Velonix", Doralene="Doralene"), function(a) {
  o <- e$run_arm(p, a); p0 <- p; p0$discount_rate_qalys <- 0; lu <- e$run_arm(p0, a)$total_lys
  list(LY_disc=o$total_lys, LY_undisc=lu, QALY=o$total_qalys, Cost=o$total_cost, parts=cnrd_parts(e, p, a))
})

## ---- DMSS
e <- load_pkg("huncMarkovDMSS"); p <- e$get_default_parameters()
dm <- c("Cardivex","SoC","Zonapride","Septoplasty","Ablatherm")
cd <- c("cost_grade1_cycle","cost_grade2_cycle","cost_grade3_cycle","cost_cardivex_cycle","cost_zonapride_c1","cost_zonapride_cycle",
        "cost_septoplasty_proc","cost_ablatherm_proc","cost_zonapride_hosp","cost_scan","cost_betablock_c1","cost_betablock_cycle","cost_calcblock_c1","cost_calcblock_cycle")
res$DMSS <- lapply(setNames(dm, dm), function(a) {
  o <- e$.run_single_arm(a, p); p0 <- p; p0$discount_rate_annual <- 0; lu <- e$.run_single_arm(a, p0)$ly
  cp <- function(keep) e$.run_single_arm(a, zero_except(p, cd, keep))$cost
  list(LY_disc=o$ly, LY_undisc=lu, QALY=o$qaly, Cost=o$cost,
       parts=c(Drug=cp(c("cost_cardivex_cycle","cost_zonapride_c1","cost_zonapride_cycle")), Grade_state=cp(cd[1:3]),
               Procedure=cp(c("cost_septoplasty_proc","cost_ablatherm_proc")), Zona_hospitalisation=cp("cost_zonapride_hosp"),
               Cardiac_scans=cp("cost_scan"), Background_meds=cp(cd[11:14])))
})

## ---- NCD
e <- new.env(parent = globalenv())
for (f in list.files(file.path(root_o, "huncMarkovNCD/R"), "\\.R$", full.names = TRUE)) if (!grepl("app_launcher|package\\.R$", f)) sys.source(f, envir = e)
e$get_mortality_table <- function() read.csv(file.path(root_o, "huncMarkovNCD/inst/extdata/cdc_wonder_mortality_2020.csv"))
p <- e$get_default_parameters()
res$NCD <- lapply(c(UsualCare="Usual_care", Zelravine="Zelravine"), function(a) {
  tr <- e$generate_trace(p, a); o <- e$calculate_outcomes(tr, p, a)
  pr <- p; pr$c_retx <- 0; o2 <- e$calculate_outcomes(tr, pr, a)
  list(LY_disc=o$total_ly, LY_undisc=o$ly_undisc, QALY=o$total_qaly, Cost=o$total_cost,
       parts=c(State=o$cost_state, Initial_course=o2$cost_int, Retreatment=o$cost_int - o2$cost_int))
})
saveRDS(res, file.path(OUT_DIR, "orig_detail.rds"))
for (m in names(res)) { cat("\n==", m, "\n"); print(t(sapply(res[[m]], function(x) c(LYd=x$LY_disc, LYu=x$LY_undisc, QALY=x$QALY, Cost=x$Cost, sumparts=sum(x$parts)))), digits=8) }
