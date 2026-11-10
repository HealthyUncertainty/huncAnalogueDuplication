# Discounted life years and QALYs by health state, both versions, both mortality bases.
source("common.R")
# patched CNRD flex with per-state LY / QALY
txt <- readLines("cnrd_flex.R")
txt <- txt[seq_len(grep("^if \\(sys.nframe", txt) - 1)]
src <- paste(txt, collapse = "\n")
src <- sub("st <- dr <- re <- qa <- lyd <- 0", "st <- dr <- re <- qa <- lyd <- 0; ly5 <- qa5 <- numeric(5); qrel <- 0", src, fixed = TRUE)
src <- sub("lyd <- lyd + sum(al * cl * dq) }", "lyd <- lyd + sum(al * cl * dq); ly5 <- ly5 + colSums(oh * cl * dq); qa5 <- qa5 + colSums(sweep(oh, 2, u5, '*') * cl * dq); qrel <- qrel - sum(al * p$disutil_relapse * rel_t[, l] * cl * dq) }", src, fixed = TRUE)
src <- sub("list(LY_disc = lyd, QALY = qa,", "list(ly5 = ly5, qa5 = qa5, qrel = qrel, LY_disc = lyd, QALY = qa,", src, fixed = TRUE)
eval(parse(text = src))

# ---- Duplicate envs
root <- DUP_DIR
files <- c(CAIS="model_cais.R", CISS="model_ciss.R", CNRD="model_cnrd.R", DMSS="model_dmss.R", NCD="model_ncd.R")
Sys.setenv(RUN_PSA = "FALSE"); denv <- list()
for (m in names(files)) { owd <- setwd(file.path(root, m)); env <- new.env(parent = globalenv()); invisible(capture.output(sys.source(files[[m]], envir = env))); denv[[m]] <- env; setwd(owd) }

rows <- list()
add <- function(basis, model, arm, version, ly, qaly) {
  rows[[length(rows) + 1]] <<- rbind(
    data.frame(basis, model, arm, version, measure = "LY",   state = names(ly),   value = as.numeric(ly)),
    data.frame(basis, model, arm, version, measure = "QALY", state = names(qaly), value = as.numeric(qaly)))
}
wsum <- function(M, w, u = NULL) { if (!is.null(u)) M <- sweep(M, 2, u, "*"); colSums(M * w) }

for (basis in c("ncd", "own")) {
  ncd <- basis == "ncd"
  ## ---- CAIS
  e <- load_pkg("huncMarkovCAIS"); p <- e$get_default_parameters()
  if (ncd) e$get_background_mortality <- function(age, prop_female, cycle_length_years) 1 - exp(-ncd_rate(age) * cycle_length_years)
  arms <- c(UsualCare = "Usual_Care", Infectaguard = "Infectaguard"); cl <- p$cycle_length_years
  for (a in names(arms)) {
    tr <- e$generate_trace(p, arms[[a]]); n <- nrow(tr); dq <- (1 + p$discount_rate_qalys)^(-(0:(n-1)) * cl)
    w <- rep(cl, n); w[c(1, n)] <- cl / 2; u <- e$get_state_utilities(p)[1:6]; S <- 1:6
    add(basis, "CAIS", a, "Original", wsum(tr[, S], w * dq), wsum(tr[, S], w * dq, u))
    env <- denv$CAIS; trh <- env$det[[a]]$trace * env$hcc_w; ud <- env$get_utilities(env$params_det)[env$alive]
    add(basis, "CAIS", a, "Duplicate", wsum(trh[, env$alive], env$cycle_length_years * env$disc_q), wsum(trh[, env$alive], env$cycle_length_years * env$disc_q, ud))
  }
  ## ---- CISS
  e <- load_pkg("huncMarkovCISS"); p <- e$get_default_parameters(); if (ncd) p$p_death_annual <- ncd_rate(45)
  cl <- p$cycle_length_years
  for (a in c("BSC","Dravokimab","Barisetinib","Tremacept","Uvestinib","Dorilumab")) {
    o <- e$calculate_outcomes(a, p); n <- nrow(o$trace) - 1; tr <- o$trace[2:(n+1), 1:4]
    dq <- 1 / (1 + p$discount_rate_qalys)^((seq_len(n) - .5) * cl)
    un <- p$util_NonResponder; uv <- c(un, p$util_Resp_Low, p$util_Resp_Moderate, p$util_Resp_High)
    um <- rbind((un + uv) / 2, matrix(uv, n - 1, 4, byrow = TRUE))
    add(basis, "CISS", a, "Original", wsum(tr, cl * dq), colSums(tr * um * cl * dq))
    env <- denv$CISS; trh <- env$det[[a]]$trace * env$hcc_w; al <- env$alive; pd <- env$params_det
    umd <- rbind(env$get_utilities(pd, TRUE)[al], matrix(env$get_utilities(pd)[al], env$n_cycles - 1, 4, byrow = TRUE))
    add(basis, "CISS", a, "Duplicate", wsum(trh[, al], env$cycle_length_years * env$disc_q), colSums(trh[, al] * umd * env$cycle_length_years * env$disc_q))
  }
  ## ---- CNRD
  e <- e0; p <- e$get_default_parameters()
  pd <- p; pd$p_relapse_no_tx <- .12; pd$cost_drug_mb2a <- pd$cost_drug_mb2b <- pd$cost_drug_mb3_clz_analogue <- 38
  sn <- c("No_Comorbidity","MetabX_Only","DiabX_Only","CardioY_Only","DiabX_CardioY")
  oo <- o_all_orig; if (ncd) oo$mort <- "dup"
  for (a in c("Velonix","Doralene")) {
    r <- cnrd_flex(p, a, oo); add(basis, "CNRD", a, "Original", setNames(r$ly5, sn), c(setNames(r$qa5, sn), Relapse_disutility = r$qrel))
    r <- cnrd_flex(pd, a, o_all_dup); add(basis, "CNRD", a, "Duplicate", setNames(r$ly5, sn), c(setNames(r$qa5, sn), Relapse_disutility = r$qrel))
  }
  ## ---- DMSS
  e <- load_pkg("huncMarkovDMSS"); p <- e$get_default_parameters()
  if (ncd) e$.bg_mort_cycle <- function(cycle, params) { age <- params$age_start + (cycle - 1) * params$cycle_length_years; 1 - exp(-ncd_rate(age) * params$cycle_length_years) }
  cl <- p$cycle_length_years
  for (a in c("Cardivex","SoC","Zonapride","Septoplasty","Ablatherm")) {
    tr <- e$generate_trace(p, a); n <- nrow(tr); dq <- (1 / (1 + p$discount_rate_annual))^((0:(n-1)) * cl)
    um <- t(sapply(seq_len(n), function(i) e$.get_utilities_cycle(p, a, i)[1:3]))
    add(basis, "DMSS", a, "Original", wsum(tr[, 1:3], cl * dq), colSums(tr[, 1:3] * um * cl * dq))
    ev <- new.env(parent = denv$DMSS); ev$arm <- a; ev$p <- denv$DMSS$params_det
    r <- eval(quote({
      d <- grade_dist(arm); S <- survival(arm); occ <- d * S; key <- util_key(arm)
      u3 <- c(p[[paste0("util_I_", key)]], p[[paste0("util_II_", key)]], p[[paste0("util_III_", key)]])
      base <- age_decrement * (age_t - age_decrement_ref); surg <- numeric(n_cycles)
      if (arm %in% c("Septoplasty", "Ablatherm")) { idx <- disutil_post_first_cycle + seq_len(disutil_post_n[[arm]]) - 1
        surg[idx[idx <= n_cycles]] <- disutil_post[[arm]]; surg[surgery_cycle:n_cycles] <- surg[surgery_cycle:n_cycles] + pacemaker_disutil * pacemaker_prob[[arm]] }
      list(ly = colSums(occ * cycle_length_years * disc_q),
           qaly = sapply(1:3, function(g) sum(occ[, g] * (u3[g] - base - surg) * cycle_length_years * disc_q)))
    }), ev)
    add(basis, "DMSS", a, "Duplicate", setNames(r$ly, c("Grade_I","Grade_II","Grade_III")), setNames(r$qaly, c("Grade_I","Grade_II","Grade_III")))
  }
  ## ---- NCD (naive + re-treated pooled by severity)
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(root_o, "huncMarkovNCD/R"), "\\.R$", full.names = TRUE)) if (!grepl("app_launcher|package\\.R$", f)) sys.source(f, envir = e)
  e$get_mortality_table <- function() read.csv(file.path(root_o, "huncMarkovNCD/inst/extdata/cdc_wonder_mortality_2020.csv"))
  p <- e$get_default_parameters(); cl <- p$cycle_length_years; sv <- c("Remission","Mild","Moderate","Severe")
  pool <- function(v) tapply(v, rep(1:4, 2), sum) |> setNames(sv)
  for (nm in c("UsualCare", "Zelravine")) {
    a <- c(UsualCare = "Usual_care", Zelravine = "Zelravine")[[nm]]
    tr <- e$generate_trace(p, a)$trace; n <- nrow(tr); dq <- (1 / (1 + p$discount_rate_qalys))^((0:(n-1)) * cl); su <- e$get_state_utilities(p)[1:8]
    add(basis, "NCD", nm, "Original",
        pool(wsum(tr[, 1:8], cl * dq)), pool(wsum(tr[, 1:8], cl * dq, su)))
  }
  env <- denv$NCD; pd <- env$params_det
  for (a in c("UsualCare", "Zelravine")) {
    tr <- env$det[[a]]$trace; if (is.null(tr)) tr <- env$run_arm(pd, a)$trace
    ut <- rep(c(pd$util_rem, pd$util_mild, pd$util_mod, pd$util_sev), 2); al <- env$alive
    add(basis, "NCD", a, "Duplicate", pool(wsum(tr[, al], env$cycle_length_years * env$disc_q)), pool(wsum(tr[, al], env$cycle_length_years * env$disc_q, ut)))
  }
}
st <- do.call(rbind, rows)
saveRDS(st, file.path(OUT_DIR, "states.rds"))
write.csv(st, file.path(OUT_DIR, "states.csv"), row.names = FALSE)
cat("rows:", nrow(st), "\n")
