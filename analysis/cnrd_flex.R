source("common.R")
e0 <- load_pkg("huncMarkovCNRD")
dupres <- readRDS(file.path(OUT_DIR, "dup_out.rds"))
o_all_orig <- list(mort="orig", hosp="orig", order="orig", timeline="orig", disc="orig", cutoff="orig", cap="orig", split="orig", hcc="orig")
o_all_dup  <- lapply(o_all_orig, function(x) "dup")

cnrd_flex <- function(p, arm, o, undisc = FALSE) {
  cl <- 0.25; K <- if (o$timeline == "orig") 320 else 319; k <- seq_len(K)
  age <- if (o$timeline == "orig") 42 + (k - 1) * cl else 42 + k * cl
  bg <- if (o$mort == "orig") sapply(age, function(a) e0$get_background_mortality(1, a, cl)) else 1 - exp(-ncd_rate(age) * cl)
  s_c <- p$smr_cnrd; s_d <- p$smr_diabx; s_v <- p$smr_cardioy
  if (o$cap == "orig") { base <- pmin(bg * s_c, .99); pd <- cbind(base, base, pmin(base + bg * s_d, .99), pmin(base + bg * s_v, .99), pmin(base + bg * (s_d + s_v), .99)) }
  else pd <- pmin(outer(bg, c(s_c, s_c, s_c + s_d, s_c + s_v, s_c + s_d + s_v)), 1)
  # line parameters
  r1 <- if (arm == "Velonix") p$p_resp_velonix else p$p_resp_doralene; r2 <- .5 * p$p_resp_mb2a + .5 * p$p_resp_mb2b
  alloc <- c(r1, (1 - r1) * r2, (1 - r1) * (1 - r2))
  rel <- c(if (arm == "Velonix") p$p_relapse_velonix else p$p_relapse_doralene, p$p_relapse_mb2, p$p_relapse_mb3)
  met <- c(if (arm == "Velonix") p$p_metabx_velonix else p$p_metabx_doralene, p$p_metabx_mb2, p$p_metabx_mb3)
  pdisc <- c(if (arm == "Velonix") p$p_disc_velonix else p$p_disc_doralene, p$p_disc_mb2)
  drug <- c(if (arm == "Velonix") p$cost_drug_velonix else p$cost_drug_doralene,
            .48 * p$cost_drug_mb2a + .52 * p$cost_drug_mb2b, .33 * p$cost_drug_mb2a + .37 * p$cost_drug_mb2b + .30 * p$cost_drug_mb3_clz_analogue)
  K0 <- if (o$cutoff == "orig") 80 else 79
  rel_t <- matrix(rel, K, 3, byrow = TRUE); aft <- k > K0
  rel_t[aft, ] <- .818 * rel_t[aft, ] + .182 * p$p_relapse_no_tx
  pdi <- p$p_diabx_if_metabx; pc <- p$p_cardioy_if_metabx
  Hm <- lapply(1:3, function(l) { H <- matrix(0, 5, 5); H[1, 1] <- 1 - met[l]; H[1, 2] <- met[l]
    if (o$split == "orig") { a <- pdi * (1 - pc); b <- pc * (1 - pdi); H[2, 3] <- a; H[2, 4] <- b; H[2, 5] <- pdi * pc; H[2, 2] <- 1 - a - b - pdi * pc }
    else { H[2, 3] <- pdi; H[2, 4] <- pc; H[2, 5] <- pdi * pc; H[2, 2] <- 1 - pdi - pc - pdi * pc }
    H[3, 3] <- 1 - pc; H[3, 5] <- pc; H[4, 4] <- 1 - pdi; H[4, 5] <- pdi; H[5, 5] <- 1; H })
  # simulate
  occ <- array(0, c(K, 5, 3)); cur <- lapply(1:3, function(l) c(alloc[l], 0, 0, 0, 0)); flows <- numeric(K)
  for (kk in k) {
    for (l in 1:3) occ[kk, , l] <- cur[[l]]
    if (kk == K) break
    act <- kk <= K0; q1 <- if (act) pdisc[1] else 0; q2 <- if (act) pdisc[2] else 0; s <- 1 - pd[kk, ]
    o1 <- cur[[1]]; o2 <- cur[[2]]; o3 <- cur[[3]]
    if (o$order == "orig") {
      d1 <- o1 * q1; d2 <- o2 * q2; flows[kk] <- sum(d1) + sum(d2)
      n1 <- as.vector(((o1 - d1) * s) %*% Hm[[1]]); n2 <- as.vector(((o2 - d2 + d1) * s) %*% Hm[[2]]); n3 <- as.vector(((o3 + d2) * s) %*% Hm[[3]])
    } else {
      n1 <- as.vector((o1 * s) %*% Hm[[1]]); n2 <- as.vector((o2 * s) %*% Hm[[2]]); n3 <- as.vector((o3 * s) %*% Hm[[3]])
      f1 <- q1 * n1; f2 <- q2 * n2; flows[kk] <- sum(f1) + sum(f2); n1 <- n1 - f1; n2 <- n2 + f1 - f2; n3 <- n3 + f2
    }
    cur <- list(n1, n2, n3)
  }
  w <- rep(1, K); if (o$hcc == "dup") w[K] <- .5
  tt <- if (o$disc == "orig") (k + .5) * cl else k * cl
  dq <- if (undisc) 1 else (1 + .03)^(-tt); dc <- 1.03^(-tt)
  base_cost <- (p$cost_physician_visit + p$cost_mh_clinic_visit + .5 * p$cost_group_per_hour) * 3
  rc <- 11 * p$cost_inpatient_relapse_daily + p$cost_ed_relapse_visit + p$cost_physician_visit + 2 * p$cost_mh_clinic_visit + 1.25 * p$cost_homecare_per_day + 3 * p$cost_group_relapse_per_hour + 2 * p$cost_group_per_hour
  c5 <- base_cost + c(0, 0, p$cost_diabx_quarterly, p$cost_cardioy_quarterly, p$cost_diabx_quarterly + p$cost_cardioy_quarterly)
  u5 <- p$util_stable_cnrd - c(0, p$disutil_metabx, p$disutil_diabx, p$disutil_cardioy, p$disutil_diabx + p$disutil_cardioy)
  st <- dr <- re <- qa <- lyd <- 0
  for (l in 1:3) { oh <- occ[, , l] * w; al <- rowSums(oh)
    st <- st + sum((oh %*% c5) * dc); dr <- dr + sum(al * drug[l] * cl * dc); re <- re + sum(al * rel_t[, l] * rc * dc)
    qa <- qa + sum(((oh %*% u5) - al * p$disutil_relapse * rel_t[, l]) * cl * dq); lyd <- lyd + sum(al * cl * dq) }
  hp <- 30000
  if (o$hosp == "orig") { acute <- 30000 * (1 + (1 - r1) + (1 - r1) * (1 - r2)); hosp <- acute / 1.03^(.5 * cl) }
  else { ex_init <- if (o$disc == "orig") .5 * cl else 0; ex_fl <- if (o$disc == "orig") (k + .5) * cl else (k + 1) * cl
         hosp <- 30000 / 1.03^ex_init + sum(30000 * flows / 1.03^ex_fl) }
  list(LY_disc = lyd, QALY = qa, Cost = st + dr + re + hosp, parts = c(Drug = dr, State_mgmt = st, Relapse = re, Hospitalisation = hosp))
}
run_flex <- function(p, o) { arms <- c("Velonix","Doralene"); lapply(setNames(arms, arms), function(a) { r <- cnrd_flex(p, a, o); r$LY_undisc <- cnrd_flex(p, a, o, TRUE)$LY_disc; r }) }
snap <- function(x) sapply(x, function(a) c(LY_disc=a$LY_disc, LY_undisc=a$LY_undisc, QALY=a$QALY, Cost=a$Cost))

if (sys.nframe() == 0) {
  options(width = 200)
  p <- e0$get_default_parameters()
  ref <- sapply(c("Velonix","Doralene"), function(a) { r <- e0$run_arm(p, a); c(LY_disc=r$total_lys, QALY=r$total_qalys, Cost=r$total_cost) })
  fo <- snap(run_flex(p, o_all_orig)); print(rbind(flex = fo[c(1,3,4), ], orig = ref)); cat("diff vs Original package:\n"); print(fo[c(1,3,4), ] - ref, digits = 3)
  # Dup parameter set
  pd <- p; pd$p_relapse_no_tx <- .12; pd$cost_drug_mb2a <- pd$cost_drug_mb2b <- pd$cost_drug_mb3_clz_analogue <- 38
  fd <- snap(run_flex(pd, o_all_dup)); d <- dupres$CNRD$det
  dd <- t(d[, c("LY_discounted","LY_undiscounted","QALY","Cost")]); colnames(dd) <- d$Arm
  print(fd); print(dd); cat("diff vs Duplicate:\n"); print(fd - dd, digits = 3)
}
