################################################################################
# MODEL PARAMETERS - CISS (Chronic Immune Skin Syndrome; BSC + 5 targeted therapies)
# Source: "Cost-Effectiveness Analysis of Treatments for CISS" (Dravex, March 2026),
#         Tables 3.1-3.4 and Sections 2.1-2.4.
# Convention: [ASSUMPTION] = not stated in the source; exposed as a named setting.
################################################################################

# ==============================================================================
# CONFIGURATION (Section 2.1)
# ==============================================================================
time_horizon_years  <- 5
cycle_length_years  <- 16 / 52
n_cycles            <- ceiling(time_horizon_years / cycle_length_years)   # 16.25 -> 17 (NOT round)
stopifnot(n_cycles == 17)
discount_rate_costs <- 0.03
discount_rate_qalys <- 0.03
treatment_names     <- c("BSC", "Dravokimab", "Barisetinib", "Tremacept", "Uvestinib", "Dorilumab")
injectables         <- c("Tremacept", "Dorilumab")           # SC injection training cost (Sec 2.4)
state_names         <- c("NonResponder", "Resp_Low", "Resp_Moderate", "Resp_High", "Death")

# Sec 2.1 "mid-cycle convention" -> implemented as half-cycle correction on the trace
# (0.5 weight on first and last cycle rows). Set FALSE to omit.            [ASSUMPTION]
apply_hcc <- TRUE

# Maintenance-phase discontinuation: year 1 = cycles 2-4, year 2+ = cycles 5-17 (Table 3.2)
disc_year1_last_cycle <- 4

# --- Silent conventions (NOT in source) -----------------------------------------
# Constant background mortality is stated (Sec 2.4) but no value is given. The shared NCD
# life table is used, evaluated at one fixed age.                              [ASSUMPTION]
ciss_mortality_age <- 45
# Are background deaths applied in the induction cycle (cycle 1)? FALSE = all patients
# alive at the week-16 assessment; deaths start from cycle 2.                  [ASSUMPTION]
deaths_in_induction_cycle <- FALSE

wtp_threshold <- 100000                                                       # [ASSUMPTION]
n_sims     <- 2500
use_seed   <- TRUE
seed_value <- 12345
run_psa    <- TRUE

# ==============================================================================
# ------------------------------------------------------------------------------
# GENERAL-POPULATION MORTALITY (shared life table)
# Source: NCD report, Appendix B (CDC WONDER 2020, all-cause crude rate per
# 100,000, both sexes, single year of age 45-100; 85+ uses the pooled 85+ rate).
# [ASSUMPTION - user instruction] The NCD life table is used for ALL five models.
# ------------------------------------------------------------------------------
lt_age  <- 45:100
lt_rate_per_100k <- c(322.0, 350.9, 380.8, 412.6, 441.0, 467.7, 514.6, 562.3, 608.6, 666.4, 713.1, 778.4, 844.8, 911.2, 991.7, 1059.1, 1161.1, 1231.3, 1334.7, 1419.9, 1488.9, 1610.2, 1717.0, 1845.8, 1959.6, 2107.7, 2310.7, 2522.3, 2648.1, 3127.6, 3254.0, 3665.9, 3973.3, 4459.5, 4879.5, 5348.0, 5951.0, 6567.7, 7362.9, 8186.1, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9, 15210.9)
stopifnot(length(lt_age) == length(lt_rate_per_100k))

# Annual all-cause mortality RATE (per person-year) for a given age (vectorised).
# Ages above 100 use the pooled 85+ rate (= age-100 value).
# Ages below 45 (not in the table):  lt_below_45 = "hold" uses the age-45 rate;
#   "loglinear" extrapolates log-rate from ages 45-46 (only relevant to CNRD, start age 42).
lt_below_45 <- "hold"   # [ASSUMPTION] option: "hold" | "loglinear"
bg_rate <- function(age) {
  a <- floor(age)
  r <- lt_rate_per_100k[pmin(pmax(a, 45), 100) - 44] / 1e5
  if (lt_below_45 == "loglinear") {
    slope <- log(lt_rate_per_100k[2] / lt_rate_per_100k[1])
    below <- a < 45
    r[below] <- lt_rate_per_100k[1] / 1e5 * exp(slope * (a[below] - 45))
  }
  r
}
# ==============================================================================
# Constant per-cycle background death probability
p_death_cycle <- 1 - exp(-bg_rate(ciss_mortality_age) * cycle_length_years)

# ==============================================================================
# PARAMETER TABLE
# ==============================================================================
# Table 3.1 induction response probabilities (PSA: Beta, SE = 10% of mean)
resp <- data.frame(
  trt  = treatment_names,
  low  = c(0.105, 0.155, 0.150, 0.145, 0.130, 0.155),
  mod  = c(0.070, 0.175, 0.135, 0.140, 0.170, 0.170),
  high = c(0.060, 0.380, 0.170, 0.180, 0.475, 0.295))
# P(NonResponder) = 1 - sum, matches Table 3.1 (0.765, 0.290, 0.545, 0.535, 0.225, 0.380)
stopifnot(all(abs((1 - resp$low - resp$mod - resp$high) -
                  c(0.765, 0.290, 0.545, 0.535, 0.225, 0.380)) < 1e-9))

# Table 3.2 discontinuation per 16-week cycle (PSA: Beta, SE = 10% of mean)
disc <- data.frame(
  trt = treatment_names,
  y1  = c(0.240, 0.040, 0.075, 0.055, 0.075, 0.040),
  y2  = c(0.240, 0.040, 0.075, 0.055, 0.075, 0.050))

# Table 3.4a annual drug costs (PSA: Gamma, SE = 20% of mean; BSC fixed at $0)
drug_annual <- c(BSC = 0, Dravokimab = 38500, Barisetinib = 18200,
                 Tremacept = 29500, Uvestinib = 58700, Dorilumab = 29500)

df_params_input <- rbind(
  data.frame(Parameter = paste0("p_rlow_",  resp$trt), Mean = resp$low,  SE = 0.10 * resp$low,  Distribution = "Beta"),
  data.frame(Parameter = paste0("p_rmod_",  resp$trt), Mean = resp$mod,  SE = 0.10 * resp$mod,  Distribution = "Beta"),
  data.frame(Parameter = paste0("p_rhigh_", resp$trt), Mean = resp$high, SE = 0.10 * resp$high, Distribution = "Beta"),
  data.frame(Parameter = paste0("p_disc1_", disc$trt), Mean = disc$y1,   SE = 0.10 * disc$y1,   Distribution = "Beta"),
  data.frame(Parameter = paste0("p_disc2_", disc$trt), Mean = disc$y2,   SE = 0.10 * disc$y2,   Distribution = "Beta"),
  # Table 3.3 utilities (PSA: Utility, SE = 0.02)
  data.frame(Parameter = c("util_NonResponder", "util_Resp_Low", "util_Resp_Moderate", "util_Resp_High"),
             Mean = c(0.62, 0.78, 0.84, 0.87), SE = 0.02, Distribution = "Utility"),
  # Table 3.4b ANNUAL state costs (PSA: Gamma, SE = 20% of mean)
  data.frame(Parameter = c("cost_annual_NonResponder", "cost_annual_Resp_Low",
                           "cost_annual_Resp_Moderate", "cost_annual_Resp_High"),
             Mean = c(17200, 9500, 8400, 8100), SE = 0.20 * c(17200, 9500, 8400, 8100), Distribution = "Gamma"),
  data.frame(Parameter = paste0("cost_drug_annual_", names(drug_annual)),
             Mean = as.numeric(drug_annual), SE = 0.20 * as.numeric(drug_annual),
             Distribution = ifelse(drug_annual == 0, "Fixed", "Gamma")),
  # One-time SC injection training (cycle 1, injectables only). No PSA distribution given.
  data.frame(Parameter = "cost_sc_training", Mean = 25, SE = 0, Distribution = "Fixed"),
  stringsAsFactors = FALSE)
rownames(df_params_input) <- NULL

# ==============================================================================
# NOTES AND ASSUMPTIONS
# ==============================================================================
# - Costs are entered as ANNUAL values and converted to per-cycle inside the model by
#   x (16/52) (Sec 3.4); the $25 training cost is a one-off and is NOT annualised.
# - Induction cycle (cycle 1): all alive patients receive the drug and are valued on the
#   post-assessment response state; utility = mean of NonResponder baseline and the
#   post-assessment state value (Sec 3.3). From cycle 2 drug costs accrue to responders only.
# - The three response probabilities are sampled as independent Betas (as specified); if a
#   draw makes them sum to >1 they are rescaled to sum to 1 (non-responders = remainder).
# - Table 3.1 gives no trial N, so a Dirichlet is not used.
# - No validation targets are present in this ("no results") version of the report.
cat("✓ CISS model parameters loaded\n")
