################################################################################
# MODEL PARAMETERS - DMSS (Cardivex, SoC, Zonapride, Septoplasty, Ablatherm)
# Source: "Cost-Effectiveness Analysis of Treatments for Diffuse Mucosal Sclerosis Syndrome (DMSS)" (Cardivex, March 2026),
#         Tables in Sections 3.1-3.3, Sections 1-2.
# Convention: [ASSUMPTION] = not stated in the source; exposed as a named setting.
################################################################################

# ==============================================================================
# CONFIGURATION (Section 2.1)
# ==============================================================================
start_age           <- 55
time_horizon_years  <- 80
cycle_length_years  <- 4 / 52
# 80 / (4/52) is 1040 up to floating-point noise, so ceiling() is applied with a tolerance
n_cycles            <- ceiling(time_horizon_years / cycle_length_years - 1e-9)     # 1040
stopifnot(n_cycles == 1040)
discount_rate_costs <- 0.03
discount_rate_qalys <- 0.03      # Sec 2.1 states only that COSTS are discounted; QALYs/LYs at the
                                 # same 3% (ICER convention).                                [ASSUMPTION]
apply_hcc           <- FALSE     # Sec 2.1: no half-cycle correction
treatment_names     <- c("Cardivex", "SoC", "Zonapride", "Septoplasty", "Ablatherm")
grade_names         <- c("Grade_I", "Grade_II", "Grade_III")

# Frozen grade distributions (Table 3.1): Grade I / II / III, freeze cycle, periop mortality
frozen <- list(
  Cardivex    = c(0.480, 0.435, 0.085), SoC         = c(0.220, 0.560, 0.220),
  Zonapride   = c(0.300, 0.476, 0.224), Septoplasty = c(0.720, 0.190, 0.090),
  Ablatherm   = c(0.720, 0.190, 0.090))
freeze_cycle <- c(Cardivex = 8, SoC = 8, Zonapride = 1, Septoplasty = 1, Ablatherm = 1)
periop_mort  <- c(Cardivex = 0, SoC = 0, Zonapride = 0, Septoplasty = 0.011, Ablatherm = 0.009)
periop_cycle <- 2                                           # Sec 2.1: one-time perioperative mortality at cycle 2
stopifnot(all(sapply(frozen, sum) > 0.9999 & sapply(frozen, sum) < 1.0001))

# --- Silent conventions (NOT in source) -----------------------------------------
# (1) Distribution BEFORE the freeze cycle (cycles 1-7 for Cardivex/SoC): baseline presentation
#     (Sec 1: 68% Grade II / 32% Grade III / 0% Grade I) until the assessment.
#     "baseline" = step change at the freeze cycle | "linear" = linear ramp to the frozen split.  [ASSUMPTION]
baseline_dist    <- c(0, 0.68, 0.32)
pre_freeze_method <- "baseline"
# (2) Utility age decrement (0.0008/yr) is applied relative to the start age (55).               [ASSUMPTION]
age_decrement      <- 0.0008
age_decrement_ref  <- start_age
# (3) Surgical timing: procedure and its one-time cost at cycle 1; "post-procedure" disutility
#     starts the cycle after (cycle 2) for the stated number of cycles; lifetime pacemaker
#     disutility (-0.04 x probability) applies to all cycles from the procedure.                 [ASSUMPTION]
surgery_cycle         <- 1
disutil_post_first_cycle <- 2
disutil_post   <- c(Septoplasty = 0.08, Ablatherm = 0.04)       # magnitude (applied as negative)
disutil_post_n <- c(Septoplasty = 6,    Ablatherm = 1)           # cycles
pacemaker_disutil <- 0.04
pacemaker_prob    <- c(Septoplasty = 0.05, Ablatherm = 0.12)
# (4) Cardiac-scan schedule: Cardivex at cycles 1, 3, 5; all other arms at cycle 1 then
#     "biannually". Read here as every 2 years = every 26 cycles (13 cycles/year).               [ASSUMPTION]
scan_cycles_cardivex  <- c(1, 3, 5)
scan_interval_other   <- 26
# (5) Zonapride hospitalisation ($7,800 one-time) is charged to all patients at cycle 1.         [ASSUMPTION]
# (6) Background medication (beta-blocker + CCB) "accrues at treatment-specific utilisation
#     rates", but neither rates nor unit costs are given. NOT INCLUDED (0 per cycle). Enter
#     per-cycle values here when available.                                  [MISSING - set to 0]
cost_bgmed_cycle <- c(Cardivex = 0, SoC = 0, Zonapride = 0, Septoplasty = 0, Ablatherm = 0)

wtp_threshold <- 100000                                                                           # [ASSUMPTION]
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
# The source says "sex-blended life tables" with linear interpolation between age points; the
# shared NCD single-year both-sexes table is used instead (user instruction).

# ==============================================================================
# PARAMETER TABLE
# ==============================================================================
# [ASSUMPTION] No SEs are given. Placeholders: utilities SE = 0.03 ("Utility" dist);
# Gamma costs SE = 20% of mean. Frozen distributions have no PSA specification (treated as fixed).
df_params_input <- data.frame(
  Parameter = c(
    "util_I_cardivex", "util_II_cardivex", "util_III_cardivex",
    "util_I_soc", "util_II_soc", "util_III_soc",
    "util_I_other", "util_II_other", "util_III_other",
    "cost_state_I", "cost_state_II", "cost_state_III",
    "cost_cardivex_cycle", "cost_zonapride_cycle",
    "cost_septoplasty", "cost_ablatherm", "cost_zona_hosp", "cost_scan"),
  Mean = c(
    0.93, 0.85, 0.69,
    0.93, 0.83, 0.68,
    0.93, 0.84, 0.69,
    680, 1850, 2560,
    4923, 380,
    110000, 50000, 7800, 95),
  SE = c(
    rep(0.03, 9),
    0.20 * c(680, 1850, 2560, 4923, 380),
    0, 0, 0, 0),
  Distribution = c(
    rep("Utility", 9),
    rep("Gamma", 5),
    rep("Fixed", 4)),
  stringsAsFactors = FALSE)

# ==============================================================================
# NOTES
# ==============================================================================
# - Population-proportion structure (not a transition-matrix Markov model): the surviving cohort is
#   redistributed each cycle over Grade I/II/III using the frozen arm-specific split; survival is driven
#   only by age-dependent background mortality (+ perioperative mortality at cycle 2).
# - Per-cycle death probability = 1 - exp(-rate x 4/52), rate from the age at the start of the cycle.
# - Costs are entered per 4-week cycle (as given in Sec 3.3); one-time costs are not annualised.
# - No validation targets are present in this ("no results") version of the report.
cat("✓ DMSS model parameters loaded\n")
