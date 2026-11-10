################################################################################
# MODEL PARAMETERS - NCD (Zelravine vs Usual Care, Nocturnal Cortical Dysrhythmia)
# Source: "Cost-Effectiveness Analysis of Zelravine for NCD" (Marovex, Sept 2026),
#         Tables 3.1-3.4, Appendices A-B.
# Convention: [ASSUMPTION] = not stated in the source; exposed as a named setting.
################################################################################

# ==============================================================================
# CONFIGURATION (Section 2.1, 2.5)
# ==============================================================================
start_age           <- 45      # Table 3.4
terminal_age        <- 100     # Table 3.4 (55 cycles)
cycle_length_years  <- 1
n_cycles            <- terminal_age - start_age        # = 55 (ceiling not needed: exact)
discount_rate_costs <- 0.03
discount_rate_qalys <- 0.03
apply_hcc           <- FALSE   # Sec 2.1: no half-cycle correction; occupancy at START of cycle
treatment_names     <- c("Zelravine", "UsualCare")
arm_key             <- c(Zelravine = "zel", UsualCare = "uc")

# Re-treatment (Table 3.4): zelravine arm only, cycles 1-3, once per patient
retreat_last_cycle  <- 3

# Year-1 valuation (Sec 2.3): base case values the whole of year 1 on the arm's week-12
# distribution. Scenario (Sec 4.3, not in this report) values the first w weeks on the
# pre-treatment baseline distribution. 0 = base case.
year1_baseline_weeks <- 0

# Willingness-to-pay used for P(cost-effective) summaries [ASSUMPTION - not in source]
wtp_threshold <- 100000

# PSA settings
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

# ==============================================================================
# PARAMETER TABLE (Mean / SE / PSA distribution)
# ==============================================================================
# Severity distributions: Table 3.1 (proportions); Dirichlet counts below.
# Utilities: Table 3.2, SE = (upper - lower)/3.92
# State costs: Table 3.3b, SE = (upper - lower)/3.92
# Mortality RR: Table 3.4, log-scale SE = (ln 1.65 - ln 1.10)/3.92 = 0.1034
df_params_input <- data.frame(
  Parameter = c(
    "p_sev_zel_rem", "p_sev_zel_mild", "p_sev_zel_mod", "p_sev_zel_sev",
    "p_sev_uc_rem",  "p_sev_uc_mild",  "p_sev_uc_mod",  "p_sev_uc_sev",
    "p_sev_base_rem","p_sev_base_mild","p_sev_base_mod","p_sev_base_sev",
    "util_rem", "util_mild", "util_mod", "util_sev",
    "cost_state_rem", "cost_state_mild", "cost_state_mod", "cost_state_sev",
    "rr_mort_symptomatic",
    "p_retreat", "cost_course_initial", "cost_course_retreat"),
  Mean = c(
    0.24, 0.30, 0.28, 0.18,
    0.15, 0.28, 0.32, 0.25,
    0.00, 0.12, 0.46, 0.42,
    0.88, 0.79, 0.66, 0.48,
    3100, 5200, 7400, 10800,
    1.35,
    0.08, 60000, 38000),
  SE = c(
    rep(NA, 12),
    (0.93 - 0.82) / 3.92, (0.85 - 0.73) / 3.92, (0.72 - 0.60) / 3.92, (0.60 - 0.36) / 3.92,
    (4000 - 2200) / 3.92, (6800 - 3600) / 3.92, (9600 - 5200) / 3.92, (14100 - 7500) / 3.92,
    (log(1.65) - log(1.10)) / 3.92,
    NA, NA, NA),
  Distribution = c(
    rep("Dirichlet", 12),
    rep("Utility", 4),
    rep("Gamma", 4),
    "LogNormal",
    "Fixed", "Fixed", "Fixed"),
  stringsAsFactors = FALSE
)
df_params_input$SE[is.na(df_params_input$SE)] <- 0   # unused for Dirichlet / Fixed

# Dirichlet blocks: whole-person counts from Table 3.1 (each sums to its trial N)
dir_counts <- list(
  zel  = list(cols = c("p_sev_zel_rem","p_sev_zel_mild","p_sev_zel_mod","p_sev_zel_sev"),
              alpha = c(14, 17, 16, 11), N = 58),
  uc   = list(cols = c("p_sev_uc_rem","p_sev_uc_mild","p_sev_uc_mod","p_sev_uc_sev"),
              alpha = c(8, 15, 18, 14),  N = 55),
  base = list(cols = c("p_sev_base_rem","p_sev_base_mild","p_sev_base_mod","p_sev_base_sev"),
              alpha = c(0, 14, 52, 47),  N = 113)
)
# (The model file checks that the published proportions reproduce these counts under
#  largest-remainder rounding.)

# ==============================================================================
# NOTES AND ASSUMPTIONS
# ==============================================================================
# - Life table: NCD Appendix B used as given (also used for the other four models).
# - Mortality: annual probability = 1 - exp(-rate); rate multiplied by RR 1.35 in
#   Mild/Moderate/Severe (naive and R states); Remission RR = 1.00 (Table 3.4).
# - Re-treatment: 8% of SURVIVING naive-state patients per cycle in cycles 1-3; flow
#   enters the R states at the start of the next cycle in proportion to the zelravine
#   post-treatment distribution; cost applied and discounted at the entry cycle.
# - No validation targets are present in this ("no results") version of the report.
cat("✓ NCD model parameters loaded\n")
