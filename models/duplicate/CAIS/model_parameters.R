################################################################################
# MODEL PARAMETERS - CAIS (Chronic Airway Infection Syndrome; Infectaguard vs Usual Care)
# Source: "Cost-Effectiveness Analysis of Treatments for CAIS" (Infectaguard Pharma, March 2026; revised
#         reference version with Sec 2.4 calculation conventions),
#         Tables 3.1-3.3, Section 3.4, Sections 1-2.
# Convention: [ASSUMPTION] = not stated in the source; exposed as a named setting.
################################################################################

# ==============================================================================
# CONFIGURATION (Section 2.1)
# ==============================================================================
start_age           <- 60
time_horizon_years  <- 100
cycle_length_years  <- 1
n_cycles            <- ceiling(time_horizon_years / cycle_length_years)    # 100
discount_rate_costs <- 0.03
discount_rate_qalys <- 0.03
apply_hcc           <- TRUE      # Sec 2.1: 0.5 weight on first and final cycle rows
treatment_names     <- c("UsualCare", "Infectaguard")
state_names         <- c("Stable", "Stable_Exac", "PathA", "PathA_Exac", "PathB", "PathB_Exac", "Dead")

# --- Silent conventions (NOT in source) -----------------------------------------
# (1) SMRs (Sec 2.3, 3.4 - revised report): condition-specific SMRs are MULTIPLIED onto the
#     CAIS-adjusted background probability of death, e.g. exacerbation + PathA = 1.50 x 1.20 x 1.30
#     x background probability (probability capped at 1). Implemented in smr_multiplier().
# (2) Exacerbation risk elevation in chronic infection: "25-30%" (Sec 1). Assigned PathA = 25%,
#     PathB = 30% (low->high in the order the states are listed).                        [ASSUMPTION]
rr_exac_pathA_mult <- 1.25
rr_exac_pathB_mult <- 1.30
# (3) Infectaguard is an add-on: the Infectaguard arm incurs the $4,500 usual-care background cost
#     PLUS the $2,000 drug cost.                                                         [ASSUMPTION]
drug_is_addon <- TRUE

wtp_threshold <- 100000                                                                 # [ASSUMPTION]
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
# PARAMETER TABLE (Tables 3.1-3.3, Sec 3.4)
# ==============================================================================
# Transition parameters are ANNUAL PROBABILITIES (PSA: Beta). Infectaguard values are derived
# as UC x RR (Table 3.1 note), so their uncertainty enters through the RR (Log-normal).
# Utility multipliers: Beta, SE 0.03 (revised report, Sec 3.2); state utilities are derived from them.
# SMRs: Log-normal (revised report, Sec 3.4); no SE is given.
# [ASSUMPTION] base Stable utility SE = 5% of mean; SMR log-scale SE = 0.10.
df_params_input <- data.frame(
  Parameter = c(
    "p_exac_uc", "p_pathA_uc", "p_pathB_uc", "p_recover", "p_clear_A", "p_clear_B",
    "rr_exac", "rr_pathA", "rr_pathB",
    "util_stable", "util_mult_exac", "util_mult_pathA", "util_mult_pathB",
    "cost_drug_inf", "cost_uc_background", "cost_exac_event", "cost_pathA_extra", "cost_pathB_extra",
    "smr_base", "smr_exac", "smr_pathA", "smr_pathB"),
  Mean = c(
    0.35, 0.04, 0.03, 0.85, 0.05, 0.04,
    0.55, 0.55, 0.58,
    0.72, 0.82, 0.88, 0.86,
    2000, 4500, 3200, 6000, 7500,
    1.50, 1.20, 1.30, 1.35),
  SE = c(
    0.070, 0.008, 0.006, 0.043, 0.013, 0.010,
    0.12, 0.15, 0.15,
    0.05 * 0.72, 0.03, 0.03, 0.03,
    400, 900, 800, 1200, 1500,
    rep(0.10, 4)),
  Distribution = c(
    rep("Beta", 6),
    rep("LogNormal", 3),
    "Utility", rep("Beta", 3),
    rep("Gamma", 5),
    rep("LogNormal", 4)),
  stringsAsFactors = FALSE
)

# ==============================================================================
# NOTES AND ASSUMPTIONS
# ==============================================================================
# - Exacerbation states resolve with probability 0.85 per cycle (the remaining 15% stay in the
#   exacerbation state); chronic infection is acquired from Stable only (Fig 2.1); PathA/PathB are
#   mutually exclusive within a cycle (acquisition probabilities are not allowed to overlap).
# - Spontaneous clearance (PathA 5%, PathB 4%) returns patients to Stable; clearance and exacerbation
#   are treated as independent events within the cycle (clearing + exacerbating -> Stable_Exac).
# - Per-exacerbation cost ($3,200) is applied to the occupancy of EACH exacerbation state in every
#   cycle, so patients whose exacerbation continues a further cycle incur it again (Sec 3.3, revised).
# - Infection costs (annual) accrue to PathA / PathA_Exac and PathB / PathB_Exac occupancy
#   (the "quarterly" remark in Sec 3.3 conflicts with the annual table; the table is used).
# - Mortality: annual probability = min(1, p_bg(age) x SMR multiplier), p_bg = 1 - exp(-crude rate);
#   age at cycle t = 60 + t - 1. The report specifies a US 2023 abridged life table (not reproduced in
#   the report, sex-weighted 55% female); the shared NCD both-sexes table is used as a stand-in
#   (user instruction) - replace lt_rate_per_100k to swap tables.
# - Table 3.1 lists Infectaguard 0.193 / 0.022 / 0.017; the model derives UC x RR (0.1925 / 0.022 /
#   0.0174), which differs only by rounding.
# - No validation targets are present in this ("no results") version of the report.
cat("✓ CAIS model parameters loaded\n")
