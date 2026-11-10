################################################################################
# MODEL PARAMETERS - CNRD (Velonix vs Doralene first-line; 3-line cascade, 18 states)
# Source: "Cost-Effectiveness Analysis of Treatments for Chronic Neurocognitive and Relapsing
#         Disorder (CNRD)" (Velonix Therapeutics, March 2026) - revised reference version.
# Convention: [ASSUMPTION] = not stated in the source; exposed as a named setting.
################################################################################

# ==============================================================================
# CONFIGURATION (Sections 2.1, 2.3, 2.4)
# ==============================================================================
start_age           <- 42
time_horizon_years  <- 80
cycle_length_years  <- 0.25                                             # quarterly
n_cycles            <- ceiling(time_horizon_years / cycle_length_years) # 320 (NOT round)
stopifnot(n_cycles == 320)
discount_rate_costs <- 0.03
discount_rate_qalys <- 0.03
apply_hcc           <- TRUE     # Sec 2.1 "mid-cycle convention" -> 0.5 weight on first/last rows [ASSUMPTION]
disc_last_cycle     <- 80       # line discontinuation only in cycles 1-80 (first 20 years)
treatment_names     <- c("Velonix", "Doralene")
health_states       <- c("Stable", "MetabX", "DiabX", "CardioY", "DiabX_CardioY")
line_names          <- c("L1", "L2", "L3")

# --- Cycle 1 = acute stabilisation phase (Sec 2.4, revised) ---------------------
# Every patient incurs the acute hospitalisation cost and is allocated to Line 1, 2 or 3 according to
# response; NO mortality, life years, QALYs (and no other costs) accrue in this quarter; the Markov
# process starts from cycle 2. Allocation rule (not spelled out in the source):
#   P(L1) = r1 ; P(L2) = (1 - r1) x r2 ; P(L3) = (1 - r1)(1 - r2)               [ASSUMPTION]
# with r1 = Line-1 response rate of the arm and r2 = Line-2 response rate (Table 3.1).
# All patients are allocated in the Stable health state.

# --- After cycle 80 (Sec 2.3, revised) --------------------------------------------
# Discontinuation ceases and relapse probabilities are blended with the NO-TREATMENT relapse
# probability: weights 0.818 (line relapse probability) / 0.182 (no-treatment). Drug costs continue.
# The no-treatment relapse probability is NOT given in the report.        [MISSING - placeholder]
relapse_blend_after_80 <- TRUE
blend_weight_line      <- 0.818
blend_weight_notx      <- 0.182
p_relapse_notx         <- 0.12   # placeholder = highest relapse probability reported (Doralene). Supply the source value.

# --- Silent conventions (NOT in source) -----------------------------------------
# (1) SMR combination (Sec 2.4, revised): "SMRs are additive to the background mortality (full SMR x
#     background mortality)" -> mortality probability = p_bg x (SMR_CNRD + SMR_diab + SMR_CV for the
#     states that apply), capped at 1.  "additive_full" | "multiplicative" | "additive_excess"
#     The model file reports all three as a sensitivity.                            [ASSUMPTION]
smr_method <- "additive_full"
# (2) Line 2 / Line 3 drug cost: components 2a, 2b and the clozapine analogue have no prices in the
#     report. Placeholder = Doralene's $38/yr for both lines.                       [ASSUMPTION - MISSING]
cost_drug_L2_annual <- 38
cost_drug_L3_annual <- 38
# (3) Relapse disutility applied as -0.43 x P(relapse in cycle); DiabX+CardioY disutility is additive
#     (-0.095 + -0.095).                                                            [ASSUMPTION]
# (4) Half-cycle weights act on the first and last rows of the trace (row 1 accrues nothing, so only
#     the last row is effectively halved). Set TRUE to also halve the first accruing (Markov) row.  [ASSUMPTION]
hcc_half_first_markov_row <- FALSE
# (5) Acute hospitalisation ($30,000 per line entry) is charged once to every patient in cycle 1 (each
#     patient enters exactly one line) and at each later switch into Line 2 or Line 3.   [ASSUMPTION]

wtp_threshold <- 100000                                                              # [ASSUMPTION]
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
# The report specifies a sex-weighted US 2023 abridged life table (42% male) which is not reproduced in
# the report. The shared NCD both-sexes table is used as a stand-in (user instruction); swap
# lt_rate_per_100k to change it. Ages 42-44 (below the table) use the age-45 rate (lt_below_45 = "hold").
# Annual rate -> quarterly death probability: 1 - exp(-rate x 0.25); SMRs then scale this PROBABILITY.

# ==============================================================================
# PARAMETER TABLE
# ==============================================================================
# [ASSUMPTION] The source says "Beta (SE from 95% CI)" / "Gamma" but gives no CIs/SEs.
# Placeholder SEs: Beta = 10% of mean; Gamma costs = 20% of mean; Gamma SMRs = 10% of mean;
# disutilities (Gamma) = 20% of mean; stable utility SE = 5% of mean.
mk <- function(par, mean, dist, rel_se = 0) data.frame(Parameter = par, Mean = mean, SE = rel_se * mean,
                                                        Distribution = dist, stringsAsFactors = FALSE)
# Blend weights for the mixed regimens (Sec 2.3 table): Line 2 = 48% 2a / 52% 2b;
# Line 3 = 33% 2a / 37% 2b / 30% clozapine analogue. Blends are recomputed per PSA draw.
w_line2 <- c(a2 = 0.48, b2 = 0.52)
w_line3 <- c(a2 = 0.33, b2 = 0.37, cloz = 0.30)

df_params_input <- rbind(
  # Response rates (Table 3.1): Line 1 by arm, Line 2
  mk(c("resp_L1_Velonix", "resp_L1_Doralene", "resp_L2"), c(0.50, 0.34, 0.44), "Beta", 0.10),
  # Relapse probability per cycle (Table 3.1): Line 1 by arm + component regimens
  mk(c("p_relapse_L1_Velonix", "p_relapse_L1_Doralene", "p_relapse_2a", "p_relapse_2b", "p_relapse_cloz"),
     c(0.10, 0.12, 0.12, 0.11, 0.09), "Beta", 0.10),
  # Discontinuation probability per cycle (cycles 1-80): L1 by arm, L2
  mk(c("p_disc_L1_Velonix", "p_disc_L1_Doralene", "p_disc_L2"), c(0.055, 0.050, 0.040), "Beta", 0.10),
  # MetabX probability per cycle (Table 3.2): Line 1 by arm + component regimens
  mk(c("p_metab_L1_Velonix", "p_metab_L1_Doralene", "p_metab_2a", "p_metab_2b", "p_metab_cloz"),
     c(0.005, 0.034, 0.050, 0.034, 0.105), "Beta", 0.10),
  mk(c("p_diabx", "p_cardioy"), c(0.011, 0.005), "Beta", 0.10),             # conditional on MetabX
  # Mortality SMRs (Section 3.3)
  mk(c("smr_cnrd", "smr_diab", "smr_cardio"), c(2.20, 2.00, 1.60), "Gamma", 0.10),
  # Utilities (Section 3.4): disutilities entered as positive magnitudes (applied as negatives)
  mk("util_stable", 0.710, "Utility", 0.05),
  mk(c("disutil_metab", "disutil_diab", "disutil_cardio", "disutil_relapse"),
     c(0.055, 0.095, 0.095, 0.430), "Gamma", 0.20),
  # Costs (Section 3.5, revised): annual drug, quarterly state/management, per-event
  mk("cost_drug_velonix_annual", 18000, "Gamma", 0.20),
  mk("cost_drug_doralene_annual", 38, "Fixed"),
  mk("cost_stable_q", 933, "Fixed"),                 # revised from 1,056
  mk("cost_relapse", 13290, "Fixed"),                # revised from 13,399
  mk("cost_hosp_line_entry", 30000, "Fixed"),
  mk(c("cost_diab_q", "cost_cardio_q"), c(2900, 3700), "Gamma", 0.20)
)
rownames(df_params_input) <- NULL

# ==============================================================================
# GAPS / NOTES
# ==============================================================================
# - Not given in the report: the life table itself (NCD table used), SEs/CIs, Line 2/3 component
#   drug costs (placeholder $38/yr), the no-treatment relapse probability (placeholder 0.12),
#   MetabX management cost (none applied).
# - Derived deterministic blends reproduce the report: L2 relapse 0.1148, L3 0.1073; L2 MetabX 0.0417,
#   L3 0.0606 (checked in the model file).
# - "Stable base cost" ($933/quarter) is applied to every alive state; DiabX and CardioY management
#   costs are ADDITIONAL to it (both added in DiabX_CardioY).
# - Within-line progression: Stable -> MetabX (line-specific); MetabX -> DiabX (p_diabx),
#   -> CardioY (p_cardioy), -> both (p_diabx x p_cardioy); DiabX -> both (p_cardioy);
#   CardioY -> both (p_diabx); complications are irreversible (Fig 2.1A).
# - Order within a Markov cycle: deaths first (state-specific SMR), then progression and line transfer.
# - No validation targets are present in this report.
cat("✓ CNRD model parameters loaded\n")
