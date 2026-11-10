# Replication Check: Original vs Duplicate HTA Models

2026-11-08

## Purpose and scope

The Duplicate models reproduce the Original reference models in structure but not in results: even on identical background mortality, four of the five models still produce different outputs, mostly in costs. The exercise compared two sets of R implementations of five fictional cost-effectiveness models: CAIS (Infectaguard vs usual care), CISS (best supportive care and five targeted therapies), CNRD (Velonix vs Doralene), DMSS (five arms, from Cardivex to Ablatherm) and NCD (Zelravine vs usual care). The Original is a set of R packages; the Duplicate is a single-folder script replication built from the same cost-effectiveness reports.

The Original is the reference, so every difference is treated as a deviation in the Duplicate. The aims were to identify the structural differences that change outputs, to quantify each one, and to show disaggregated outputs side by side: life years, QALYs and costs, with costs split by type and life years and QALYs split by health state.

## Approach

Both versions were run on the same background mortality so that life-table differences could not mask or inflate modelling differences. The Duplicate already uses one shared life table: CDC WONDER 2020 all-cause crude mortality rates per 100,000 (both sexes, single years of age 45 to 100, with 85 and over pooled), taken from the NCD report. The Original packages for CAIS, CISS, CNRD and DMSS were recalculated with their mortality functions replaced by that table, converted to per-cycle probabilities as the Duplicate does: p = 1 − exp(−rate × cycle length), with ages below 45 held at the age-45 rate. NCD already used this table in both versions. The Original code was otherwise left as supplied.

The remaining differences were found by reading the code and the reports side by side, and their effects were measured by attribution. Starting from the recalculated Original, the differences were applied one at a time, cumulatively, until the model reproduced the Duplicate. The endpoint matched the Duplicate to numerical precision (about 1e-9) for all four models, which confirms that no unidentified difference remains. Step sizes depend on the order in which differences are applied; the totals do not.

Cost components were isolated by zeroing the other cost parameters, since costs are linear in them, and the components sum to the totals. Life years and QALYs by state were taken from each model's state trace and reconcile to the reported totals to within 1e-13. All results are the deterministic base case; probabilistic sensitivity analysis was not compared.

## Headline findings

NCD matches exactly; the other four models do not, and the Duplicate's costs are lower in every arm of every one of them, by 0.3% to 7.7% of the Original total. Differences below are Duplicate minus Original, as ranges across arms.

| Model | Δ LY (discounted) | Δ QALY | Δ Cost | Main drivers |
| --- | --- | --- | --- | --- |
| CAIS | +0.03 to +0.07 | +0.05 to +0.10 | −$3,732 to −$1,630 | Transition structure: no infection from the exacerbation state, no clearance from infected exacerbation states |
| CISS | −0.26 | −0.18 to −0.16 | −$13,247 to −$4,146 | Half-cycle correction added in the Duplicate |
| CNRD | −0.01 | +0.07 to +0.08 | −$24,363 to −$11,268 | Relapse probability 0.40 → 0.12; Line 2/3 drug cost placeholder; hospitalisation costing |
| DMSS | +0.03 | −0.01 to +0.03 | −$10,493 to −$4,996 | Background medication costs omitted; grade split before freeze cycle; 4-week mortality lag |
| NCD | 0.00 | 0.00 | $0 | None: outputs identical |

Three themes explain almost all of the differences. First, the Duplicate fills gaps in the source reports with placeholders or assumptions that the Original resolved differently: a no-treatment relapse probability, a flat second- and third-line drug cost, omitted background medication costs and a half-cycle correction. Second, a few structural reinterpretations change which transitions are possible, notably in CAIS, and how CNRD counts hospital admissions. Third, timing conventions differ: start-of-cycle rather than mid-cycle discounting, a mortality age lag in DMSS and an added stabilisation cycle in CNRD.

Life-year differences are under 1% except in CISS (about −5.4%). QALY differences are within about 1.2% except in CISS (about −5%).

## Model-by-model findings

**CAIS.** One difference matters: the transition structure. The Original lets infection be acquired from the stable exacerbation state and lets patients in the infected exacerbation states clear back to stable states; the Duplicate allows neither, and caps death probabilities at 1 rather than 0.999. This accounts for essentially all of the gap: +0.066 and +0.031 life years, +0.101 and +0.047 QALYs, and costs lower by $3,720 and $1,625 for usual care and Infectaguard. The cost reduction sits almost entirely in chronic infection costs (−$3,981 and −$1,813), consistent with fewer patient-years in infected states. A second difference, swapped exacerbation risk multipliers for the two infection pathways (1.30 and 1.25 in the Original, 1.25 and 1.30 in the Duplicate), is negligible.

**CISS.** One difference explains the result: the Duplicate applies a half-cycle correction, weighting the first and last cycles at 0.5, which the Original does not. With only 17 cycles of 16 weeks (about 5.2 years), this removes about 0.31 years of follow-up and cuts discounted life years by 0.285 (from 4.805 to 4.546 in every arm, since survival is the same across arms). QALYs fall by 0.16 to 0.18 and costs by $4,592 to $14,334. Drug arms lose proportionally more cost because cycle 1 is the one cycle in which every alive patient is on drug, including non-responders. Two smaller differences offset a tenth of the effect: the Duplicate applies no background deaths in the induction cycle (+0.005 life years) and discounts from the start of each cycle rather than the middle (+0.022).

**CNRD.** Ten differences were identified, but three drive the cost gap. The no-treatment relapse probability used after cycle 80 is 0.40 in the Original and 0.12 in the Duplicate (a placeholder), lowering costs by about $9,500 per arm and raising QALYs by about 0.077. Second- and third-line drug costs are $100.64 and $445.94 per year in the Original and a flat $38 in the Duplicate, lowering costs by $5,060 (Velonix) and $5,356 (Doralene). Hospitalisation is charged differently: the Original applies $30,000 times expected admissions at cycle 1 only, while the Duplicate charges one admission per patient plus one at each later switch to line 2 or 3, which moves cost by +$4,206 for Velonix and −$9,076 for Doralene.

Timing differences largely cancel in life years. An added stabilisation cycle with ages shifted by a quarter year removes 0.072 life years, while start-of-cycle discounting adds 0.063. Discontinuation ordering, the cutoff cycle, the mortality cap, the MetabX transition split and a last-row weight each move life years by under 0.01. Net effects are about zero life years, +0.083 and +0.072 QALYs, and costs lower by $11,268 (Velonix) and $24,363 (Doralene).

**DMSS.** Six differences were identified. The largest for cost is that the Duplicate omits background beta-blocker and calcium-channel-blocker costs, because the report gives no rates, removing $9,915 to $11,585 per arm. The Duplicate also lags mortality by one cycle: the transition into cycle i uses the age at the start of cycle i−1 (four weeks earlier), adding 0.027 to 0.028 life years. Before the freeze cycle, the Original applies the frozen grade split from cycle 2 for all arms, although its own code comment says Cardivex and SoC should be frozen at cycle 8; the Duplicate implements cycle 8, changing QALYs by −0.035 to +0.008 and costs by −$1,006 to +$4,328. Three minor differences (Zonapride cycle-1 drug cost, the Septoplasty disutility window, and rounding of the surgical-arm grade split) change costs by under $100 and QALYs by under 0.001. Net costs are lower by $4,996 to $10,493, but only 0.3% for Cardivex because its drug cost exceeds $1.1 million.

**NCD.** Outputs are identical in both versions to numerical precision: usual care 20.346 life years, 13.959 QALYs and $141,524; zelravine 20.450 life years, 14.756 QALYs and $196,234. This confirms that the shared life table and comparison method introduce no differences of their own.

## Effect on ICERs

The deviations move ICERs most in CNRD (+58%) and CAIS (+30%), and by 4% to 12% in the CISS comparisons. In CNRD, Doralene's cost falls much more than Velonix's, so the incremental cost rises from $19,278 to $32,374. In CISS, the front-loaded drug cost means incremental costs shrink faster than incremental QALYs, so every ICER falls.

| Model | Comparison | ICER, Original ($/QALY) | ICER, Duplicate ($/QALY) | Change |
| --- | --- | --- | --- | --- |
| CAIS | Infectaguard vs usual care | 26,447 | 34,396 | +30% |
| CISS | Dravokimab vs BSC | 165,852 | 158,415 | −4% |
| CISS | Barisetinib vs BSC | 86,642 | 76,421 | −12% |
| CISS | Tremacept vs BSC | 152,647 | 138,972 | −9% |
| CISS | Uvestinib vs BSC | 274,246 | 261,457 | −5% |
| CISS | Dorilumab vs BSC | 127,208 | 119,587 | −6% |
| CNRD | Velonix vs Doralene | 111,607 | 175,821 | +58% |
| DMSS | Cardivex vs SoC | 1,057,549 | 1,079,008 | +2% |
| DMSS | Zonapride vs SoC | 292,534 | 262,453 | −10% |
| DMSS | Septoplasty vs SoC | dominant | dominant | n/a |
| DMSS | Ablatherm vs SoC | dominant | dominant | n/a |
| NCD | Zelravine vs usual care | 68,644 | 68,644 | 0% |

At a $100,000 per QALY threshold no comparison changes side, but at $150,000 Tremacept and CNRD would. The threshold is illustrative; the reports do not fix one.

## Half-cycle correction in the Original documentation

The Original CISS documentation does not describe a half-cycle correction; it says only that costs and outcomes are "discounted at 3% per annum using mid-cycle convention". That wording appears in the report's model structure section, the README and the validation results. The Original code implements it as mid-cycle discounting alone: each cycle is discounted at (cycle − 0.5) × cycle length and counts in full, with no weighting of the first or last cycle.

The Duplicate's parameter file shows where its correction came from. A code comment reads the report's "mid-cycle convention" as a half-cycle correction on the trace (0.5 weight on the first and last rows), flags this as an assumption, and provides a switch to turn it off. The documentation therefore does not support the correction, and the Duplicate's start-of-cycle discounting also departs from the stated mid-cycle convention. The Original packages are not consistent with one another: CAIS applies a half-cycle correction in code and NCD states that it uses none.

## Limitations

The findings rest on deterministic base-case runs, so differences in parameter uncertainty and PSA results were not examined. CISS and CNRD start below the life table's first age (38 and 42), so both depend on holding the age-45 rate for younger ages. Step-by-step contributions depend on the order of attribution, although totals do not. Many deviations reflect values the source reports do not state, so the Duplicate's placeholders may be defensible choices; the exercise follows the instruction to treat the Original as the reference and does not judge which choice is clinically better.
