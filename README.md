# HTA replication check: Original vs Duplicate

A comparison of two R implementations of five fictional cost-effectiveness models, built to test how
closely a model replicated from a report matches its reference implementation.

## Layout

| Path | Contents |
| --- | --- |
| `models/duplicate/` | Duplicate models and the source report (.docx) for each. `run_models.R` runs any or all of them. |
| `models/original/` | Place the Original packages here (not included). |
| `app/` | `view_results.R` (tables and figures in RStudio) and `app.R` (Shiny app, one tab per model). Results are embedded; neither needs the other folders. |
| `analysis/` | Scripts that produced the embedded results. |
| `docs/` | Written summary. |

## Viewing the results

Open `app/view_results.R` in RStudio and source it, or run `shiny::runApp("app/app.R")`.
Needs `shiny`, `dplyr`, `tidyr`, `ggplot2` and `scales` (`DT` optional).

## Running the Duplicate models

```r
setwd("models/duplicate")
source("run_models.R")
res <- run_models("NCD", psa = FALSE)   # or run_models() for all five with PSA
```

## Reproducing the comparison

1. Put the five Original packages in `models/original/` (or set `HUNC_ORIGINAL_DIR`).
2. From `analysis/`: `source("run_all.R")`. Takes about 20 seconds. Outputs go to `analysis/output/`.

| Script | Does |
| --- | --- |
| `01_run_duplicate.R` | Duplicate results and cost components (deterministic) |
| `02_original_own_mortality.R` | Original as supplied |
| `03_original_common_mortality.R` | Original with the NCD life table (the comparison basis) |
| `04_bridge.R` | Applies the differences one at a time, from Original to Duplicate; the end point reproduces the Duplicate |
| `05_states.R` | Life years and QALYs by health state |
| `06_export.R` | Writes the CSVs embedded in `app/` |
| `07_check_states.R` | Checks that state results sum to the totals |

The Original packages are sourced from their `R/` folders; nothing needs installing. The regenerated CSVs match
those embedded in `app/` exactly. The step that embeds the CSVs into `app/` is not included.

## Scope

Deterministic base case only; PSA was not compared. The bridge's step sizes depend on the order of the steps, the totals do not.

## Licence

MIT, see `LICENSE`.
