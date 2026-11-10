# Run the whole analysis in order. Working directory must be analysis/.
for (f in c("01_run_duplicate.R", "02_original_own_mortality.R", "03_original_common_mortality.R",
            "04_bridge.R", "05_states.R", "06_export.R", "07_check_states.R")) {
  cat("\n##### ", f, "\n"); source(f, local = new.env())
}
