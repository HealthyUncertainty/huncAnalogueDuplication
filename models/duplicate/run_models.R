################################################################################
# run_models.R - run any or all of the HTA reference models from their sub-folders
#
# Folders (each holds model_parameters.R, model_<name>.R and the source report .docx):
#   CAIS  CISS  CNRD  DMSS  NCD
#
# From RStudio (open HTA_Reference_Models.Rproj first so the working directory is this folder):
#   source("run_models.R")                              # defines run_models(); runs nothing
#   res <- run_models()                                 # all five: deterministic + PSA
#   res <- run_models(c("NCD", "CISS"), n_sims = 500)   # chosen models, fewer PSA runs
#   res <- run_models("CAIS", psa = FALSE)              # deterministic only
#   res$NCD$results$det                                 # deterministic table for one model
#
# From a terminal:
#   Rscript run_models.R                                # all five
#   Rscript run_models.R NCD CAIS --sims 500 --no-psa
#
# Each model reads its inputs from its own model_parameters.R (assumptions are tagged
# [ASSUMPTION] / [MISSING] there). run_models() only sets the working directory to the model
# folder, applies the PSA options, and collects the results.
################################################################################

available_models <- c(CAIS = "model_cais.R", CISS = "model_ciss.R", CNRD = "model_cnrd.R",
                      DMSS = "model_dmss.R", NCD  = "model_ncd.R")

.find_root <- function() {
  cands <- character()
  fa <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(fa)) cands <- c(cands, dirname(normalizePath(fa[1], mustWork = FALSE)))
  sf <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)   # when source()d
  if (!is.null(sf)) cands <- c(cands, dirname(sf))
  cands <- c(cands, getwd())
  for (d in cands) if (all(dir.exists(file.path(d, names(available_models))))) return(d)
  stop("Cannot find the model folders. Open HTA_Reference_Models.Rproj or setwd() to the top-level folder.")
}

run_models <- function(models = names(available_models), n_sims = NULL, psa = TRUE,
                       save_rds = FALSE, root = .find_root()) {
  models <- toupper(models)
  bad <- setdiff(models, names(available_models))
  if (length(bad)) stop("Unknown model(s): ", paste(bad, collapse = ", "),
                        ". Available: ", paste(names(available_models), collapse = ", "))

  old_wd  <- getwd()
  old_env <- Sys.getenv(c("N_SIMS", "RUN_PSA"), unset = NA)
  on.exit({
    setwd(old_wd)
    for (v in names(old_env)) if (is.na(old_env[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, setNames(list(old_env[[v]]), v))
  }, add = TRUE)
  Sys.setenv(RUN_PSA = as.character(psa))
  if (is.null(n_sims)) Sys.unsetenv("N_SIMS") else Sys.setenv(N_SIMS = as.character(as.integer(n_sims)))

  out <- list()
  for (m in models) {
    cat("\n", strrep("#", 78), "\n# ", m, "\n", strrep("#", 78), "\n", sep = "")
    setwd(file.path(root, m))
    env <- new.env(parent = globalenv())          # isolates each model's objects
    t0  <- Sys.time()
    sys.source(available_models[[m]], envir = env)
    out[[m]] <- list(results = env$results_list,
                     seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
    if (save_rds) {
      dir.create(file.path(root, "results"), showWarnings = FALSE)
      saveRDS(out[[m]]$results, file.path(root, "results", paste0(tolower(m), "_results.rds")))
    }
  }

  cols <- c("Arm", "LY_discounted", "LY_undiscounted", "Cost", "QALY")
  summary_tbl <- do.call(rbind, lapply(models, function(m)
    cbind(Model = m, out[[m]]$results$det[, cols])))
  cat("\n", strrep("=", 78), "\nDETERMINISTIC SUMMARY (all models run)\n", strrep("=", 78), "\n", sep = "")
  print(summary_tbl, row.names = FALSE, digits = 6)
  cat("\nRun time (s):", paste(names(out), sapply(out, `[[`, "seconds"), sep = " = ", collapse = ", "), "\n")
  invisible(c(out, list(summary = summary_tbl)))
}

# Command-line entry point (not triggered by source() or in an interactive session)
if (!interactive() && sys.nframe() == 0L) {
  a    <- commandArgs(trailingOnly = TRUE)
  psa  <- !("--no-psa" %in% a)
  sims <- NULL
  i <- match("--sims", a)
  if (!is.na(i)) { sims <- as.integer(a[i + 1]); a <- a[-c(i, i + 1)] }
  a <- a[a != "--no-psa"]
  invisible(run_models(if (length(a)) a else names(available_models), n_sims = sims, psa = psa))
}
