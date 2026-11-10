# Run the Duplicate scripts (deterministic only) and save arm-level results and cost parts.
source("config.R")
root <- DUP_DIR
files <- c(CAIS="model_cais.R", CISS="model_ciss.R", CNRD="model_cnrd.R", DMSS="model_dmss.R", NCD="model_ncd.R")
Sys.setenv(RUN_PSA = "FALSE")
res <- list(); envs <- list()
for (m in names(files)) {
  owd <- setwd(file.path(root, m))
  env <- new.env(parent = globalenv())
  invisible(capture.output(sys.source(files[[m]], envir = env)))
  setwd(owd)
  det <- env$det
  res[[m]] <- list(det = env$det_res,
                   parts = sapply(det, `[[`, "cost_parts"))
  envs[[m]] <- env
}
options(width = 200); print(res)
saveRDS(res, file.path(OUT_DIR, "dup_out.rds"))
