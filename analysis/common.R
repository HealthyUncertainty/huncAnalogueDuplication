source("config.R")
root_o <- ORIG_DIR
root_d <- DUP_DIR
load_pkg <- function(pkg) {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(root_o, pkg, "R"), "\\.R$", full.names = TRUE)) {
    if (grepl("app_launcher|package\\.R$", f)) next
    sys.source(f, envir = env)
  }
  env
}
# Shared NCD life table (as used by the Duplicate for all five models; identical to Original NCD csv, ages 45-100)
txt <- readLines(file.path(root_d, "NCD/model_parameters.R"))
ltline <- grep("^lt_rate_per_100k <- c\\(", txt, value = TRUE)
lt <- eval(parse(text = sub("^lt_rate_per_100k <- ", "", ltline)))
ncd_rate <- function(age) lt[pmin(pmax(floor(age), 45), 100) - 44] / 1e5
