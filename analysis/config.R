# Paths used by the analysis scripts. Run every script with analysis/ as the working directory.
# The Original packages are not in this repo: clone them into models/original/ (see README)
# or point HUNC_ORIGINAL_DIR at the folder that holds them.
REPO_DIR <- normalizePath("..")
ORIG_DIR <- Sys.getenv("HUNC_ORIGINAL_DIR", unset = file.path(REPO_DIR, "models", "original"))
DUP_DIR  <- file.path(REPO_DIR, "models", "duplicate")
OUT_DIR  <- "output"
dir.create(OUT_DIR, showWarnings = FALSE)
if (!dir.exists(file.path(ORIG_DIR, "huncMarkovNCD")))
  stop("Original packages not found in ", ORIG_DIR, ". See README.")
