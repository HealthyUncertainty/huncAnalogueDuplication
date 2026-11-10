# Write the CSV tables that are embedded in app/view_results.R and app/app.R.
OUT_DIR <- "output"
wf   <- readRDS(file.path(OUT_DIR, "waterfall.rds"))
dup  <- readRDS(file.path(OUT_DIR, "dup_out.rds"))

# Bridge (step-by-step attribution)
rows <- list()
for (m in names(wf)) { st <- wf[[m]]; for (i in seq_along(st)) for (a in colnames(st[[i]]))
  rows[[length(rows) + 1]] <- data.frame(model = m, step = i - 1, label = names(st)[i], arm = a, t(st[[i]][, a])) }
write.csv(do.call(rbind, rows), file.path(OUT_DIR, "wf.csv"), row.names = FALSE)

# Totals and cost parts, Original vs Duplicate. "" = Original with its own mortality; "_n" = common NCD life table.
export_basis <- function(orig_file, suffix) {
  orig <- readRDS(file.path(OUT_DIR, orig_file)); r2 <- list()
  for (m in names(orig)) for (a in names(orig[[m]])) {
    o <- orig[[m]][[a]]; d <- dup[[m]]$det; d <- d[d$Arm == a, ]; dp <- dup[[m]]$parts[, a]
    r2[[length(r2) + 1]] <- data.frame(model = m, arm = a, LYd_O = o$LY_disc, LYd_D = d$LY_discounted,
      LYu_O = o$LY_undisc, LYu_D = d$LY_undiscounted, Q_O = o$QALY, Q_D = d$QALY, C_O = o$Cost, C_D = d$Cost)
    for (n in union(names(o$parts), names(dp)))
      r2[[length(r2) + 1]] <- data.frame(model = m, arm = a, part = n,
        O = if (n %in% names(o$parts)) o$parts[[n]] else 0, D = if (n %in% names(dp)) dp[[n]] else 0)
  }
  tot   <- do.call(rbind, Filter(function(x) "LYd_O" %in% names(x), r2))
  parts <- do.call(rbind, Filter(function(x) "part" %in% names(x), r2))
  write.csv(tot,   file.path(OUT_DIR, paste0("tot",   suffix, ".csv")), row.names = FALSE)
  write.csv(parts, file.path(OUT_DIR, paste0("parts", suffix, ".csv")), row.names = FALSE)
}
export_basis("orig_detail.rds", "")
export_basis("orig_ncd_detail.rds", "_n")
