# State-level LYs and QALYs must sum to the reported totals.
OUT_DIR <- "output"
st <- readRDS(file.path(OUT_DIR, "states.rds"))
agg <- aggregate(value ~ basis+model+arm+version+measure, st, sum)
chk <- function(basis, f) {
  t <- read.csv(f); out <- NULL
  for (v in c("O","D")) for (me in c("LYd","Q")) {
    a <- agg[agg$basis==basis & agg$version==ifelse(v=="O","Original","Duplicate") & agg$measure==ifelse(me=="LYd","LY","QALY"),]
    m <- merge(a, data.frame(model=t$model, arm=t$arm, ref=t[[paste0(me,"_",v)]]), by=c("model","arm"))
    out <- rbind(out, data.frame(m[,c("model","arm","version","measure","value","ref")], diff=m$value-m$ref, basis=basis))
  }
  out
}
r <- rbind(chk("ncd", file.path(OUT_DIR, "tot_n.csv")), chk("own", file.path(OUT_DIR, "tot.csv")))
cat("rows checked:", nrow(r), " max abs diff:", max(abs(r$diff)), "\n")
print(r[abs(r$diff) > 1e-7, ])
