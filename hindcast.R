
##  ----Hindcast MASE calculation ----

# sc0r1h <- hindcast(results_by_scenario$SC5$RUN8)
# plotspict.hindcast(sc0r1h)

dir.create("figs/hindcast", recursive = TRUE, showWarnings = FALSE)

for (sc in names(results_by_scenario)) {
  for (rn in names(results_by_scenario[[sc]])) {

    fit <- results_by_scenario[[sc]][[rn]]
    if (is.null(fit)) next

    hc <- tryCatch(
      hindcast(fit),
      error = function(e) NULL
    )

    # si no convergió → saltar
    if (is.null(hc)) next

    png(
      filename = paste0("figs/hindcast/hindcast_", sc, "_", rn, ".png"),
      width = 2000,
      height = 1400,
      res = 200
    )

    plotspict.hindcast(hc)

    dev.off()
  }
}

