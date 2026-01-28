## ---------------------- Manage tables and figs-------------------------------------
# Aplicar manejo (manage)
#
# Añadir el HCR ICES (2025) para especies vulnerables (fractil 0.15)
#
# Ejecutarlo para todos los escenarios y todos los runs
#
# Guardar los resultados de manejo por escenario/run
#
# Mantener una estructura ordenada y reutilizable

# get.TAC‘: gives the catch predicted management scenario
# ‘man.tac‘: gives the catch prediction of all defined

# example fx()
# fit <- manage(results_by_scenario$SC4$RUN4)
# sumspict.manage(fit)
# # example plot
# plotspict.hcr(fit)

# ---- Apply HCRs and save results by scenario ----

#using ˝results_by_scenario˝ list from fits

# Create base folder
##---------------- HCR tables ------------------

out_base <- file.path("outputs", "HCR")
dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

for (sc in names(results_by_scenario)) {

  if (length(results_by_scenario[[sc]]) == 0) next

  for (rn in names(results_by_scenario[[sc]])) {

    message("Running HCR for ", sc, " / ", rn)

    tryCatch({

      base_fit <- results_by_scenario[[sc]][[rn]]
      if (is.null(base_fit)) next

      fit <- base_fit
      fit <- add.man.scenario(fit, "F=0", ffac = 0)
      fit <- add.man.scenario(fit, "F=Fsq", ffac = 1)
      fit <- add.man.scenario(fit, "F=Fmsy")
      fit <- add.man.scenario(
        fit,
        "F=Fmsy_C_fractile_35",
        fractiles = list(catch = 0.35),
        breakpointB = 0.5
      )

      res <- sumspict.manage(
        fit,
        include.unc = TRUE,
        include.abs = TRUE
      )

      out_dir <- file.path(out_base, sc)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      write.csv(
        res,
        file = file.path(out_dir, paste0("HCR_", sc, "_", rn, ".csv")),
        row.names = TRUE
      )

    }, error = function(e) {
      message("❌ Skipping ", sc, " / ", rn, " → ", e$message)
    })
  }
}


##---------------- HCR plots ------------------

fig_base <- file.path("figs", "hcr")
dir.create(fig_base, recursive = TRUE, showWarnings = FALSE)

for (sc in names(results_by_scenario)) {

  if (length(results_by_scenario[[sc]]) == 0) next

  for (rn in names(results_by_scenario[[sc]])) {

    message("Plotting HCR for ", sc, " / ", rn)

    tryCatch({

      fit <- results_by_scenario[[sc]][[rn]]
      if (is.null(fit)) next

      fit <- add.man.scenario(fit, "F=0", ffac = 0)
      fit <- add.man.scenario(fit, "F=Fsq", ffac = 1)
      fit <- add.man.scenario(fit, "F=Fmsy")
      fit <- add.man.scenario(
        fit,
        "F=Fmsy_C_fractile_35",
        fractiles = list(catch = 0.35),
        breakpointB = 0.5
      )

      out_dir <- file.path(fig_base, sc)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      png(
        filename = file.path(out_dir, paste0("HCR_", sc, "_", rn, ".png")),
        width = 2400,
        height = 2000,
        res = 300
      )

      plotspict.hcr(fit)
      dev.off()

    }, error = function(e) {
      message("❌ Skipping HCR plot ", sc, " / ", rn, ": ", e$message)
    })
  }
}
