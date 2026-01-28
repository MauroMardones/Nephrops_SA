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
dir.create("outputs/HCR", recursive = TRUE, showWarnings = FALSE)

scenarios <- paste0("SC", 5:9)
runs <- paste0("RUN", 5:8)

for (sc in scenarios) {
  for (rn in runs) {

    message("Running HCR for ", sc, " / ", rn)

    tryCatch({

      # 1. Base fitted model
      base_fit <- results_by_scenario[[sc]][[rn]]

      # 2. Add management scenarios
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

      # 3. Summarise management results
      res <- sumspict.manage(
        fit,
        include.unc = TRUE,
        include.abs = TRUE
      )

      # 4. Output folder per scenario
      out_dir <- file.path("results", "HCR", sc)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      # 5. Write CSV per SC × RUN
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


###----- Plots HCR ------------------
## problemas con este loop para hacer los plots de HCR!!!
dir.create("figs/hcr", recursive = TRUE, showWarnings = FALSE)

for (sc in names(results_by_scenario)) {
  for (rn in names(results_by_scenario[[sc]])) {

    message("Plotting HCR for ", sc, " / ", rn)

    tryCatch({

      # 1. Base fitted model
      fit <- results_by_scenario[[sc]][[rn]]

      # 2. Add management scenarios
      fit <- add.man.scenario(fit, "F=0", ffac = 0)
      fit <- add.man.scenario(fit, "F=Fsq", ffac = 1)
      fit <- add.man.scenario(fit, "F=Fmsy")
      fit <- add.man.scenario(
        fit,
        "F=Fmsy_C_fractile_35",
        fractiles = list(catch = 0.35),
        breakpointB = 0.5
      )

      # 3. Run management (CRITICAL STEP)
      fit <- sumspict.manage(
        fit,
        include.unc = TRUE,
        include.abs = TRUE
      )

      # 4. Output folder
      out_dir <- file.path("figs", "hcr", sc)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      # 5. Save plot
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
