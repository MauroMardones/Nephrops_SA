
## --------------Function to extract diagnostics from a spict fit object------------------------------------------

# Table and plots
results_by_scenario2<-readRDS("outputs/SPiCT_SC8_SC9_RUN5_results.rds")

## --------------Function to extract diagnostics from a spict fit object------------------------------------------

# Table and plots

run_osa_diagnostics <- function(spict_obj,
                                scenario,
                                run,
                                out_dir = "figs/diagnostics") {

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Calcular residuos OSA
  res_osa <- calc.osa.resid(spict_obj)

  # Nombre de archivo
  fname <- file.path(
    out_dir,
    paste0("OSA_", scenario, "_", run, ".png")
  )

  # Guardar plot
  png(filename = fname, width = 2400, height = 1800, res = 300)
  plotspict.diagnostic(res_osa, qlegend = FALSE)
  dev.off()

  invisible(res_osa)
}


osa_results <- list()

for (sc in names(results_by_scenario)) {
  osa_results[[sc]] <- list()

  for (run in names(results_by_scenario[[sc]])) {

    cat("Running OSA diagnostics:", sc, run, "\n")

    osa_results[[sc]][[run]] <- tryCatch(
      run_osa_diagnostics(
        spict_obj = results_by_scenario[[sc]][[run]],
        scenario  = sc,
        run       = run
      ),
      error = function(e) {
        message("❌ Error in ", sc, " ", run, ": ", e$message)
        NULL
      }
    )
  }
}



## -------------------Plot initaial default----------------------------------------------

run_spict_plot <- function(spict_obj,
                           scenario,
                           run,
                           CI = 0.8,
                           out_dir = "figs/spict_plots") {

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  fname <- file.path(
    out_dir,
    paste0("SPiCT_", scenario, "_", run, ".png")
  )

  png(filename = fname, width = 2400, height = 1800, res = 300)
  plot(spict_obj, CI = CI)
  dev.off()

  invisible(fname)
}
# Run plots for all scenarios and runs
spict_plots <- list()

for (sc in names(results_by_scenario)) {
  spict_plots[[sc]] <- list()

  for (run in names(results_by_scenario[[sc]])) {

    cat("Generating SPiCT plot:", sc, run, "\n")

    spict_plots[[sc]][[run]] <- tryCatch(
      run_spict_plot(
        spict_obj = results_by_scenario[[sc]][[run]],
        scenario  = sc,
        run       = run,
        CI = 0.8
      ),
      error = function(e) {
        message("❌ Error in ", sc, " ", run, ": ", e$message)
        NULL
      }
    )
  }
}

##-----------------------------Results Tables ------------------------------

# Create base results folder
dir.create("outputs/results", showWarnings = FALSE)

scenarios <- paste0("SC", 8:9)
runs <- paste0("RUN", 5)

for (sc in scenarios) {
  for (rn in runs) {

    message("Processing ", sc, " / ", rn)

    res <- results_by_scenario[[sc]][[rn]]

    # Create folder results/SCx/RUNy
    out_dir <- file.path("outputs/results", sc, rn)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    #
    # Tables SPiCT parameters
    #

    # Summary of estimates
    write.csv(
      round(sumspict.parest(res), 2),
      file = file.path(out_dir, "SummaryEstimates.csv")
    )

    # Reference points (stochastic)
    write.csv(
      round(sumspict.srefpoints(res), 2),
      file = file.path(out_dir, "RefPoints.csv")
    )

    # States
    write.csv(
      round(sumspict.states(res), 2),
      file = file.path(out_dir, "States.csv")
    )

    # Predictions
    write.csv(
      round(sumspict.predictions(res), 2),
      file = file.path(out_dir, "Predictions.csv")
    )
  }
}
