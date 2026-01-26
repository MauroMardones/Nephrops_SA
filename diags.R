
## --------------Function to extract diagnostics from a spict fit object------------------------------------------

# Table and plots
results_by_scenario<-readRDS("outputs/SPiCT_SC8_SC9_RUN8_results.rds")

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
export_spict_results <- function(results_by_scenario,
                                 base_dir = "outputs/results",
                                 digits = 2) {

  # Create base output directory
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  for (sc in names(results_by_scenario)) {

    for (rn in names(results_by_scenario[[sc]])) {

      message("Processing ", sc, " / ", rn)

      res <- results_by_scenario[[sc]][[rn]]

      # Create output directory: outputs/results/SCx/RUNy
      out_dir <- file.path(base_dir, sc, rn)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      # Safely write each output
      tryCatch({

        write.csv(
          round(sumspict.parest(res), digits),
          file = file.path(out_dir, "SummaryEstimates.csv")
        )

        write.csv(
          round(sumspict.srefpoints(res), digits),
          file = file.path(out_dir, "RefPoints.csv")
        )

        write.csv(
          round(sumspict.states(res), digits),
          file = file.path(out_dir, "States.csv")
        )

        write.csv(
          round(sumspict.predictions(res), digits),
          file = file.path(out_dir, "Predictions.csv")
        )

      }, error = function(e) {
        message("❌ Failed for ", sc, " / ", rn, ": ", e$message)
      })
    }
  }

  invisible(TRUE)
}
