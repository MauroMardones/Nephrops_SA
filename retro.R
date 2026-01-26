
## ---------------------------Retros----------------------------------------
##
# Correr análisis retrospectivo (retro)
#
# Generar y guardar los plots retrospectivos
#
# Guardar los objetos de resultados
#
# Iterar automáticamente por escenario y run

# retrosc1r1 <- retro(results_by_scenario$SC1$RUN1,
#                            nretroyear = 5,
#                            mc.cores = 1)
# # plot
# plotspict.retro(retrosc1r1)


run_spict_retro_simple <- function(fit,
                                   scenario,
                                   run,
                                   nretroyear = 5,
                                   fig_dir = "figs/retro",
                                   out_dir = "outputs/retro") {

  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Retro
  res_retro <- retro(
    fit,
    nretroyear = nretroyear,
    mc.cores = 1
  )

  # Guardar objeto
  saveRDS(
    res_retro,
    file = file.path(
      out_dir,
      paste0("RETRO_", scenario, "_", run, ".rds")
    )
  )

  # Guardar plot
  png(
    filename = file.path(
      fig_dir,
      paste0("RETRO_", scenario, "_", run, ".png")
    ),
    width = 2400,
    height = 1800,
    res = 300
  )
  plotspict.retro(res_retro)
  dev.off()

  return(res_retro)
}
# Run retrospectives for all scenarios and runs

retro_results <- list()

for (sc in names(results_by_scenario)) {

  message("\n=== Scenario:", sc, "===")
  retro_results[[sc]] <- list()

  for (run in names(results_by_scenario[[sc]])) {

    message("Running RETRO:", sc, run)

    retro_results[[sc]][[run]] <- tryCatch(
      run_spict_retro_simple(
        fit = results_by_scenario[[sc]][[run]],
        scenario = sc,
        run = run,
        nretroyear = 5
      ),
      error = function(e) {
        message("❌ Error in ", sc, " ", run, ": ", e$message)
        NULL
      }
    )
  }
}

#Objetos retros guardados en "retro_results"


### -------Extract rho parametrer by scenario--------
#
extract_mohn_spict <- function(retro_obj) {

  if (is.null(retro_obj)) {
    return(data.frame(
      Mohn_BBmsy = NA_real_,
      Mohn_FFmsy = NA_real_
    ))
  }

  mr <- tryCatch(
    mohns_rho(
      rep = retro_obj,
      what = c("BBmsy", "FFmsy"),
      annualfunc = mean
    ),
    error = function(e) NULL
  )

  if (is.null(mr)) {
    return(data.frame(
      Mohn_BBmsy = NA_real_,
      Mohn_FFmsy = NA_real_
    ))
  }

  data.frame(
    Mohn_BBmsy = mr["BBmsy"],
    Mohn_FFmsy = mr["FFmsy"]
  )
}

# Extraer rho
mohn_table <- data.frame()

for (sc in names(retro_results)) {
  for (run in names(retro_results[[sc]])) {

    mohn_vals <- extract_mohn_spict(retro_results[[sc]][[run]])

    mohn_table <- rbind(
      mohn_table,
      data.frame(
        Scenario = sc,
        Run = run,
        Mohn_BBmsy = mohn_vals$Mohn_BBmsy,
        Mohn_FFmsy = mohn_vals$Mohn_FFmsy
      )
    )
  }
}

mohn_table
# guaerda la tabla en outputs/retro
write.csv(
  mohn_table,
  file = "outputs/retro/mohns_rho_by_scenario_5_8.csv",
  row.names = FALSE
)
