
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

run_spict_retro_light <- function(fit,
                                  scenario,
                                  run,
                                  nretroyear = 5,
                                  fig_dir = "figs/retro") {

  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  # correr retro (objeto temporal)
  res_retro <- retro(
    fit,
    nretroyear = nretroyear,
    mc.cores = 1
  )

  ## ---- Plot estándar retro ----
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

  ## ---- Plot incertidumbre ----
  png(
    filename = file.path(
      fig_dir,
      paste0("RETRO_UNCERTAINTY_", scenario, "_", run, ".png")
    ),
    width = 2400,
    height = 1800,
    res = 300
  )
  plotspict.retro.fixed(res_retro)
  dev.off()

  ## ---- Extraer Mohn's rho ----
  mr <- tryCatch(
    mohns_rho(
      rep = res_retro,
      what = c("BBmsy", "FFmsy"),
      annualfunc = mean
    ),
    error = function(e) NULL
  )

  ## limpiar memoria explícitamente
  rm(res_retro)
  gc(verbose = FALSE)

  if (is.null(mr)) {
    return(data.frame(
      Scenario = scenario,
      Run = run,
      Mohn_BBmsy = NA_real_,
      Mohn_FFmsy = NA_real_
    ))
  }

  data.frame(
    Scenario = scenario,
    Run = run,
    Mohn_BBmsy = unname(mr["BBmsy"]),
    Mohn_FFmsy = unname(mr["FFmsy"])
  )
}


mohn_table <- data.frame()

for (sc in names(results_by_scenario)) {

  message("\n=== Scenario:", sc, "===")

  for (run in names(results_by_scenario[[sc]])) {

    message("Running RETRO:", sc, run)

    res_row <- tryCatch(
      run_spict_retro_light(
        fit = results_by_scenario[[sc]][[run]],
        scenario = sc,
        run = run,
        nretroyear = 5
      ),
      error = function(e) {
        message("❌ Error in ", sc, " ", run, ": ", e$message)
        data.frame(
          Scenario = sc,
          Run = run,
          Mohn_BBmsy = NA_real_,
          Mohn_FFmsy = NA_real_
        )
      }
    )

    mohn_table <- rbind(mohn_table, res_row)
  }
}

## ------------- Guarda resultados  de Mohn-----------------

dir.create("outputs/retro", showWarnings = FALSE, recursive = TRUE)

write.csv(
  mohn_table,
  file = "outputs/retro/mohns_rho_by_scenario_5_9.csv",
  row.names = FALSE
)

mohn_table
