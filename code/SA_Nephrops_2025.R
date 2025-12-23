rm(list = ls())
options(bitmapType = "cairo")
#XQuartz is a mess, put this in your onload to default to cairo instead (https://github.com/tidyverse/ggplot2/issues/2655)
# Lo mapas se hacen mas rapido
# solo para IOs
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "  ",
                      fig.align = 'center',
                      cache=FALSE,
                      warning = FALSE)

# install.packages("devtools")
# install.packages("TMB")
# #si hay problemas, instalarlo desde el github
# devtools::install_github("kaskr/adcomp", subdir = "TMB")
# # SPiCT now
# devtools::install_github("DTUAqua/spict/spict")
# #devtools::install_github("DTUAqua/spict/spict", ref = "1.2.8")
# # aqui algunas dependencias tambien necesitan ser instaladas
# install.packages("Rcpp")
# install.packages("ellipse")

library(usethis)
library(devtools)
library(ellipse)
library(spict) #comprobar esta versión de spict_v1.2.8
#library(MQMF) #Suprlus production models Malcom Haddon
library(tidyverse)
library(patchwork)
library(knitr)
library(egg) # ggarrange
library(ggthemes)
library(readxl)
library(tidyverse)
library(ggpubr)
# Paquetes necesari
library(GGally)
library(flextable)
library(officer)
library(here)
# para pheatmap
library(pheatmap)
# definir un directorio para guardar plott "figs"

fig.path <- here("figs")
if (!dir.exists(fig.path)) {
  dir.create(fig.path)
}


## -------- READ DATA --------------
# Por ahora no tengo Effort ni LPUE standar

bac <- read_excel(here("data",
                           "inputdata_spict_fu30_2025_Rev.xlsx")) %>%
  mutate(
    Effort = if_else(is.na(catch), NA_real_, 1),
    LPUE_std = catch / Effort
  )
# Data actualizada
# bac <- read_csv(here("data",
#                      "inputdata_FU30_wkbmsyspict.csv")) %>%
#   mutate(
#     Effort = if_else(is.na(catch), NA_real_, 1),
#     LPUE_std = catch / Effort
#   )



## -------------Plot Catch Index-------------------------------------------
bac_long <- bac %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")
p_catch <- ggplot(filter(bac_long, variable == "catch"),
                  aes(x = year, y = value)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = mean(bac$catch, na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(y = "Landings (tons)", x = "", title = "Landings Nephrops time series")
plot_index <- function(var_name, ylab){
  ggplot(filter(bac_long, variable == var_name),
         aes(x = year, y = value)) +
    geom_point(color = "darkred", size = 2) +
    geom_smooth(color = "darkred",
                se = TRUE,
                method = "loess",
                formula = y ~ x,
                linewidth = 0.8) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    labs(y = ylab, x = "", title = var_name)
}

p1 <- plot_index("arsaspr", "Index value")
p2 <- plot_index("arsaautum", "Index value")
p3 <- plot_index("isunepbioREV", "Index value")
p4 <- plot_index("isunepabunREV", "Index value")
p5 <- plot_index("rendiarsaspr", "Index value")
p6 <- plot_index("LPUEcomercial", "Index value")
p7 <- plot_index("Effort", "Index value")
p8 <- plot_index("LPUE_std", "Index value")

# combinar y guuardar en "figs"
fig_indices <- ggarrange(
  p_catch, p1, p2, p3, p4, p5, p6, p7, p8,
    ncol = 3,
    nrow = 3,
    labels = "AUTO"
  )


ggsave(
  filename = file.path(fig.path, "indices_nephrops_FU30.png"),
  plot = fig_indices,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 300
)

## ------------- Correlation----------

data_log <- bac %>%
  select(-year, -catch) %>%
  mutate(across(everything(), log))

cor_pearson  <- cor(data_log, use = "pairwise.complete.obs", method = "pearson")
cor_spearman <- cor(data_log, use = "pairwise.complete.obs", method = "spearman")

ph1 <- pheatmap(cor_pearson,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Correlation Heatmap (Pearson)",
         color = colorRampPalette(c("blue", "white", "red"))(50))

ph2 <- pheatmap(cor_spearman,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Correlation Heatmap (Spearman)",
         color = colorRampPalette(c("blue", "white", "red"))(50))


## --------- Preparing Data for Spict--------------

data <- bac
# Create Catch dataframe
C_nep <- data.frame(
  obsC = data$catch,
  timeC = data$year
)

# Create abundance index dataframes

I_arsa_spring <- data.frame(
  obsI = data$arsaspr,
  timeI = data$year + 0.25
)

I_arsa_autumn <- data.frame(
  obsI = data$arsaautum,
  timeI = data$year + 0.75
)

I_isunepbio <- data.frame(
  obsI = data$isunepbioREV,
  timeI = data$year + 0.5
)

I_isunepbio2 <- data.frame(
  obsI = data$isunepbio_2,
  timeI = data$year + 0.5
)

I_isunepabun <- data.frame(
  obsI = data$isunepabunREV,
  timeI = data$year + 0.5
)

I_rendiarsaspr <- data.frame(
  obsI = data$rendiarsaspr,
  timeI = data$year
)

I_LPUEcom <- data.frame(
  obsI = data$LPUEcomercial,
  timeI = data$year
)
# work in progress
I_Effort <- data.frame(
  obsI = data$Effort,
  timeI = data$year
)
# work in progress
I_LPUEstd <- data.frame(
  obsI = data$LPUE_std,
  timeI = data$year
)

ind  <- which(C_nep$timeC == 1994)
ind2 <- which(C_nep$timeC == 2025)


## ---------------Scenarios for SPiCT model-------------


# Scenario 0 -- only ISUNEP abundance index

inp0 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunepabun$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunepabun$obsI[ind:ind2]
  )
)


# Scenario 1 -- Landings + ISUNEPCA TV Survey + IBTS ARSA Survey
inp1 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunepabun$timeI[ind:ind2],   # ISUNEPCA TV
    I_arsa_spring$timeI[ind:ind2]  # ARSA spring
  ),

  obsI = list(
    I_isunepabun$obsI[ind:ind2],
    I_arsa_spring$obsI[ind:ind2]
  )
)




# SCENARIO 2 # Landings + ISUNEPCA TV Survey + Directed LPUE
inp2 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunepabun$timeI[ind:ind2],   # ISUNEPCA TV
    I_LPUEcom$timeI[ind:ind2]       # Directed LPUE
  ),

  obsI = list(
    I_isunepabun$obsI[ind:ind2],
    I_LPUEcom$obsI[ind:ind2]
  )
)


# Scenario 3 -- Landings + ISUNEPCA TV Survey + Directed Effort

I_effort <- data.frame(
  obsI  = data$Effort,
  timeI = data$year
)

inp3 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunepabun$timeI[ind:ind2],
    I_effort$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunepabun$obsI[ind:ind2],
    I_effort$obsI[ind:ind2]
  )
)



# Scenario 4 -- Catches, ISUNEPCA LPUE Std

inp4 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunepabun$timeI[ind:ind2],   # ISUNEPCA TV
    I_LPUEstd$timeI[ind:ind2]       # Directed LPUE
  ),

  obsI = list(
    I_isunepabun$obsI[ind:ind2],
    I_LPUEstd$obsI[ind:ind2]
  )
)



# Scenario 5 -- All Indices
inp5 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_arsa_spring$timeI[ind:ind2],   # 1. ARSA spring
    I_arsa_autumn$timeI[ind:ind2],   # 2. ARSA autumn
    I_isunepbio$timeI[ind:ind2],     # 3. ISUNEP biomass
    I_isunepabun$timeI[ind:ind2],    # 4. ISUNEP abundance
    I_rendiarsaspr$timeI[ind:ind2],  # 5. Rendimiento ARSA spring
    I_LPUEcom$timeI[ind:ind2]        # 6. LPUE comercial
  ),

  obsI = list(
    I_arsa_spring$obsI[ind:ind2],
    I_arsa_autumn$obsI[ind:ind2],
    I_isunepbio$obsI[ind:ind2],
    I_isunepabun$obsI[ind:ind2],
    I_rendiarsaspr$obsI[ind:ind2],
    I_LPUEcom$obsI[ind:ind2]
  )
)


inp1$dteuler <- 1/16  # must be set before check.inp
inp1 <- check.inp(inp1)

# Inspect
inp1$dtc


## -----------Priors configurations-------------------------------
# Priors list
list.possible.priors()

priors_run1 <- list(
  name = "RUN1_default",
  priors = NULL
)

priors_run2 <- list(
  name = "RUN2_logbkfrac",
  priors = list(
    logbkfrac = c(log(0.5), 0.2, 1)
  )
)

priors_run3 <- list(
  name = "RUN3_logbkfrac_logn",
  priors = list(
    logbkfrac = c(log(0.5), 0.2, 1),
    logn      = c(log(2),   0.5, 1)
  )
)

priors_run4 <- list(
  name = "RUN4_logbkfrac_logn_logr",
  priors = list(
    logbkfrac = c(log(0.5), 0.2, 1),
    logn      = c(log(2),   0.5, 1),
    logr      = c(log(0.2), 0.2, 1)
  )
)

# Grouped Scenarios

scenarios_data <- list(
  SC0 = inp0,
  SC1 = inp1,
  SC2 = inp2,
  SC3 = inp3,
  SC4 = inp4,
  SC5 = inp5
)
scenarios_priors <- list(
  RUN1 = priors_run1,
  RUN2 = priors_run2,
  RUN3 = priors_run3,
  RUN4 = priors_run4
)

###
# Create all combinations of scenarios and priors.
#Now "spict_scenarios" objetct contains all combinations of data scenarios and prior configurations

spict_scenarios <- list()
for (sc_name in names(scenarios_data)) {
  for (run_name in names(scenarios_priors)) {
    spict_scenarios[[paste(sc_name, run_name, sep = "_")]] <- list(
      scenario = sc_name,
      run      = run_name,
      input    = scenarios_data[[sc_name]],
      priors   = scenarios_priors[[run_name]]$priors
    )
  }
}
#


# Run SPICT

results_by_scenario <- list()

for (sc_name in names(scenarios_data)) {

  cat("\nRunning Scenario:", sc_name, "\n")
  results_by_scenario[[sc_name]] <- list()

  for (run_name in names(scenarios_priors)) {

    cat("  - Run:", run_name, "\n")

    # Copia limpia del input
    current_input <- scenarios_data[[sc_name]]

    # Añadir priors SOLO si existen
    if (!is.null(scenarios_priors[[run_name]]$priors)) {
      current_input$priors <- scenarios_priors[[run_name]]$priors
    }

    fit <- tryCatch(
      {
        fit.spict(
          inp = current_input,
          verbose = FALSE
        )
      },
      error = function(e) {
        message("    ❌ Error in ", sc_name, " ", run_name, ": ", e$message)
        return(NULL)
      }
    )

    results_by_scenario[[sc_name]][[run_name]] <- fit
  }
}

## --------------------Read Scenarios----------------------------------------------

# Example access to results

names(results_by_scenario)

# individual Scenario
names(results_by_scenario$SC1)

# Individual Scenario and run (e.i.)
results_by_scenario$SC0$RUN4

# genera  un .rsd por escenario

saveRDS(results_by_scenario, "outputs/SPiCT_full_results.rds")


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
  png(filename = fname, width = 2000, height = 1600, res = 300)
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


# leer outputs por escenaruio
# osa_results$SC1$RUN1

## Save al outputs
saveRDS(
  osa_results,
  file = "outputs/SPiCT_OSA_results_by_scenario.rds"
)


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


## -------extract rho parametrer by scenario--------
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
  file = "outputs/retro/mohns_rho_by_scenario.csv",
  row.names = FALSE
)

## ---------------------------Obtener AIC----------------------------------

# Because AIC depends on the likelihood associated with a
# given data configuration, its values are not directly comparable
# across models that differ in the number or type of input data.
# Therefore, AIC-based comparisons were restricted to models within the same scenario.

# Tabla de AIC
aic_table <- data.frame()

for (sc in names(results_by_scenario)) {
  for (run in names(results_by_scenario[[sc]])) {

    fit <- results_by_scenario[[sc]][[run]]

    aic_val <- tryCatch(
      get.AIC(fit),
      error = function(e) NA_real_
    )

    aic_table <- rbind(
      aic_table,
      data.frame(
        Scenario = sc,
        Run = run,
        AIC = aic_val
      ) %>%
        arrange(Scenario, AIC)
    )
  }
}

aic_table

write.csv(
  aic_table,
  file = "outputs/AIC_by_scenario_run.csv",
  row.names = FALSE
)
aic_table <- aic_table[order(aic_table$Scenario, aic_table$AIC), ]


## --------------------------Extrae BB and F---------------------------------------

extract_FF_BB_MSY <- function(res){

  #  FFMSY
  ffmsy <- exp(as.data.frame(get.par("logFFmsy", res)))
  ffmsy$year <- round(as.numeric(rownames(ffmsy)), 0)

  FFMSY <- ffmsy %>%
    group_by(year) %>%
    summarise(FFMSY = mean(est), .groups = "drop")

  #  BBMSY
  bbmsy <- exp(as.data.frame(get.par("logBBmsy", res)))
  bbmsy$year <- round(as.numeric(rownames(bbmsy)), 0)

  BBMSY <- bbmsy %>%
    group_by(year) %>%
    summarise(BBMSY = mean(est), .groups = "drop")

  # combina
  kobebro <- left_join(FFMSY, BBMSY, by = "year")

  return(kobebro)
}
# Create table for all scenarios and runs
kobebro_table <- data.frame()

for (sc in names(results_by_scenario)) {
  for (run in names(results_by_scenario[[sc]])) {

    res <- results_by_scenario[[sc]][[run]]

    tmp <- tryCatch(
      extract_FF_BB_MSY(res),
      error = function(e) NULL
    )

    if (!is.null(tmp)) {
      tmp$Scenario <- sc
      tmp$Run <- run

      kobebro_table <- bind_rows(kobebro_table, tmp)
    }
  }
}
# Reorder columns
kobebro_table <- kobebro_table %>%
  select(Scenario, Run, year, FFMSY, BBMSY)

# Save table
write.csv(
  kobebro_table,
  "outputs/FFMSY_BBMSY_by_scenario_run_year.csv",
  row.names = FALSE
)

# Plots

plot_kobe_scenario <- function(df, scenario_name) {

  ggplot(
    df %>% filter(Scenario == scenario_name),
    aes(x = FFMSY, y = BBMSY)
  ) +
    # Cuadrantes Kobe
    annotate("rect", xmin = 1, xmax = Inf, ymin = 1, ymax = Inf,
             fill = "orange", alpha = 0.4) +
    annotate("rect", xmin = 0, xmax = 1, ymin = 1, ymax = Inf,
             fill = "red", alpha = 0.4) +
    annotate("rect", xmin = 1, xmax = Inf, ymin = 0, ymax = 1,
             fill = "lightgreen", alpha = 0.4) +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "orange", alpha = 0.4) +

    # Trayectoria temporal
    geom_path(color = "black", linewidth = 0.8) +
    geom_point(aes(color = year), size = 2) +

    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +

    scale_color_viridis_c(name = "Year") +

    facet_wrap(~Run, ncol = 2) +

    coord_cartesian(xlim = c(0, max(df$FFMSY, na.rm = TRUE) * 1.1),
                    ylim = c(0, max(df$BBMSY, na.rm = TRUE) * 1.1)) +

    labs(
      title = paste("Kobe plot –", scenario_name),
      x = expression(F/F[MSY]),
      y = expression(B/B[MSY])
    ) +

    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
}
# Generate and save plots for each scenario
for (sc in unique(kobebro_table$Scenario)) {
  p_kobe <- plot_kobe_scenario(kobebro_table, sc)

  ggsave(
    filename = file.path(fig.path, paste0("Kobe_plot_", sc, ".png")),
    plot = p_kobe,
    width = 18,
    height = 12,
    units = "cm",
    dpi = 300
  )
}

## ---------------------- manage tables and figs-------------------------------------
# Aplicar manejo (manage)
#
# Añadir el HCR ICES (2025) para especies vulnerables (fractil 0.15)
#
# Ejecutarlo para todos los escenarios y todos los runs
#
# Guardar los resultados de manejo por escenario/run
#
# Mantener una estructura ordenada y reutilizable


## Ojo que se demora mucho!!!

run_management_spict <- function(fit){

  fit_m <- manage(fit)

  fit_m <- add.man.scenario(
    fit_m,
    name = "ICES_2025_0.15_fractile",
    fractiles = list(catch = 0.15),
    breakpointB = 0.5,
    limitB = 0.3
  )

  return(fit_m)
}
# Run management for all scenarios and runs
management_results <- list()

for (sc in names(results_by_scenario)) {
  management_results[[sc]] <- list()

  for (run in names(results_by_scenario[[sc]])) {

    fit <- results_by_scenario[[sc]][[run]]

    fit_m <- tryCatch(
      run_management_spict(fit),
      error = function(e) NULL
    )

    management_results[[sc]][[run]] <- fit_m
  }
}
# Extract management summary
management_summary <- data.frame()

for (sc in names(management_results)) {
  for (run in names(management_results[[sc]])) {

    fit_m <- management_results[[sc]][[run]]

    if (is.null(fit_m)) next

    sm <- tryCatch(
      sumspict.manage(fit_m),
      error = function(e) NULL
    )

    if (!is.null(sm)) {
      sm$Scenario <- sc
      sm$Run <- run
      management_summary <- rbind(management_summary, sm)
    }
  }
}
# Save management summary
saveRDS(
  management_results,
  file = "outputs/SPiCT_management_results_by_scenario.rds"
)


## --------- Extract TAC predictions function--------------

# get.TAC‘: gives the catch predicted management scenario
# ‘man.tac‘: gives the catch prediction of all defined
# management scenarios


## Get the TAC for the ICES (2020) recommended HCR (as used in WKMSYSPICT)
reptest1 <- add.man.scenario(results_by_scenario$SC0$RUN4, fractiles = list(catch=0.35), breakpointB = c(0.3, 0.5))

## Now `rep` includes 3 management scenarios

## Get the TAC when fishing mortality is equal to Fmsy
get.TAC(reptest1)

##  ----Hindcast MASE calculation ----

sc0r1h <- hindcast(results_by_scenario$SC0$RUN4)

## -------------- Comparision plots -----------
# Lista para almacenar resultados
BF_list <- list()

for (sc in names(results_by_scenario)) {

  for (rn in names(results_by_scenario[[sc]])) {

    rep <- results_by_scenario[[sc]][[rn]]

        # biomass
    B <- exp(as.data.frame(get.par("logB", rep)))
    B$year <- as.numeric(rownames(B))

    # F
    F <- exp(as.data.frame(get.par("logF", rep)))
    F$year <- as.numeric(rownames(F))

    # combine
    BF_df <- B %>%
      select(
        year,
        Biomass = est,
        Biomass_lwr = ll,
        Biomass_upr = ul
      ) %>%
      left_join(
        F %>%
          select(
            year,
            FishingMortality = est,
            F_lwr = ll,
            F_upr = ul
          ),
        by = "year"
      ) %>%
      mutate(
        Scenario = sc,
        Run = rn
      )

    BF_list[[paste(sc, rn, sep = "_")]] <- BF_df
  }
}

# Combinar todo en un solo data frame
BF_all <- bind_rows(BF_list)
## Guardar resultados
write.csv(
  BF_all,
  "outputs/SPiCT_BB_F_all_scenarios.csv",
  row.names = FALSE
)

# Plot por variable
for (sc in unique(BF_all$Scenario)) {

  df_sc <- BF_all %>%
    filter(Scenario == sc)

  p_B <- ggplot(df_sc,
                aes(x = year,
                    y = Biomass,
                    color = Run,
                    group = Run)) +
    geom_line(linewidth = 0.9) +
    theme_bw(base_size = 12) +
    labs(
      title = paste("Estimated biomass trajectories –", sc),
      x = "Year",
      y = "Biomass",
      color = "Run"
    )

  ggsave(
    filename = paste0("figs/Biomass_", sc, ".png"),
    plot = p_B,
    width = 18,
    height = 12,
    units = "cm",
    dpi = 300
  )
}
# F plot
for (sc in unique(BF_all$Scenario)) {

  df_sc <- BF_all %>%
    filter(Scenario == sc)

  p_F <- ggplot(df_sc,
                aes(x = year,
                    y = FishingMortality,
                    color = Run,
                    group = Run)) +
    geom_line(linewidth = 0.9) +
    theme_bw(base_size = 12) +
    labs(
      title = paste("Estimated fishing mortality trajectories –", sc),
      x = "Year",
      y = "Fishing Mortality",
      color = "Run"
    )

  ggsave(
    filename = paste0("figs/FishingMortality_", sc, ".png"),
    plot = p_F,
    width = 18,
    height = 12,
    units = "cm",
    dpi = 300
  )
}

# para despues
## --------------------------------------------------------------------------------

## Extraigo Valores

extract_spict_diagnostics <- function(fit) {

  if (is.null(fit)) {
    return(list(
      convergence = FALSE,
      pdHess = NA,
      osa_bias_p = NA,
      osa_acf_p = NA,
      shapiro_p = NA,
      mohn_bbmsy = NA,
      mohn_ffmsy = NA,
      prod_curve = NA,
      uncertainty_bbmsy = NA,
      uncertainty_ffmsy = NA,
      aic = NA_real_
    ))
  }

  # AIC seguro
  aic_val <- NA_real_
  if (!is.null(fit$aic) && is.numeric(fit$aic)) {
    aic_val <- fit$aic
  }

  list(
    convergence = fit$opt$convergence == 0,

    pdHess = if (!is.null(fit$sdrep))
      isTRUE(fit$sdrep$pdHess) else NA,

    osa_bias_p = if (!is.null(fit$diagnostics$osa_bias_p))
      fit$diagnostics$osa_bias_p else NA,

    osa_acf_p = if (!is.null(fit$diagnostics$osa_acf_p))
      fit$diagnostics$osa_acf_p else NA,

    shapiro_p = if (!is.null(fit$diagnostics$shapiro_p))
      fit$diagnostics$shapiro_p else NA,

    mohn_bbmsy = if (!is.null(fit$retro$mohn$bbmsy))
      fit$retro$mohn$bbmsy else NA,

    mohn_ffmsy = if (!is.null(fit$retro$mohn$ffmsy))
      fit$retro$mohn$ffmsy else NA,

    prod_curve = if (!is.null(fit$par$n))
      fit$par$n else NA,

    uncertainty_bbmsy = if (!is.null(fit$sdrep))
      sd(fit$sdrep$value[grep("bbmsy", names(fit$sdrep$value))],
         na.rm = TRUE) else NA,

    uncertainty_ffmsy = if (!is.null(fit$sdrep))
      sd(fit$sdrep$value[grep("ffmsy", names(fit$sdrep$value))],
         na.rm = TRUE) else NA,

    aic = aic_val
  )
}


# round function
round_safe <- function(x, digits = 2) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    round(x, digits)
  } else {
    NA
  }
}

# made table

library(flextable)
library(dplyr)

build_diagnostic_table <- function(results_scenario, scenario_name) {

  runs <- names(results_scenario)

  diag_list <- lapply(results_scenario, extract_spict_diagnostics)

  diag_df <- tibble(
    Diagnostic = c(
      "Convergence",
      "Parameters variance finite (pdHess)",
      "OSA residuals – Bias p-value",
      "OSA residuals – ACF Ljung-Box p-value",
      "OSA residuals – Sample quantiles (Shapiro p-value)",
      "Mohn's rho (B/BMSY)",
      "Mohn's rho (F/FMSY)",
      "Production curve parameter (n)",
      "Uncertainty order of magnitude (B/BMSY)",
      "Uncertainty order of magnitude (F/FMSY)",
      "AIC"
    )
  )

  for (run in runs) {
    d <- diag_list[[run]]

    diag_df[[run]] <- c(
      ifelse(isTRUE(d$convergence), "✓", "✗"),
      ifelse(isTRUE(d$pdHess), "TRUE", "FALSE"),
      round_safe(d$osa_bias_p, 3),
      round_safe(d$osa_acf_p, 3),
      round_safe(d$shapiro_p, 3),
      round_safe(d$mohn_bbmsy, 3),
      round_safe(d$mohn_ffmsy, 3),
      round_safe(d$prod_curve, 3),
      round_safe(d$uncertainty_bbmsy, 3),
      round_safe(d$uncertainty_ffmsy, 3),
      round_safe(d$aic, 2)
    )
  }

  flextable(diag_df) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    align(j = 2:ncol(diag_df), align = "center", part = "all") |>
    align(j = 1, align = "left", part = "all") |>
    valign(valign = "top", part = "all") |>
    theme_booktabs() |>
    autofit() |>
    set_caption(
      paste0(
        scenario_name,
        ": summary of convergence, diagnostics, uncertainty and model performance across prior configurations."
      )
    )
}



# Tabla por escenario

ft_SC1 <- build_diagnostic_table(results_by_scenario$SC1, "Scenario 1")
ft_SC2 <- build_diagnostic_table(results_by_scenario$SC2, "Scenario 2")
ft_SC3 <- build_diagnostic_table(results_by_scenario$SC3, "Scenario 3")
ft_SC4 <- build_diagnostic_table(results_by_scenario$SC4, "Scenario 4")
ft_SC5 <- build_diagnostic_table(results_by_scenario$SC5, "Scenario 5")





