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
lapply(c("figs", "outputs"),
       function(x) if(!dir.exists(x))
         dir.create(x, recursive = TRUE))



## -------- Read Data--------------
# Data actualizada
bac <- read_csv(here("data",
                     "inputdata_FU30_wkbmsyspict.csv"))
# mean

# bac_means <- bac %>%
#   summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


## -------------Plot Catch Index-------------------------------------------
bac_long <- bac %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")
p_catch <- ggplot(
  filter(bac_long, variable == "catch"),
  aes(x = year, y = value)
) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_hline(
    yintercept = mean(bac$catch, na.rm = TRUE),
    linetype = "dashed",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(min(bac$year, na.rm = TRUE),
                 max(bac$year, na.rm = TRUE),
                 by = 1)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 8
    )
  ) +
  labs(y = "Landings (tons)",
       x = "", title = "Landings Nephrops time series")
plot_index <- function(var_name, panel_title){
  ggplot(
    filter(bac_long, variable == var_name),
    aes(x = year, y = value)
  ) +
    geom_point(color = "darkred", size = 2) +
    geom_smooth(
      color = "darkred",
      se = TRUE,
      method = "loess",
      formula = y ~ x,
      linewidth = 0.8
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.title = element_text(
        size = 11,
        face = "bold",
        hjust = 0.5
      )
    ) +
    labs(
      x = "",
      y = "",
      title = panel_title
    )
}


#Índices ARSA (completos)
p1 <- plot_index("arsabio", "ARSA biomass index")
p2 <- plot_index("arsarendi", "ARSA productivity index")
p3 <- plot_index("arsarendistand_grh", "ARSA standardized productivity (GRH)")
p4 <- plot_index("arsarendistand_Kgh", "ARSA standardized productivity (Kgh)")
p5 <- plot_index("arsa_std_nor", "ARSA standardized index (normalized)")
p6 <- plot_index("arsa_CV_std_nor", "ARSA CV (standardized)")

# Índices ISUNEPCA (UWTV)
p7  <- plot_index("isunep_bio1", "ISUNEPCA UWTV biomass index")
p8  <- plot_index("isunep_bio1_nor", "ISUNEPCA biomass (normalized)")
p9  <- plot_index("isunep_abun", "ISUNEPCA UWTV abundance index")
p10 <- plot_index("CV_isunep_abun", "ISUNEPCA CV")

# Índices dependientes de la pesquería
p11 <- plot_index("LPUE_10%nep", "Commercial LPUE (≥10% Nephrops)")
p12 <- plot_index("LPUE_std_target_year", "Standardized LPUE (target year)") # Modificar
p13 <- plot_index("LPUE_std_Vessel_RE", "Standardized LPUE (Vessel_RE)") # Modificar
p14 <- plot_index("Effort_10%nep", "Directed fishing effort (≥10% Nephrops)")
p15 <- plot_index("Total_Effort", "Total fishing effort")

# combinar y guuardar en "figs"
fig_indices <- ggarrange(
  p1,  p2,  p3,
  p4,  p5,  p6,
  p7,  p8,  p9,
  p10, p11, p12,
  p13, p14, p15,
  ncol = 3,
  nrow = 5,
  font.label = list(size = 10)
)

ggsave(
  filename = file.path(fig.path, "indices_nephrops_FU30_all.png"),
  plot = fig_indices,
  width = 25,
  height = 41,
  units = "cm",
  dpi = 300
)

# save p_catch
ggsave(
  filename = file.path(fig.path, "landings_nephrops_FU30.png"),
  plot = p_catch,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 300
)


## ------------- Correlation----------

data_log <- bac %>%
  select(-year, -catch) %>%
  mutate(across(everything(), log))

cor_pearson  <- cor(data_log, use = "pairwise.complete.obs", method = "pearson")
cor_spearman <- cor(data_log, use = "pairwise.complete.obs", method = "spearman")

ph1 <- pheatmap(
  cor_pearson,
  display_numbers = TRUE,
  number_format = "%.2f",
  main = "Correlation Heatmap (Pearson)",
  color = colorRampPalette(c("blue", "white", "red"))(50),
  cluster_rows = FALSE,
  cluster_cols = FALSE
)

ph2 <- pheatmap(
  cor_spearman,
  display_numbers = TRUE,
  number_format = "%.2f",
  main = "Correlation Heatmap (Pearson)",
  color = colorRampPalette(c("blue", "white", "red"))(50),
  cluster_rows = FALSE,
  cluster_cols = FALSE
)


# save plots
# ggsave(
#   filename = file.path(fig.path, "correlation_pearson_nephrops_FU30.png"),
#   plot = ph1,
#   width = 20,
#   height = 18,
#   units = "cm",
#   dpi = 300
# )
# ggsave(
#   filename = file.path(fig.path, "correlation_spearman_nephrops_FU30.png"),
#   plot = ph2,
#   width = 20,
#   height = 18,
#   units = "cm",
#   dpi = 300
# )

## --------- Preparing Data for Spict--------------

data <- bac
# Create Catch dataframe
C_nep <- data.frame(
  obsC = data$catch,
  timeC = data$year
)

# Create abundance index dataframes

# ARSA biomass index
I_arsa_bio <- data.frame(
  obsI  = bac$arsabio,
  timeI = bac$year + 0.75
)

# ARSA productivity index (raw)
I_arsa_rendi <- data.frame(
  obsI  = bac$arsarendi,
  timeI = bac$year + 0.75
)

# ARSA productivity – standardized (GRH scale)
I_arsa_rendi_std_grh <- data.frame(
  obsI  = bac$arsarendistand_grh,
  timeI = bac$year + 0.75
)

# ARSA productivity – standardized (Kgh scale)
I_arsa_rendi_std_kgh <- data.frame(
  obsI  = bac$arsarendistand_Kgh,
  timeI = bac$year + 0.75
)

# ARSA standardized normalized index
I_arsa_std_nor <- data.frame(
  obsI  = bac$arsa_std_nor,
  timeI = bac$year + 0.75
)

# ARSA CV standardized normalized
I_arsa_cv_std_nor <- data.frame(
  obsI  = bac$arsa_CV_std_nor,
  timeI = bac$year + 0.75
)

# UWTV biomass index
I_isunep_bio <- data.frame(
  obsI  = bac$isunep_bio1,
  timeI = bac$year + 0.5
)

# UWTV biomass
I_isunep_bio_nor <- data.frame(
  obsI  = bac$isunep_bio1_nor,
  timeI = bac$year + 0.5
)

# UWTV abundance index
I_isunep_abun <- data.frame(
  obsI  = bac$isunep_abun,
  timeI = bac$year + 0.5
)

# UWTV CV normalized
I_isunep_cv_nor <- data.frame(
  obsI  = bac$CV_isunep_abun,
  timeI = bac$year + 0.5
)
# LPUE (10% Nephrops fleet)
I_LPUE_10nep <- data.frame(
  obsI  = bac$`LPUE_10%nep`,
  timeI = bac$year
)

# Effort (10% Nephrops fleet) # work in progress
I_Effort_10nep <- data.frame(
  obsI  = bac$`Effort_10%nep`,
  timeI = bac$year
)

# Total effort # work in progress
I_Total_Effort <- data.frame(
  obsI  = bac$Total_Effort,
  timeI = bac$year
)

# Standardized LPUE # work in progress
I_LPUE_std <- data.frame(
  obsI  = bac$LPUE_std_target_year,
  timeI = bac$year
)

# Indices time range

ind  <- which(C_nep$timeC == 1987)
ind2 <- which(C_nep$timeC == 2025)


## ---------------Scenarios for SPiCT model-------------


# Scenario 0 -- only ISUNEP abundance index

inp0 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2]
  )
)


# Scenario 1 -- Landings + ISUNEPCA UWTV abundance + ARSA biomass

inp1 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2],
    I_arsa_bio$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_arsa_bio$obsI[ind:ind2]
  )
)

# Scenario 2 -- Landings + ISUNEPCA UWTV abundance + standardized LPUE

inp2 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2],
    I_LPUE_std$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_LPUE_std$obsI[ind:ind2]
  )
)

# Scenario 3 -- Landings + ISUNEPCA UWTV abundance + total effort

inp3 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2],
    I_Total_Effort$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_Total_Effort$obsI[ind:ind2]
  )
)

# Scenario 4-- Landings + ISUNEPCA bio + standardized LPUE
#(idéntico en estructura a SC2, solo difiere la unidad del indice ISUNEPCA (bio -Abund))

inp4 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_bio$timeI[ind:ind2],
    I_LPUE_std$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_bio $obsI[ind:ind2],
    I_LPUE_std$obsI[ind:ind2]
  )
)

# new scenarios
# Scenario 5 — Scenario combining total landings with ISUNEPCA UWTV abundance and normalized ARSA
#yield indices to assess consistency between fishery-independent indices.

inp5 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2],
    I_arsa_rendi_std_kgh$timeI[ind:ind2]
    ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_arsa_rendi_std_kgh$obsI[ind:ind2]
   )
)

# Scenario 6-- Scenario integrating total landings with  ISUNEPCA UWTV abundance and
#a standardized LPUE index, combining fishery-independent and fishery-dependent information.

inp6 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2],
    I_LPUE_std$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_LPUE_std$obsI[ind:ind2]
  )
)

# Scenario 7-- Most information-rich configuration,
#combining total landings with ISUNEPCA UWTV abundance,
#normalized ARSA yield, and standardized LPUE indices

inp7 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2],
    I_arsa_rendi_std_kgh$timeI[ind:ind2],
    I_LPUE_std$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_arsa_rendi_std_kgh$obsI[ind:ind2],
    I_LPUE_std$obsI[ind:ind2]
  )
)

#Scenario 8 -- Scenario integrating total landings with
#normalized ISUNEPCA UWTV biomasss and a standardized LPUE index,
#combining fishery-independent and fishery-dependent information.

inp8 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_bio$timeI[ind:ind2],
    I_LPUE_std$timeI[ind:ind2]
  ),

  obsI = list(
    I_isunep_bio$obsI[ind:ind2],
    I_LPUE_std$obsI[ind:ind2]
  )
)

#Scenario 9 -- Scenario combines total landings with the ISUNEPCA UWTV abundance (2015–2025),
#ARSA biomass survey (1993–2012),
#and the standardised commercial LPUE (2009–2024).
# Scenario combines total landings with the ISUNEPCA UWTV biomass index,
# ARSA yield, and the standardised commercial LPUE, integrating long-term
# and recent information while ensuring consistency among data sources

inp9 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_abun$timeI[ind:ind2],      # Índice 1
    I_arsa_rendi_std_kgh$timeI[7:26],   # Índice 2 (ARSA)
    I_LPUE_std$timeI[23:38]            # Índice 3
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_arsa_rendi_std_kgh$obsI[7:26],
    I_LPUE_std$obsI[23:38]
  )
)
# # 1. Inicializar stdevfacI con 1s para todos los índices
# inp9$stdevfacI <- list(
#   rep(1, length(inp9$obsI[[1]])),
#   rep(1, length(inp9$obsI[[2]])),
#   rep(1, length(inp9$obsI[[3]]))
# )
#
# # 2. Aplicar incertidumbre extra a ARSA (Índice 2)
# # Buscamos la posición de los años 2020 y 2024 dentro del vector de tiempo de ARSA
# años_con_incertidumbre <- c(2020, 2024)
# idx_arsa <- which(inp9$timeI[[2]] %in% años_con_incertidumbre)
#
# # Asignamos un factor (ejemplo: 10 para indicar que el dato es casi irrelevante)
# inp9$stdevfacI[[2]][idx_arsa] <- 10
#


# must be set before check.inp
inp_list <- list(
  SC0 = inp0,
  SC1 = inp1,
  SC2 = inp2,
  SC3 = inp3,
  SC4 = inp4,
  SC5 = inp5,
  SC6 = inp6,
  SC7 = inp7,
  SC8 = inp8,
  SC9 = inp9
)
# Check inputs

inp_list_checked <- lapply(inp_list, function(inp) {
  inp$dteuler <- 1 / 16
  inp <- check.inp(inp)
  return(inp)
})

sapply(inp_list_checked, function(x) {
  c(
    n_catch = length(x$obsC),
    n_index = x$nindex,
    dtc_min = min(x$dtc),
    dtc_max = max(x$dtc)
  )
})


## -----------Priors configurations-------------------------------
# Priors list
list.possible.priors()


##
# old Prior configurations set
##

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

#
# New Prior configurations set
#
# RUN5 — Default SPiCT priors
priors_run5 <- list(
  name   = "RUN5_default",
  priors = NULL
)

# RUN6 — No prior on logbkfrac (n kept at Schaefer-type default)
priors_run6 <- list(
  name = "RUN6_no_logbkfrac_logn",
  priors = list(
    #logbkfrac = c(log(0.5), 0.2, 1),
      logn  = c(log(2), 0.5, 1),   # prior on n only (Schaefer-type)
      logr  = c(log(0.5), 0.2, 1),
      logsdb = c(log(0.2), 0.5), # prior on catch process uncertainty
      logsdc = c(log(3), 0.5, 1), # prior decrease catch sd
      logdf = c(log(0.1), 0.2, 1) # Decresase f error sd
  )
)

# RUN7 — Prior on index observation uncertainty (CV ≈ 0.2)
priors_run7 <- list(
  name = "RUN7_index_cv",
  priors = list(
    #logbkfrac = c(log(0.5), 0.2, 1),
    #logsdi = c(log(0.1), 0.2, 1),
    logn      = c(log(2),   0.5, 1),
    logr      = c(log(0.5), 0.2, 1)
    # logsdb = c(log(0.2), 0.5),
    # logsdc = c(log(3), 0.5, 1),
    # logdf = c(log(0.1), 0.2, 1)
  )
)

# RUN8 — Alpha and beta stabilising priors disabled
priors_run8 <- list(
  name = "RUN8_no_alpha_beta",
  priors = list(
    #logbkfrac = c(log(0.5), 0.2, 1),
    logalpha = c(0, 0, 0), # disable
    logbeta  = c(0, 0, 0), # disable , to activate, change to (0,0,0)
    logn      = c(log(2),   0.5, 1),
    logr      = c(log(0.5), 0.2, 1)
    # logsdb = c(log(0.2), 0.5),
    # logsdc = c(log(3), 0.5, 1),
    # logdf = c(log(0.1), 0.2, 1)
  )
)

# Collect runs
# Grouped Scenarios and Priors
# here we define wich combinations of scenarios and priors we want to run

scenarios_data <- list(
  # SC0 = inp0,
  # SC1 = inp1,
  # SC2 = inp2,
  # SC3 = inp3,
  # SC4 = inp4,
  #SC5 = inp5,
  SC6 = inp6,
  SC7 = inp7,
  #SC8 = inp8,
  SC9 = inp9
)
scenarios_priors <- list(
  # RUN1 = priors_run1,
  # RUN2 = priors_run2,
  # RUN3 = priors_run3,
  # RUN4 = priors_run4,
  RUN5 = priors_run5,
  RUN6 = priors_run6,
  RUN7 = priors_run7,
  RUN8 = priors_run8
)

# if we read rds
scenarios_data<-readRDS("data/scenarios_data.rds")
scenarios_priors<-readRDS("data/scenarios_priors.rds")
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
names(results_by_scenario$SC5)

# Individual Scenario and run (e.i.)
# SC5
results_by_scenario$SC5$RUN5
results_by_scenario$SC5$RUN6
results_by_scenario$SC5$RUN7
results_by_scenario$SC5$RUN8
results_by_scenario$SC5$RUN9
results_by_scenario$SC5$RUN10
results_by_scenario$SC5$RUN11
results_by_scenario$SC5$RUN12

# SC6
results_by_scenario$SC6$RUN5
results_by_scenario$SC6$RUN6
results_by_scenario$SC6$RUN7
results_by_scenario$SC6$RUN8

# SC7
results_by_scenario$SC7$RUN5
results_by_scenario$SC7$RUN6
results_by_scenario$SC7$RUN7
results_by_scenario$SC7$RUN8

# SC8
results_by_scenario$SC8$RUN5
results_by_scenario$SC8$RUN6
results_by_scenario$SC8$RUN7
results_by_scenario$SC8$RUN8

# SC9
results_by_scenario$SC9$RUN5
results_by_scenario$SC9$RUN6
results_by_scenario$SC9$RUN7
results_by_scenario$SC9$RUN8


# genera  un .rds por escenario

#saveRDS(results_by_scenario, "outputs/SPiCT_full_results.rds")

# out <- list(
#   results_by_scenario$SC8$RUN5,
#   results_by_scenario$SC9$RUN5
# )
### POSIBLE CORTE DE CODE ####

#------- RESULTS-----------
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


# leer outputs por escenaruio
osa_results$SC9$RUN6


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

scenarios <- paste0("SC", 5:9)
runs <- paste0("RUN", 5:8)

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
# por cada retro_result, haz un plotsipct.retro.fixed() plot y guardalo en  la csarpeta de fig de retro
fig_dir <- file.path("retro", "figs")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

for (sc in names(retro_results)) {

  for (run in names(retro_results[[sc]])) {

    retro_obj <- retro_results[[sc]][[run]]

    # nombre del archivo
    file_out <- file.path(
      fig_dir,
      paste0("SIPCT_retro_", sc, "_", run, ".pdf")
    )

    pdf(file_out, width = 7, height = 7)
    plotsipct.retro.fixed(retro_obj)
    dev.off()
  }
}






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
  file = "outputs/retro/mohns_rho_by_scenario_5_9.csv",
  row.names = FALSE
)

## ---------------------------Get AIC----------------------------------

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
        arrange(AIC)
    )
  }
}

aic_table

write.csv(
  aic_table,
  file = "outputs/AIC_by_scenario_run_5_8.csv",
  row.names = FALSE
)

## --------------------------Get BB and F vector---------------------------------------

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
# Comparison plot SC5 to SC9
png(
  filename = "figs/SPiCT_Comparison_SC5_all_RUN.png",
  width = 2800,
  height = 2200,
  res = 300
)

plotspict.compare(
  list(
    "RUN5"       = results_by_scenario$SC5$RUN5,
    "RUN6"       = results_by_scenario$SC5$RUN6,
    "RUN7"       = results_by_scenario$SC5$RUN7,
    "RUN8"       = results_by_scenario$SC5$RUN8
  ),
  varname = c("B", "F", "C", "P"),
  CI = 0.1
)

dev.off()

png(
  filename = "figs/SPiCT_Comparison_SC6_all_RUN.png",
  width = 2800,
  height = 2200,
  res = 300
)

plotspict.compare(
  list(
    "RUN5"       = results_by_scenario$SC6$RUN5,
    "RUN6"       = results_by_scenario$SC6$RUN6,
    "RUN7"       = results_by_scenario$SC6$RUN7,
    "RUN8"       = results_by_scenario$SC6$RUN8
  ),
  varname = c("B", "F", "C", "P"),
  CI = 0.1
)

dev.off()
png(
  filename = "figs/SPiCT_Comparison_SC7_all_RUN.png",
  width = 2800,
  height = 2200,
  res = 300
)

plotspict.compare(
  list(
    "RUN5"       = results_by_scenario$SC7$RUN5,
    "RUN6"       = results_by_scenario$SC7$RUN6,
    "RUN7"       = results_by_scenario$SC7$RUN7,
    "RUN8"       = results_by_scenario$SC7$RUN8
  ),
  varname = c("B", "F", "C", "P"),
  CI = 0.1
)

dev.off()
png(
  filename = "figs/SPiCT_Comparison_SC8_all_RUN.png",
  width = 2800,
  height = 2200,
  res = 300
)

plotspict.compare(
  list(
    "RUN5"       = results_by_scenario$SC8$RUN5,
    "RUN6"       = results_by_scenario$SC8$RUN6,
    "RUN7"       = results_by_scenario$SC8$RUN7,
    "RUN8"       = results_by_scenario$SC8$RUN8
  ),
  varname = c("B", "F", "C", "P"),
  CI = 0.1
)

dev.off()
png(
  filename = "figs/SPiCT_Comparison_SC9_all_RUN.png",
  width = 2800,
  height = 2200,
  res = 300
)

plotspict.compare(
  list(
    "RUN5"       = results_by_scenario$SC9$RUN5,
    "RUN6"       = results_by_scenario$SC9$RUN6,
    "RUN7"       = results_by_scenario$SC9$RUN7,
    "RUN8"       = results_by_scenario$SC9$RUN8
  ),
  varname = c("B", "F", "C", "P"),
  CI = 0.1
)

dev.off()

## ----------------------- BRPs ---------------------------------------

# funcion para sacar los BRP
get_spict_report <- function(fit) {

  if (is.null(fit$report)) {
    return(tibble(Bmsy = NA, Fmsy = NA, MSY = NA))
  }

  tibble(
    Bmsy = fit$report$Bmsy[1],
    Fmsy = fit$report$Fmsy[1],
    MSY  = fit$report$MSY[1]
  )
}


srp_table <- imap_dfr(results_by_scenario, function(scen_list, scen_name) {

  imap_dfr(scen_list, function(run_fit, run_name) {

    rp <- get_spict_report(run_fit)

    tibble(
      Scenario = scen_name,
      Run      = run_name,
      Bmsy     = rp$Bmsy,
      Fmsy     = rp$Fmsy,
      MSY      = rp$MSY
    )
  })
})

srp_table_clean <- srp_table %>%
  mutate(
    Bmsy = ifelse(Bmsy <= 0 | abs(Bmsy) > 1e12, NA, Bmsy),
    Fmsy = ifelse(Fmsy <= 0 | abs(Fmsy) > 10, NA, Fmsy),
    MSY  = ifelse(MSY  <= 0 | abs(MSY)  > 1e6, NA, MSY)
  )

srp_table_final <- srp_table_clean %>%
  mutate(across(c(Bmsy, Fmsy, MSY), ~ round(.x, 3)))

write.csv(
  srp_table_final,
  file = "outputs/brps_table_final.csv",
  row.names = FALSE
)

# Un plot Simple

srp_long <- srp_table_final %>%
  pivot_longer(
    cols = c(Bmsy, Fmsy, MSY),
    names_to = "Variable",
    values_to = "Value"
  )
p <- ggplot(srp_long,
            aes(x = Scenario,
                y = Value,
                colour = Run)) +

  geom_point(size = 3,
             position = position_dodge(width = 0.6),
             na.rm = TRUE) +

  facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(option = "H")+
  labs(
    title = "",
    x = "Scenario",
    y = "Estimated value",
    colour = "Prior (Set)"
  ) +

  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey90"),
    panel.grid.minor = element_blank()
  )

p

ggsave(
  filename = "figs/SPiCT_BRP_scenario_5_8_RUN_comparison.png",
  plot = p,
  width = 6,
  height = 10,
  dpi = 300
)

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



## ----Final Tables Values -----

get_conv <- function(x) {
  if (is.null(x)) return(NA)
  if (!is.null(x$opt$convergence)) x$opt$convergence == 0 else NA
}

get_pdhess <- function(x) {
  if (is.null(x)) return(NA)
  x$pdHess %||% NA
}

get_aic <- function(x) {
  if (is.null(x)) return(NA)
  tryCatch(get.AIC(x), error = function(e) NA)
}
get_biomass_2025 <- function(x) {
  if (is.null(x)) return(NA_real_)

  b <- exp(as.data.frame(get.par("logB", x)))
  b$year <- round(as.numeric(rownames(b)), 0)

  b_2025 <- b %>% filter(year == 2025)
  if (nrow(b_2025) == 0) return(NA_real_)

  sum(b_2025$est, na.rm = TRUE)
}

get_fishingmortality_2025 <- function(x) {
  if (is.null(x)) return(NA_real_)

  f <- exp(as.data.frame(get.par("logF", x)))
  f$year <- round(as.numeric(rownames(f)), 0)

  f_2025 <- f %>% filter(year == 2025)
  if (nrow(f_2025) == 0) return(NA_real_)

  mean(f_2025$est, na.rm = TRUE)
}

get_Bmsy_prodcurve <- function(fit) {
  if (is.null(fit)) return(NA_real_)
    out <- tryCatch(
    calc.bmsyk(fit),
    error = function(e) NA
  )
    # si falla o es NA
  if (length(out) == 0 || all(is.na(out))) return(NA_real_)
    # caso 1: escalar numérico
  if (is.atomic(out) && length(out) == 1) {
    return(as.numeric(out))
  }
    # caso 2: vector con nombre Bmsy
  if (is.atomic(out) && "Bmsy" %in% names(out)) {
    return(as.numeric(out["Bmsy"]))
  }

  # caso 3: lista
  if (is.list(out) && "Bmsy" %in% names(out)) {
    return(as.numeric(out$Bmsy))
  }
  NA_real_
}


# Create summary table


summary_table <- data.frame()

for (sc in names(results_by_scenario)) {
  for (rn in names(results_by_scenario[[sc]])) {

    fit <- results_by_scenario[[sc]][[rn]]

    summary_table <- rbind(
      summary_table,
      data.frame(
        Scenario = sc,
        Run = rn,
        Convergence = get_conv(fit),
        PDHess = get_pdhess(fit),
        AIC = get_aic(fit),
        Biomass_2025 = get_biomass_2025(fit),
        FishingMortality_2025 = get_fishingmortality_2025(fit),
        ProdCurve = get_Bmsy_prodcurve(fit)
      )
    )
  }
}


summary_table <- summary_table %>%
  left_join(
    mohn_table,
    by = c("Scenario", "Run")
  )

# Save summary table
write.csv(
  summary_table,
  "outputs/SPiCT_summary_table_all_scenarios_runs.csv",
  row.names = FALSE
)


## ----Final Tables Values -----

get_conv <- function(x) {
  if (is.null(x)) return(NA)
  if (!is.null(x$opt$convergence)) x$opt$convergence == 0 else NA
}

get_pdhess <- function(x) {
  if (is.null(x)) return(NA)
  x$pdHess %||% NA
}

get_aic <- function(x) {
  if (is.null(x)) return(NA)
  tryCatch(get.AIC(x), error = function(e) NA)
}
get_biomass_2025 <- function(x) {
  if (is.null(x)) return(NA_real_)

  b <- exp(as.data.frame(get.par("logB", x)))
  b$year <- round(as.numeric(rownames(b)), 0)

  b_2025 <- b %>% filter(year == 2025)
  if (nrow(b_2025) == 0) return(NA_real_)

  sum(b_2025$est, na.rm = TRUE)
}

get_fishingmortality_2025 <- function(x) {
  if (is.null(x)) return(NA_real_)

  f <- exp(as.data.frame(get.par("logF", x)))
  f$year <- round(as.numeric(rownames(f)), 0)

  f_2025 <- f %>% filter(year == 2025)
  if (nrow(f_2025) == 0) return(NA_real_)

  mean(f_2025$est, na.rm = TRUE)
}

get_Bmsy_prodcurve <- function(fit) {
  if (is.null(fit)) return(NA_real_)
    out <- tryCatch(
    calc.bmsyk(fit),
    error = function(e) NA
  )
    # si falla o es NA
  if (length(out) == 0 || all(is.na(out))) return(NA_real_)
    # caso 1: escalar numérico
  if (is.atomic(out) && length(out) == 1) {
    return(as.numeric(out))
  }
    # caso 2: vector con nombre Bmsy
  if (is.atomic(out) && "Bmsy" %in% names(out)) {
    return(as.numeric(out["Bmsy"]))
  }

  # caso 3: lista
  if (is.list(out) && "Bmsy" %in% names(out)) {
    return(as.numeric(out$Bmsy))
  }
  NA_real_
}


# Create summary table


summary_table <- data.frame()

for (sc in names(results_by_scenario)) {
  for (rn in names(results_by_scenario[[sc]])) {

    fit <- results_by_scenario[[sc]][[rn]]

    summary_table <- rbind(
      summary_table,
      data.frame(
        Scenario = sc,
        Run = rn,
        Convergence = get_conv(fit),
        PDHess = get_pdhess(fit),
        AIC = get_aic(fit),
        Biomass_2025 = get_biomass_2025(fit),
        FishingMortality_2025 = get_fishingmortality_2025(fit),
        ProdCurve = get_Bmsy_prodcurve(fit)
      )
    )
  }
}


summary_table <- summary_table %>%
  left_join(
    mohn_table,
    by = c("Scenario", "Run")
  )

# Save summary table
write.csv(
  summary_table,
  "outputs/SPiCT_summary_table_all_scenarios_runs.csv",
  row.names = FALSE
)

## --------------Example single scenario run ------------------------

# Exampl with just 1 Scenario

# Scenario: Landings + ISUNEPCA UWTV abundance + standardized LPUE (SC2)
inp_sc2 <- list(
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  timeI = list(
    I_isunep_bio_nor$timeI[ind:ind2],
    I_arsa_rendi_std_kgh$timeI[7:26],
    I_LPUE_std$timeI[23:38]
  ),

  obsI = list(
    I_isunep_bio_nor$obsI[ind:ind2],
    I_arsa_rendi_std_kgh$obsI[7:26],
    I_LPUE_std$obsI[23:38]
  )
)


inp_sc2$priors$logbkfrac <- c(log(0.5), 0.2, 1)
inp_sc2$priors$logn <- c(log(2),   0.5, 1)
inp_sc2$priors$logr <- c(log(0.2), 0.2, 1)
inp_sc2$priors$logsdi = list(
      c(log(0.1), 0.1, 1),            # prior for index 1 (CV ≈ 0.1)
      c(log(0.1), 0.2, 1),            # prior for index 2 (CV ≈ 0.2)
      c(log(0.1), 0.2, 1) # prior for index 2 (CV ≈ 0.2)
    )


# Run SPiCT for SC2 with default priors
fit_sc2 <- fit.spict(
  inp = check.inp(inp_sc2),
  verbose = TRUE
)
# Run OSA diagnostics
# for fit_sc2
res <- calc.osa.resid(fit_sc2)
plotspict.diagnostic(res)
# Run SPiCT plot

plot(fit_sc2, CI = 0.8)
# Run retrospective
retro_sc2 <- retro(fit_sc2, nretroyear=5)
plotspict.retro(retro_sc2)
# Extract Mohn's rho
mohn_sc2 <- extract_mohn_spict(retro_sc2)
# Extract FFMSY and BBMSY
kobebro_sc2 <- extract_FF_BB_MSY(fit_sc2)
# Run hindcast
hindcast_sc2 <- hindcast(fit_sc2)
# plot hindcast
plotspict.hindcast(hindcast_sc2)
# Run management
manage_sc2 <- manage(fit_sc2)
