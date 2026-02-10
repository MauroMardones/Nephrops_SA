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

# fig.path <- here("figs")
# lapply(c("figs", "outputs"),
#        function(x) if(!dir.exists(x))
#          dir.create(x, recursive = TRUE))



## -------- Read Data--------------
# Data actualizada
bac <- read_csv(here("data",
                     "inputdata_FU30_wkbmsyspict.csv"))
# mean

# bac_means <- bac %>%
#   summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

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
  obsI  = bac$LPUE_std_DCP,
  timeI = bac$year
)

# Indices time range

ind  <- which(C_nep$timeC == 1987)
ind2 <- which(C_nep$timeC == 2025)


## ---------------Scenarios for SPiCT model-------------


#Scenario 9 -- Scenario combines total landings with the ISUNEPCA UWTV abundance (2015–2025),
#ARSA biomass survey (1993–2012),
#and the standardised commercial LPUE (2009–2024).
# Scenario combines total landings with the ISUNEPCA UWTV biomass index,
# ARSA yield, and the standardised commercial LPUE, integrating long-term
# and recent information while ensuring consistency among data sources
# aca elijes los indices y datoas a usar

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


#### Priors for Scenario 2 ####

inp_sc2$priors$logbkfrac <- c(log(0.5), 0.2, 1)
inp_sc2$priors$logn <- c(log(2),   0.5, 1)
inp_sc2$priors$logr <- c(log(0.2), 0.2, 1)
inp_sc2$priors$logsdi = list(
  c(log(0.1), 0.1, 1),            # prior for index 1 (CV ≈ 0.1)
  c(log(0.1), 0.2, 1),            # prior for index 2 (CV ≈ 0.2)
  c(log(0.1), 0.2, 1) # prior for index 2 (CV ≈ 0.2)
)


# ---------------------------  Run SPiCT for SC2 with default priors---------
spict_obj <- fit.spict(
  inp = check.inp(inp_sc2),
  verbose = TRUE)


## --------------------Read Scenarios----------------------------------------------

# Example access to results

names(spict_obj)

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

## -------------------Plot initaial default----------------------------------------------


osa_run <- plot(spict_obj, CI = 0.2)



##-----------------------------Results Tables ------------------------------

# Create base results folder
dir.create("outputs/results", showWarnings = FALSE)

    # Create folder results/SCx/RUNy
    out_dir <- file.path("outputs/results")
    #
    # Tables SPiCT parameters
    #

    # Summary of estimates
    write.csv(
      round(sumspict.parest(spict_obj), 2),
      file = file.path(out_dir, "SummaryEstimates.csv")
    )

    # Reference points (stochastic)
    write.csv(
      round(sumspict.srefpoints(spict_obj), 2),
      file = file.path(out_dir, "RefPoints.csv")
    )

    # States
    write.csv(
      round(sumspict.states(spict_obj), 2),
      file = file.path(out_dir, "States.csv")
    )

    # Predictions
    write.csv(
      round(sumspict.predictions(spict_obj), 2),
      file = file.path(out_dir, "Predictions.csv")
    )



## ---------------------------Retros----------------------------------------
##
# Correr análisis retrospectivo (retro)
#
# Generar y guardar los plots retrospectivos
#
# Guardar los objetos de resultados
#
# Iterar automáticamente por escenario y run

retrosct <- retro(spict_obj,
                           nretroyear = 5,
                           mc.cores = 1)
# plot
plotspict.retro(retrosct)


## ---------------------------Get AIC----------------------------------

# Because AIC depends on the likelihood associated with a
# given data configuration, its values are not directly comparable
# across models that differ in the number or type of input data.
# Therefore, AIC-based comparisons were restricted to models within the same scenario.

# Tabla de AIC
aic_table <- data.frame(
  AIC = tryCatch(
    get.AIC(spict_obj),
    error = function(e) NA_real_
  )
)


##  ----Hindcast MASE calculation ----

sc0r1h <- hindcast(spict_obj)
plotspict.hindcast(sc0r1h)





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
fit <- manage(spict_obj)
sumspict.manage(fit)
# example plot
plotspict.hcr(fit)

# ---- Apply HCRs and save results by scenario ----

# Create base folder
dir.create("outputs/HCR", recursive = TRUE, showWarnings = FALSE)

      # 1. Base fitted model
      base_fit <- spict_obj

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
      out_dir <- file.path("results", "HCR")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      # 5. Write CSV per SC × RUN
      write.csv(
        res,
        file = file.path(out_dir, paste0("HCR_", sc, "_", rn, ".csv")),
        row.names = TRUE
      )


###----- Plots HCR ------------------
## problemas con este loop para hacer los plots de HCR!!!
dir.create("figs/hcr", recursive = TRUE, showWarnings = FALSE)


      # 1. Base fitted model
      fit <- spict_obj

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
      out_dir <- file.path("figs", "hcr")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      # 5. Save plot
      png(
        filename = file.path(out_dir, paste0("HCR_", ".png")),
        width = 2400,
        height = 2000,
        res = 300
      )

      plotspict.hcr(fit)
      dev.off()




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

