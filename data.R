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
# lapply(c("figs", "outputs"),
#        function(x) if(!dir.exists(x))
#          dir.create(x, recursive = TRUE))



## -------- Read Data--------------
# Data actualizada
bac <- read_csv(here("data",
                     "inputdata_FU30_wkbmsyspict.csv"))


## --------- Preparing Data for Spict--------------

data <- bac

# Create Catch dataframe
C_nep <- data.frame(
  obsC = data$catch,
  timeC = data$year
)

# Create abundance index dataframes

# ARSA biomass index (otoño)
I_arsa_bio <- data.frame(
  obsI  = bac$arsabio,
  timeI = bac$year + 0.75
)

# ARSA productivity index (raw) (otoño)
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
    I_LPUE_std$timeI[ind:ind2]            # Índice 3
  ),

  obsI = list(
    I_isunep_abun$obsI[ind:ind2],
    I_arsa_rendi_std_kgh$obsI[7:26], #1993:2012
    I_LPUE_std$obsI[ind:ind2]
  )
)

# 1. Inicializar stdevfacI con 1s para todos los índices
inp9$stdevfacI <- list(
  rep(1, length(inp9$obsI[[1]])),
  rep(1, length(inp9$obsI[[2]])),
  rep(1, length(inp9$obsI[[3]]))
)

# 2. Aplicar incertidumbre extra a ARSA (Índice 2)
# Buscamos la posición de los años 2020 y 2024 dentro del vector de tiempo de ARSA
años_con_incertidumbre <- c(2020, 2024)
idx_arsa <- which(inp9$timeI[[2]] %in% años_con_incertidumbre)

# Asignamos un factor (ejemplo: 10 para indicar que el dato es casi irrelevante)
inp9$stdevfacI[[2]][idx_arsa] <- 10



# must be set before check.inp
inp_list <- list(
  SC5 = inp5,
  SC6 = inp6,
  SC7 = inp7,
  SC8 = inp8,
  SC9 = inp9
)


# Collect runs
# Grouped Scenarios and Priors
# here we define wich combinations of scenarios and priors we want to run

scenarios_data <- list(
  SC5 = inp5,
  SC6 = inp6,
  SC7 = inp7,
  SC8 = inp8,
  SC9 = inp9
)

#guardar Rdata
 saveRDS(scenarios_data,  file = "data/scenarios_data.rds")
#------------------------------------------------------
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
#------------------------------------------------------
