## ----setup, include=FALSE, message=F---------------------------------------------
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


## ----eval=FALSE, echo=TRUE-------------------------------------------------------
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


## ----echo = TRUE, message = FALSE------------------------------------------------
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



# Por ahora no tengo Effort ni LPUE standar
## --------------------------------------------------------------------------------
bac <- read_excel(here("data",
                           "inputdata_spict_fu30_2025_Rev.xlsx")) %>%
  mutate(
    Effort = if_else(is.na(catch), NA_real_, 1),
    LPUE_std = catch / Effort
  )



## --------------------------------------------------------------------------------
bac_long <- bac %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")
# ---- Gráfico de capturas (barras) ----
p_catch <- ggplot(filter(bac_long, variable == "catch"),
                  aes(x = year, y = value)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = mean(bac$catch, na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(y = "Landings (tons)", x = "", title = "Landings Nephrops time series")


# ---- Gráficos para los índices (puntos + suavizado) ----
plot_index <- function(var_name, ylab){
  ggplot(filter(bac_long, variable == var_name),
         aes(x = year, y = value)) +
    geom_point(color = "darkred", size = 2) +
    geom_smooth(color = "darkred",
                se = TRUE,
                method = "loess",
                formula = y ~ x,
                linewidth = 0.8) +
    theme_minimal(base_size = 12) +
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

# ---- Combinar todos los gráficos ----
# y guuardar en "figs"
fig_indices <- ggarrange(
  p_catch,
  ggarrange(
    p1, p2, p3, p4, p5, p6, p7, p8,
    ncol = 2,
    nrow = 4,
    labels = "AUTO"
  )
)

ggsave(
  filename = file.path(fig.path, "indices_nephrops_FU30.png"),
  plot = fig_indices,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 300
)

###

# Correlation

## ----warning=FALSE, message=FALSE------------------------------------------------
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


# Preparing Data for Splitc

## --------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------
ind  <- which(C_nep$timeC == 1994)
ind2 <- which(C_nep$timeC == 2025)


## Scenarios for SPiCT model

## ----------
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


## --------------------------------------------------------------------------------
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



## --------------------------------------------------------------------------------
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



## --------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------
# Scenario 5 -- All Indices
inp5 <- list(
  # ---- Capturas ----
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

## --------------------------------------------------------------------------------
inp1$dteuler <- 1/16  # must be set before check.inp
inp1 <- check.inp(inp1)

# Inspect
inp1$dtc


## PRIORS AND INITIAL VALUES buy Scenario regarding last WG

# ------------------------------
# Priors configurations
# ------------------------------

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
# Now spict_scenarios contains all combinations of data scenarios and prior configurations

## --------------------------------------------------------------------------------
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

# Example access to results

# Qué escenarios se corrieron
names(results_by_scenario)

# Qué RUNs tiene un escenario
names(results_by_scenario$SC1)

# Ver un modelo concreto
results_by_scenario$SC0$RUN4

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


##  --------------------------------------------------------------------------------


## Plots




# # Convert model to Schaefer (set exponent n = 1)
# inp1$ini$logn <- log(2)
# inp1$phases$logn <- -1  # Fix it

# # ---- Fox model ----
# inp1$ini$logn <- log(1.01)  # n ~ 0, modelo tipo Fox
# inp1$phases$logn <- -1      # fijar el exponente

#
# # ---- Pella–Tomlinson (n = 2) ----
inp1$ini$logn <- log(3)   # da n = 2 en la parametrización interna
inp1$phases$logn <- -1    # fijar el exponente (no estimarlo)


## --------------------------------------------------------------------------------
# Priors list
list.possible.priors()


## --------------------------------------------------------------------------------
inp1$priors <- list()

inp1$priors$logalpha <- c(1, 1, 0)
inp1$priors$logbeta <- c(1, 1, 0)
# --- Population dynamics priors ---

# Intrinsic growth rate
inp1$priors$logr <- c(log(0.8), 0.1, 0.3)      # Moderate productivity

# Biomass relative to Bmsy
inp1$priors$logBBmsy <- c(log(0.4), 0.3, 1, 1994)
#Biomasa inicial igual a Bmsy. Priorizas estado de explotación neutral.


# Exploitation prior -> high exploitation (low B0/K)
inp1$priors$logbkfrac <- c(log(0.2), 0.5, 1)   # High exploitation (B0 ≈ 30% K)

# o fijar
# # Valor inicial (en escala log)
# inp1$ini$logbkfrac <- log(0.2)
# # Fase negativa => no se estima (parámetro fijo)
# inp1$phases$logp1robfac <- -1

# Fishing mortality relative to Fmsy
inp1$priors$logFFmsy <- c(log(1.2), 0.4, 1, 1994)
#F= Fmsy al inicio de la serie. Priorizas estado de explotación neutral.

# --- Catchability priors (logq) ---
# Surveys ≈ 1 (neutral scaling)
# CPUE commercial ≈ 0.01 (much smaller)

# inp1$priors$logq <- list(
#   I_arsa_spring  = c(log(1),   0.9, 1.2),   # scientific survey
#   I_arsa_autumn  = c(log(1),   0.9, 1.2),   # scientific survey
#   I_isunepbio    = c(log(1),   0.9, 1.2),   # scientific survey
#   I_isunepabun   = c(log(1),   0.9, 1.2), # commercial CPUE, much lower q
#   I_rendiarsaspr = c(log(1),   0.9, 1.2),   # scientific survey
#   I_LPUEcom     = c(log(0.0005), 0, 0.01)    # commercial CPUE, much lower q
# )


#prior solo con INSUPEC Bio,  Arsa Spring y LPUE Comercial

# inp1$priors$logq <- list(
#   I_arsa_spring  = c(log(1),   0.9, 1.2),   # scientific survey
#
#   I_isunepbio    = c(log(1),   0.9, 1.2),   # scientific survey
#   I_LPUEcom      = c(log(0.0005), 0, 0.01)    # commercial CPUE, much lower q
# )

# Prior solo para INSUPEC Bio y Arsa Spring
#
# inp1$priors$logq <- list(
#   I_arsa_spring  = c(log(5),   0.9, 1.2),   # scientific survey
#
#   I_isunepbio    = c(log(5),   0.9, 1.2)   # scientific survey
#
# )

# # -- Fijar parámetros: colocar valor inicial (ini) y fase = -1 (no estimar) --
#
# # Intrinsic growth rate (r)
# inp1$ini$logr    <- log(0.8)
# inp1$phases$logr <- -1
#
# # Biomass relative to Bmsy (B0/Bmsy)
# inp1$ini$logBBmsy    <- log(0.4)
# inp1$phases$logBBmsy <- -1
#
# # B0/K (bkfrac)
# inp1$ini$logbkfrac    <- log(0.2)
# inp1$phases$logbkfrac <- -1
#
# # Fishing mortality relative to Fmsy (F0/Fmsy)
# inp1$ini$logFFmsy    <- log(1.2)
# inp1$phases$logFFmsy <- -1

# (Opcional) Si tu estructura de objetos usa otra rama para fases/ini, ajusta los nombres en consecuencia.


# Check input
inp1 <- check.inp(inp1)


## --------------------------------------------------------------------------------
res1 <- fit.spict(inp1)


## --------------------------------------------------------------------------------
capture.output(summary(res1))


## ----figwidth=8, fig.height=9----------------------------------------------------
plot(res1,CI = 0.8)

a <-plotspict.bbmsy(res1)
b <-  plotspict.ffmsy(res1)
b<- plotspict.biomass(res1, qlegend = T, ylim=c(0, 15000))
p <- plotspict.production(res1, n.plotyears = 60)
c <- plotspict.f(res1, qlegend = F, rel.axes = F)
d <- plotspict.fb(res1)
f <- plotspict.ffmsy(res1, qlegend=T)
g <- plotspict.catch(res1, qlegend=FALSE, ylim=c(0, 1000))
check.ini(res1, ntrials=4)


## ----fig.width=7, fig.height=6---------------------------------------------------

all(is.finite(res1$sd))


## --------------------------------------------------------------------------------
calc.bmsyk(res1)
plotspict.production(res1)


## ----fig.width=9, fig.height=7---------------------------------------------------
resbacres <- calc.osa.resid(res1)
plotspict.diagnostic(resbacres, qlegend = F)


## --------------------------------------------------------------------------------
resretro <- retro(res1,
                  nretroyear = 4,
                  mc.cores = 1)
plotspict.retro(resretro)


## --------------------------------------------------------------------------------
plotspict.retro.fixed(resretro)


## --------------------------------------------------------------------------------
get.AIC(res1)


## --------------------------------------------------------------------------------
bioest <- exp(as.data.frame(get.par('logB',
                                    res1)))
year <- round(as.numeric(rownames(bioest)),0)
bioest <- cbind(bioest, year)
BIOEST <- bioest %>%
  group_by(year) %>%
  summarise(estimado=mean(est))%>%
  rename(BIOEST=estimado)


## --------------------------------------------------------------------------------
fest <- exp(as.data.frame(get.par('logF',
                                  res1)))
year <- round(as.numeric(rownames(fest)),0)
fest <- cbind(fest, year)
FEST <- fest %>%
  group_by(year) %>%
  summarise(estimado=mean(est))%>%
  rename(FEST=estimado)
# uno
estimadosFB <- cbind(BIOEST, FEST[,2])


## ----eval=FALSE------------------------------------------------------------------
# write_csv(estimadosFB, "estimadoSPICT.csv")


## --------------------------------------------------------------------------------
ffmsy <- exp(as.data.frame(get.par('logFFmsy', res1)))
year <- round(as.numeric(rownames(ffmsy)),0)
ffmsy <- cbind(ffmsy, year)
FFMSY <- ffmsy %>%
  group_by(year) %>%
  summarise(estimado=mean(est))%>%
  rename(FFMSY=estimado)


## --------------------------------------------------------------------------------
bbmsy <- exp(as.data.frame(get.par('logBBmsy', res1)))
year <- round(as.numeric(rownames(bbmsy)),0)
bbmsy <- cbind(bbmsy, year)
BBMSY <- bbmsy %>%
  group_by(year) %>%
  summarise(estimado=mean(est))%>%
  rename(BBMSY=estimado)


## --------------------------------------------------------------------------------
kobebro <- cbind(FFMSY, BBMSY[,2])
#write.csv(kobebro, "kobebro.csv", sep=",")


## --------------------------------------------------------------------------------
texto_coords <- data.frame(
  x = c(0.2, 1, 2.5, 2.3),   # Coordenadas x para los textos
  y = c(2, 2.5, 2.5, 0.2),   # Coordenadas y para los textos
  etiqueta = c("Agotada", "Plena Explotación", "Sobrexplotación", "Subexplotación")  # Textos que se agregarán
)


## ----fig.height=6, fig.width=6---------------------------------------------------
ggplot()+
  geom_point(aes(kobebro$BBMSY, kobebro$FFMSY),
            lwd=2) +
  #geom_line(aes(kobebro$BBMSY, kobebro$FFMSY)) +
  geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 3),
            fill = "#E43338", alpha = 0.5) +
  geom_rect(aes(xmin = 0.5, xmax = 0.75, ymin = 0, ymax = 3),
            fill = "#F2ED23", alpha = 0.5) +
  geom_rect(aes(xmin = 0.75, xmax = 1.25, ymin = 0, ymax = 3),
            fill = "#ACC39A", alpha = 0.5) +
  geom_rect(aes(xmin = 1.25, xmax = 3.5, ymin = 0, ymax = 1),
            fill = "#608D68", alpha = 0.5) +
  geom_rect(aes(xmin = 1.25, xmax = 3.5, ymin = 1, ymax = 3),
            fill = "#808080", alpha = 0.5) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = c(0.5, 0.75, 1.75, 1, 1.25), linetype=2)+
  theme_few()+
  labs(x = expression("BD/BD"[RMS]), y = expression("F/F"[RMS]))+
  # geom_text(data = texto_coords, aes(x = x, y = y, label = etiqueta),
  #            vjust = -0.5)+
  geom_text(aes(x=kobebro$BBMSY,y=kobebro$FFMSY,label=kobebro$year),
             nudge_y = 0.1,size = 3,
               check_overlap = TRUE)


## ----eval=FALSE------------------------------------------------------------------
# #cross correlation between cpue and catch in schaef Fig 7.2
# parset(cex=0.85) #sets par parameters for a tidy base graphic
# ccf(x=bacalaodata$obsC[20:38],y=bacalaodata$obsI[20:38],type="correlation",
#     ylab="Correlation",plot=TRUE)


## --------------------------------------------------------------------------------
cov2cor(res1$cov.fixed)


## --------------------------------------------------------------------------------
cov2cor(get.cov(resbac1, 'logBmsy', 'logFmsy'))


## ----warning=FALSE, message=FALSE------------------------------------------------
resbro2 <- manage(resretro)


## --------------------------------------------------------------------------------
mohns_rho(resbro2, what = c("FFmsy", "BBmsy"))


## --------------------------------------------------------------------------------
sumspict.manage(resbro2)


## --------------------------------------------------------------------------------
plotspict.bbmsy(resbro2, qlegend = F, ylim=c(0, 20))
plotspict.ffmsy(resbro2, qlegend = F)
plotspict.catch(resbro2, qlegend = F)
plotspict.fb(resbro2)


## ----warning=FALSE, message=F----------------------------------------------------
plotspict.hcr(resbro2, CI=0.95)


## ----fig.width=5, fig.height=4---------------------------------------------------
xsf <-rnorm(1000, mean = 1356.5 , sd = 387)
xs1f  <-seq(min(xsf),max(xsf),5)
ysf  <-dnorm(xs1f,  mean = 1356.5 , sd = 387)
qsf <- as.data.frame(qnorm(c(0.1, 0.2, 0.3, 0.4,  0.5),
                          mean = 1356.5 , sd = 387))
cbabac <- ggplot()+
  geom_line(aes(xs1f, ysf))+
  geom_vline(xintercept=qsf[1,1], col="#999999")+
  geom_vline(xintercept=qsf[2,1], col="#E69F00")+
  geom_vline(xintercept=qsf[3,1], col="#009E73")+
  geom_vline(xintercept=qsf[4,1], col="#56B4E9")+
  geom_vline(xintercept=qsf[5,1], col="#e66101")+
   geom_text(aes(x=qsf[1,1]+50,
                label=round(qsf[1,1], 1),y=0.00040),
             colour="#999999", angle=90)+
   geom_text(aes(x=qsf[2,1]+50,
                label=round(qsf[2,1], 1),y=0.00040),
             colour="#E69F00", angle=90)+
   geom_text(aes(x=qsf[3,1]+50,
                label=round(qsf[3,1], 1),y=0.00040),
             colour="#009E73", angle=90)+
   geom_text(aes(x=qsf[4,1]+50,
                label=round(qsf[4,1], 1),y=0.00040),
             colour="#56B4E9", angle=90)+
   geom_text(aes(x=qsf[5,1]+50,
                label=round(qsf[5,1], 1),y=0.00040),
             colour="#e66101", angle=90)+
  ylab('')+
  xlab('')+
  ggtitle("")+
  theme_few()
cbabac

