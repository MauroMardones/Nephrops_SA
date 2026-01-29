#rm(list = ls())

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


# read this object if anytime you changes  scenarios data or scenarios priors
scenarios_data<-readRDS("data/scenarios_data.rds")
scenarios_priors<-readRDS("data/scenarios_priors.rds")

### Seleccionar Escenarios que se quiere comparar

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
results_by_scenario$SC6$RUN9
results_by_scenario$SC6$RUN10
results_by_scenario$SC6$RUN11
results_by_scenario$SC6$RUN12


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


# genera  un .rds por escenario example
#saveRDS(results_by_scenario$SC5$RUN(), "outputs/SPiCT_full_results_SC5_R8.rds")
#o todo el objeto con los resultados
#saveRDS(results_by_scenario, "outputs/SPiCT_full_results.rds")


