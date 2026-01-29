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
  "outputs/SPiCT_BB_F_SC5_SC9_scenarios.csv",
  row.names = FALSE
)


## ------------ plots comparision --------------

#He sacado algunos priors que no convergen o q  generan
#problemas con los ejes daddo que son magnitudes inverosimiles

# compare with spict compare function
png(
  filename = "figs/SPiCT_Comparison_SC5_all_RUN.png",
  width = 2800,
  height = 2200,
  res = 300
)
plotspict.compare(
  list(
    # "RUN5"       = results_by_scenario$SC5$RUN5,
    # "RUN6"       = results_by_scenario$SC5$RUN6,
    # "RUN7"       = results_by_scenario$SC5$RUN7,
    "RUN8"       = results_by_scenario$SC5$RUN8,
    "RUN9"       = results_by_scenario$SC5$RUN9,
    "RUN10"      = results_by_scenario$SC5$RUN10,
    "RUN11"      = results_by_scenario$SC5$RUN11,
    "RUN12"      = results_by_scenario$SC5$RUN12
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
    #"RUN5"       = results_by_scenario$SC6$RUN5,
    #"RUN6"       = results_by_scenario$SC6$RUN6,
    #"RUN7"       = results_by_scenario$SC6$RUN7,
    #"RUN8"       = results_by_scenario$SC6$RUN8,
    #"RUN9"       = results_by_scenario$SC6$RUN9,
    #"RUN10"      = results_by_scenario$SC6$RUN10,
    "RUN11"      = results_by_scenario$SC6$RUN11,
    "RUN12"      = results_by_scenario$SC6$RUN12
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
    # "RUN5"       = results_by_scenario$SC7$RUN5,
    # "RUN6"       = results_by_scenario$SC7$RUN6,
    # "RUN7"       = results_by_scenario$SC7$RUN7,
     "RUN8"       = results_by_scenario$SC7$RUN8,
     "RUN9"       = results_by_scenario$SC7$RUN9
    # "RUN10"      = results_by_scenario$SC7$RUN10,
    # "RUN11"      = results_by_scenario$SC7$RUN11,
    # "RUN12"      = results_by_scenario$SC7$RUN12

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
    # "RUN5"       = results_by_scenario$SC8$RUN5,
    # "RUN6"       = results_by_scenario$SC8$RUN6,
    # "RUN7"       = results_by_scenario$SC8$RUN7,
    # "RUN8"       = results_by_scenario$SC8$RUN8,
    # "RUN9"       = results_by_scenario$SC8$RUN9,
    # "RUN10"      = results_by_scenario$SC8$RUN10,
    "RUN11"      = results_by_scenario$SC8$RUN11,
    "RUN12"      = results_by_scenario$SC8$RUN12
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
    # "RUN5"       = results_by_scenario$SC9$RUN5,
    # "RUN6"       = results_by_scenario$SC9$RUN6,
    # "RUN7"       = results_by_scenario$SC9$RUN7,
    # "RUN8"       = results_by_scenario$SC9$RUN8,
    # "RUN9"       = results_by_scenario$SC9$RUN9,
    "RUN10"      = results_by_scenario$SC9$RUN10,
    "RUN11"      = results_by_scenario$SC9$RUN11,
    "RUN12"      = results_by_scenario$SC9$RUN12
  ),
  varname = c("B", "F", "C", "P"),
  CI = 0.1
)

dev.off()
