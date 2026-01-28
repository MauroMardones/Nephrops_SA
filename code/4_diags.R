
## --------------Function to extract diagnostics from a spict fit object------------------------------------------

# Table and plots
results_by_scenario#<-readRDS("outputs/SPiCT_SC8_SC9_RUN8_results.rds")

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
dir.create("outputs/results", showWarnings = FALSE)
# ojo aca. Cambiar los valores de escenarios y runs según corresponda
scenarios <- paste0("SC", 5:9)
runs <- paste0("RUN", 5:12)

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
  file = "outputs/AIC_by_scenario_run_5_9.csv",
  row.names = FALSE
)


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
  file = "outputs/brps_table_SC5_SC9.csv",
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
  scale_color_viridis_d(option = "G")+
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
  filename = "figs/SPiCT_BRP_scenario_5_9_RUN_comparison.png",
  plot = p,
  width = 6,
  height = 10,
  dpi = 300
)

#### ------ plotprios ----------------
# wit plots.spict.prior function. i.e plotspict.priors(results_by_scenario$SC5$RUN5)


dir.create("figs/priors", recursive = TRUE, showWarnings = FALSE)

for (sc in names(results_by_scenario)) {
  for (run in names(results_by_scenario[[sc]])) {

    fit <- results_by_scenario[[sc]][[run]]

    cat("Generating SPiCT priors plot:", sc, run, "\n")

    fname <- file.path(
      "figs/priors",
      paste0("SPiCT_priors_", sc, "_", run, ".png")
    )

    png(filename = fname, width = 2400, height = 1800, res = 300)

    try(
      plotspict.priors(fit),
      silent = TRUE
    )

    dev.off()
  }
}

## ----Final Tables Diags -----

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
    mohn_table, # object from retro.R
    by = c("Scenario", "Run")
  )

# Save summary table
write.csv(
  summary_table,
  "outputs/SPiCT_summary_table_all_scenarios_runs.csv",
  row.names = FALSE
)












