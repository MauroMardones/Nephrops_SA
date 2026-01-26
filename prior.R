rm(list = ls())
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
    # logbkfrac = c(log(0.5), 0.2, 1),
    logsdi = c(log(0.1), 0.2, 1),
    logn      = c(log(2),   0.5, 1),
    logr      = c(log(0.5), 0.2, 1),
    logsdb = c(log(0.2), 0.5),
    logsdc = c(log(3), 0.5, 1),
    logdf = c(log(0.1), 0.2, 1)
  )
)

# RUN8 — Alpha and beta stabilising priors disabled
priors_run8 <- list(
  name = "RUN8_no_alpha_beta",
  priors = list(
    logbkfrac = c(log(0.5), 0.2, 1),
    logalpha = c(1, 1, 0), # disable
    logbeta  = c(1, 1, 0), # disable , to activate, change to (0,0,0)
    logn      = c(log(2),   0.5, 1),
    logr      = c(log(0.5), 0.2, 1),
    logsdb = c(log(0.2), 0.5),
    logsdc = c(log(3), 0.5, 1),
    logdf = c(log(0.1), 0.2, 1)
  )
)

scenarios_priors <- list(
  # RUN5 = priors_run5,
  # RUN6 = priors_run6,
  # RUN7 = priors_run7,
  RUN8 = priors_run8
)

#guardar Rdata
saveRDS(scenarios_priors,  file = "data/scenarios_priors.rds")
