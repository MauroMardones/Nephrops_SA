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

# RUN6 — prior_logn Tighter Schaefer
priors_run6 <- list(
  name = "RUN6_prior_logn_tighter",
  priors = list(
    logn  = c(log(2), 0.5, 1)
    #logK = log(100)
  )   # prior on n only (Schaefer-type))
)


# RUN7 — fixing log_n
priors_run7 <- list(
  name = "RUN7_fixing_logn",
  ini = list(
    logn = log(2)),
  phases =list(
    logn=-1)
)

# RUN8 — prior_logn Tighter Schaefer and prior logr
priors_run8 <- list(
  name = "RUN8_prior_logn_tighter_logr",
  priors = list(
    logn     = priors_run6$priors$logn,
    logr      = c(log(0.4), 0.2, 1)
  ))

# RUN8.1 — fixing log_n and prior logr (no usar)
# priors_run8.1 <- list(
#   name = "RUN8_fixing_logn_logr",
#   priors = list(
#     logr      = c(log(0.5), 0.5, 1)
#   ),
#   ini = list(
#     logn = log(2)),
#   phases =list(
#     logn=-1))
#

# RUN9— prior_logn Tighter Schaefer and prior logr and logbkfrac
priors_run9 <- list(
  name = "RUN8_prior_logn_tighter_logr_logbk",
  priors = list(
    logn     = priors_run6$priors$logn,
    logr      = priors_run8$priors$logr,
    logbkfrac = c(log(0.5), 0.5, 1)
  ))

priors_run10<- list(
  name = "RUN10_uncertan_catch",
  priors = list(
    logalpha = c(1, 1, 0), # disable
    logbeta  = c(1, 1, 0), # disable , to activate, change to (0,0,0)
    logn  = c(log(2), 0.5, 1),  # prior on n only (Schaefer-type)
    logr  = c(log(0.4), 0.2, 1)
    # logsdb = c(log(0.2), 0.5), # prior on catch process uncertainty
    # logsdc = c(log(3), 0.5, 1), # prior decrease catch sd
    # logsdf = c(log(0.1), 0.2, 1)  # prior decrease fishing mortality sd
  )   # prior on n only (Schaefer-type))
)
priors_run11<- list(
  name = "RUN11_uncertan_catch",
  priors = list(
    logalpha = c(1, 1, 1), # act1vate
    logbeta  = c(1, 1, 1), # activte , to activate, change to (0,0,0)
    logn  = c(log(2), 0.5, 1),   # prior on n only (Schaefer-type)
    logr  = c(log(0.4), 0.2, 1),
    logsdb = c(log(0.2), 0.5), # prior on catch process uncertainty
    logsdc = c(log(3), 0.5, 1), # prior decrease catch sd
    logsdf = c(log(0.1), 0.2, 1)  # prior decrease fishing mortality sd
  )   # prior on n only (Schaefer-type))
)

priors_run12<- list(
  name = "RUN12_uncertan_catch_w_ab",
  priors = list(
    logalpha = c(1, 1, 0), # disable
    logbeta  = c(1, 1, 0), # disable , to activate, change to (0,0,0)
    logn  = c(log(2), 0.5, 1),   # prior on n only (Schaefer-type)
    logr  = c(log(0.4), 0.2, 1),
    logsdb = c(log(0.2), 0.5), # prior on catch process uncertainty
    logsdc = c(log(0.4), 0.2, 1), # prior decrease catch sd
    logsdf = c(log(0.1), 0.2, 1)  # prior decrease fishing mortality sd
  )   # prior on n only (Schaefer-type))
)
scenarios_priors <- list(
  RUN5 = priors_run5,
  RUN6 = priors_run6,
  RUN7 = priors_run7,
  RUN8 = priors_run8,
  RUN9 = priors_run9,
  RUN10= priors_run10,
  RUN11= priors_run11,
  RUN12= priors_run12
)

#guardar Rdata
saveRDS(scenarios_priors,  file = "data/scenarios_priors.rds")
