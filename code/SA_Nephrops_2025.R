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
# definir un directorio para guardar plott "figs"

fig.path <- here("figs")
if (!dir.exists(fig.path)) {
  dir.create(fig.path)
}




## --------------------------------------------------------------------------------
bac <- read_excel(here("data",
                           "inputdata_spict_fu30_2025.xlsx"))


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
p3 <- plot_index("isunepbio_2", "Index value")
p4 <- plot_index("isunepabun", "Index value")
p5 <- plot_index("rendiarsaspr", "Index value")
p6 <- plot_index("LPUEcomercial", "Index value")

# ---- Combinar todos los gráficos ----
# y guuardar en "figs"
fig_indices <- ggarrange(
  p_catch,
  ggarrange(
    p1, p2, p3, p4, p5, p6,
    ncol = 2,
    nrow = 3,
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



## --------------------------------------------------------------------------------
# Visual check
str(data)
summary(data)


## ----warning=FALSE, message=FALSE------------------------------------------------
data_log <- data %>%
  select(-year, -catch) %>%
  mutate(across(everything(), log))

cor_pearson  <- cor(data_log, use = "pairwise.complete.obs", method = "pearson")
cor_spearman <- cor(data_log, use = "pairwise.complete.obs", method = "spearman")

pheatmap(cor_pearson,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Correlation Heatmap (Pearson)",
         color = colorRampPalette(c("blue", "white", "red"))(50))

pheatmap(cor_spearman,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Correlation Heatmap (Spearman)",
         color = colorRampPalette(c("blue", "white", "red"))(50))



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
  obsI = data$isunepbio,
  timeI = data$year + 0.5
)

I_isunepbio2 <- data.frame(
  obsI = data$isunepbio_2,
  timeI = data$year + 0.5
)

I_isunepabun <- data.frame(
  obsI = data$isunepabun,
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


## --------------------------------------------------------------------------------
ind  <- which(C_nep$timeC == 1994)
ind2 <- which(C_nep$timeC == 2025)


## --------------------------------------------------------------------------------

inp1 <- list(
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
inp1 <- list(
  # ---- Capturas ----
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  # ---- Índices de abundancia ----
  timeI = list(


    I_isunepbio$timeI[ind:ind2],     # 3. ISUNEP biomass


    I_LPUEcom$timeI[ind:ind2]        # 6. LPUE comercial
  ),

  obsI = list(


    I_isunepbio$obsI[ind:ind2],


    I_LPUEcom$obsI[ind:ind2]
  )
)


## --------------------------------------------------------------------------------
inp1 <- list(
  # ---- Capturas ----
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  # ---- Índices de abundancia ----
  timeI = list(
    I_arsa_spring$timeI[ind:ind2],   # 1. ARSA spring

    I_isunepbio2$timeI[ind:ind2]     # 3. ISUNEP biomass
   ),

  obsI = list(
    I_arsa_spring$obsI[ind:ind2],

    I_isunepbio2$obsI[ind:ind2]
  )
)


## --------------------------------------------------------------------------------
inp1 <- list(
  # ---- Capturas ----
  timeC = C_nep$timeC[ind:ind2],
  obsC  = C_nep$obsC[ind:ind2],

  # ---- Índices de abundancia ----
  timeI = list(

    I_isunepbio$timeI[ind:ind2]     # 3. ISUNEP biomass
   ),

  obsI = list(

    I_isunepbio$obsI[ind:ind2]
  )
)


## --------------------------------------------------------------------------------
inp1$dteuler <- 1/16  # must be set before check.inp
inp1 <- check.inp(inp1)

# Inspect
inp1$dtc


## --------------------------------------------------------------------------------
# hacer una tabla con los escenarios con los indices usados


## ----figwidth=8, fig.height=9----------------------------------------------------
plotspict.data(inp1)
plotspict.ci(inp1)
guess.m(inp1) # solo probatina


## --------------------------------------------------------------------------------
# Parámetros comunes
r <- 1
K <- 1
B <- seq(0.001, K, length.out = 200)  # evita log(0) en Fox

# Parámetro de Pella–Tomlinson
n_pella <- 2

# Funciones de producción
P_schaefer <- r * B * (1 - B / K)             # n = 1 (Schaefer)
P_pella    <- r * B * (1 - (B / K)^n_pella)   # n = 2 (Pella–Tomlinson)
P_fox      <- r * B * log(K / B)              # Fox

# Crear dataframe para graficar
df <- data.frame(
  B = rep(B, 3),
  Production = c(P_schaefer, P_pella, P_fox),
  Model = factor(rep(c("Schaefer (n=1)",
                       "Pella–Tomlinson (n=2)",
                       "Fox (log)"), each = length(B)))
)

# Graficar
ggplot(df, aes(x = B/K, y = Production, color = Model)) +
  geom_line(size = 1.2) +
  theme_bw(base_size = 14) +
  labs(
    x = "Relative Biomass (B/K)",
    y = "Relative Production (Y/K)",
    title = "Comparison of Surplus Production Models: Fox, Schaefer, and Pella–Tomlinson"
  ) +
  scale_color_manual(values = c("blue", "red", "darkgreen")) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )



## --------------------------------------------------------------------------------
tab_modelos <- data.frame(
  Modelo = c("Fox", "Schaefer", "Pella–Tomlinson (n=2)"),
  `Forma de la curva` = c("Asimétrica izquierda", "Simétrica", "Asimétrica derecha"),
  `BMSY/K` = c("~0.37", "0.5", "~0.6–0.7"),
  `Tipo de especie` = c("Rápido crecimiento", "Intermedia", "Lenta, longeva"),
  `Implicancia de manejo` = c(
    "Alta explotación tolerable",
    "Balanceado",
    "Precaución, biomasa alta necesaria"
  )
)

ft <- flextable(tab_modelos) |>
  set_header_labels(
    Modelo = "Modelo",
    `Forma de la curva` = "Forma de la curva",
    `BMSY/K` = "BMSY/K",
    `Tipo de especie` = "Tipo de especie",
    `Implicancia de manejo` = "Implicancia de manejo"
  ) |>
  theme_vanilla() |>
  autofit() |>
  bold(part = "header") |>
  color(part = "header", color = "white") |>
  bg(part = "header", bg = "#2E86C1") |>
  fontsize(size = 11, part = "all") |>
  align(align = "center", part = "all")

ft


## --------------------------------------------------------------------------------

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

