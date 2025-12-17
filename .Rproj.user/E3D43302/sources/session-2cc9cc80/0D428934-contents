library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

Nephind <- read_excel(here("data",
                      "NephropsFU30_indices-2.xlsx"))
names(Nephind)
Neph <- Nephind %>%
  mutate(Year = as.numeric(Year))

plot1_df <- Neph %>%
  select(Year,
         ARSA_std_Model_Casper_norm = `ARSA_std_Model Casper_normalized`,
         ARSA_CV_std_norm,
         isun = isunepca_normalizada,
         isun_cv = CV_isunepca_bio_norm) %>%
  pivot_longer(cols = c(ARSA_std_Model_Casper_norm, isun),
               names_to = "index",
               values_to = "value") %>%
  mutate(
    CV = case_when(
      index == "ARSA_std_Model_Casper_norm" ~ ARSA_CV_std_norm,
      index == "isun" ~ isun_cv
    ),
    lower = value * (1 - CV),
    upper = value * (1 + CV)
  )


plot1 <- ggplot(plot1_df, aes(x = Year, y = value, color = index, fill = index)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Normalized index"
  ) +
  theme_bw() +
  theme(legend.position = "top")
plot1

###

Neph <- Neph %>%
  mutate(
    lower_casper = `ARSA_std_Model Casper_normalized` * (1 - ARSA_CV_std_norm),
    upper_casper = `ARSA_std_Model Casper_normalized` * (1 + ARSA_CV_std_norm),
    lower_isun = isunepca_normalizada * (1 - CV_isunepca_bio_norm),
    upper_isun = isunepca_normalizada * (1 + CV_isunepca_bio_norm)
  )


plot2 <- ggplot() +
  # Ribbon Casper (rojo)
  geom_ribbon(
    data = Neph,
    aes(x = Year, ymin = lower_casper, ymax = upper_casper),
    fill = "red", alpha = 0.2
  ) +
  # Ribbon ISUNEPCA (azul)
  geom_ribbon(
    data = Neph,
    aes(x = Year, ymin = lower_isun, ymax = upper_isun),
    fill = "blue", alpha = 0.2
  ) +

  # Línea ARSA standardized and normalized (roja)
  geom_line(
    data = Neph,
    aes(x = Year,
        y = `ARSA_std_Model Casper_normalized`,
        color = "ARSA standardized and normalized"),
    linewidth = 1
  ) +

  # Línea ISUNEPCA normalized (azul)
  geom_line(
    data = Neph,
    aes(x = Year, y = isunepca_normalizada,
        color = "ISUNEPCA normalized"),
    linewidth = 1
  ) +

  # Línea ARSA normalized (negra)
  geom_line(
    data = Neph,
    aes(x = Year, y = ARSA_normalized,
        color = "ARSA normalized"),
    linewidth = 1
  ) +

  labs(
    x = "",
    y = "Index (normalized)",
    color = "Index"
  ) +

  scale_color_manual(values = c(
    "ARSA standardized and normalized" = "red",
    "ISUNEPCA normalized" = "blue",
    "ARSA normalized" = "black"
  )) +

  scale_x_continuous(breaks = Neph$Year) +

  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
plot2
