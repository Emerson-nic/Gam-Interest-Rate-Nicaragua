#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-04"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse
)

if (!exists("banca")) {
  if (file.exists("csv/banca_nicaragua_estacionarios.csv")) {
    banca_ni <- readr::read_csv("csv/banca_nicaragua_estacionarios.csv")
  } else {
    source("scripts/03_stationary_test.R")
  }
}

#select data ----

moneda <- banca_ni %>%
  dplyr::select(Fecha, Var_ln_IPC, Var_ln_itcer) %>%
  tidyr::drop_na()

ciclo_economico <- banca_ni %>%
  dplyr::select(Fecha, Var_ln_IMAE, Var_ln_inss, FEDFUNDS) %>%
  tidyr::drop_na()
  

#pca moneda ----

pca_moneda <- stats::prcomp(~ moneda$Var_ln_IPC +
                              moneda$Var_ln_itcer, center = TRUE,  #subtracting the historical 
                            #mean of each variable causes them all to start 
                            #from a common origin point
                            scale. = TRUE) #divide each variable by its 
#standard deviation so that they all have an exact variance of 1
summary(pca_moneda)
# Importance of components:
#   PC1     PC2
# Standard deviation     1.3677 0.35963
# Proportion of Variance 0.9353 0.06467
# Cumulative Proportion  0.9353 1.00000

pca_moneda$rotation
# PC1       PC2
# moneda$Var_ln_IPC   -0.7071068 0.7071068
# moneda$Var_ln_itcer  0.7071068 0.7071068

#pca ciclo_economico ----

pca_ciclo_economico <- stats::prcomp(~ Var_ln_IMAE + Var_ln_inss + FEDFUNDS,
                                     data = ciclo_economico,
                                     center = TRUE, 
                                     scale. = TRUE)

summary(pca_ciclo_economico)
# Importance of components:
#   PC1    PC2     PC3
# Standard deviation     1.3330 0.9804 0.51183
# Proportion of Variance 0.5923 0.3204 0.08732
# Cumulative Proportion  0.5923 0.9127 1.00000

pca_ciclo_economico$rotation
# PC1         PC2        PC3
# Var_ln_IMAE -0.6151287 -0.48550414  0.6212105
# Var_ln_inss -0.6976334 -0.03191167 -0.7157438
# FEDFUNDS     0.3673205 -0.87365175 -0.3190741

#marge pca ----

banca_pca <- moneda %>%
  dplyr::mutate(
    PC1_Moneda = pca_moneda$x[, 1],
    PC1_Ciclo = pca_ciclo_economico$x[, 1],
    PC2_Ciclo = pca_ciclo_economico$x[, 2]
  ) %>%
  dplyr::select(-Var_ln_IPC, -Var_ln_itcer)
  

#save csv ----

banca_pca_csv <- banca_ni %>%
  dplyr::inner_join(banca_pca, by = "Fecha") %>%
  tibble::as_tibble() %>%
  print(n=300)

readr::write_csv(banca_pca_csv, "csv/banca_nicaragua_estacionarios_pca.csv")

rm(list = ls())
