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
  dplyr::select(Var_ln_IPC, Var_ln_itcer) %>%
  tidyr::drop_na()

ciclo_economico <- banca_ni %>%
  dplyr::select(Var_ln_IMAE, Var_ln_inss, FEDFUNDS) %>%
  tidyr::drop_na()
  

#pca moneda ----

pca_moneda <- stats::prcomp(moneda, center = TRUE,  #subtracting the historical 
                            #mean of each variable causes them all to start 
                            #from a common origin point
                            scale. = TRUE) #divide each variable by its 
#standard deviation so that they all have an exact variance of 1
summary(pca_moneda)

pca_moneda$rotation

#pca ciclo_economico ----

pca_ciclo_economico <- stats::prcomp(ciclo_economico, center = TRUE, scale. = TRUE)
summary(pca_ciclo_economico)

pca_ciclo_economico$rotation

rm(list = ls())