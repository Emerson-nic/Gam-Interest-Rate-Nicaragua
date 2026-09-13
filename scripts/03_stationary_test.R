#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-04"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               tseries
               )

if (!exists("banca")) {
  if (file.exists("csv/sistema_bancario.csv")) {
    banca <- readr::read_csv("csv/sistema_bancario.csv")
  } else {
    source("scripts/01_cleaning.R")
  }
}

#select data ----

banca <- banca %>%
  mutate(
    d_ln_imae = d_ln_imae * 100,
    d_ln_ipc = d_ln_ipc * 100,
    d_ln_itcer = d_ln_itcer * 100,
    d_ln_asegurados_inss = d_ln_asegurados_inss * 100,
    d_ln_cafe_millones = d_ln_cafe_millones *100,
    d_ln_banano_millones = d_ln_banano_millones *100,
    d_ln_azucar_millones = d_ln_azucar_millones * 100,
    mes = as.numeric(format(fecha, "%m"))
         )

banca <- banca %>%
  arrange(fecha) %>%
  mutate(tiempo = row_number())

dplyr::glimpse(banca)

banca_ni <- banca %>%
  dplyr::rename(
    Fecha = fecha,
    Tasa_interes_activa = tasa_interes_activo,
    FEDFUNDS = FEDFUNDS,
    Morosidad = ratio_morosidad,
    Liquidez = ratio_liquidez,
    Var_ln_IMAE = d_ln_imae,
    Var_ln_IPC = d_ln_ipc,
    Var_ln_itcer = d_ln_itcer,
    Var_ln_cafe = d_ln_cafe_millones,
    Var_ln_azucar = d_ln_azucar_millones,
    Var_ln_banano = d_ln_banano_millones,
    Var_ln_inss = d_ln_asegurados_inss,
    Tiempo = tiempo,
    Mes = mes
  ) 

banca_ni <- banca_ni %>%
  dplyr::select(Fecha,
                Tasa_interes_activa,
                FEDFUNDS,
                Morosidad,
                Liquidez,
                Var_ln_IMAE,
                Var_ln_IPC,
                Var_ln_itcer,
                Var_ln_cafe,
                Var_ln_azucar,
                Var_ln_banano,
                Var_ln_inss,
                Tiempo,
                Mes
  )

dplyr::glimpse(banca_ni)


#check unit root ----

set.seed(57971643)

tseries::adf.test(banca_ni$Morosidad)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Morosidad
# Dickey-Fuller = -1.6307, Lag order = 6, p-value = 0.731
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Tasa_interes_activa)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Tasa_interes_activa
# Dickey-Fuller = -2.7382, Lag order = 6, p-value = 0.2662
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$FEDFUNDS)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$FEDFUNDS
# Dickey-Fuller = -3.3794, Lag order = 6, p-value = 0.05915
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Liquidez)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Liquidez
# Dickey-Fuller = -1.968, Lag order = 6, p-value = 0.5894
# alternative hypothesis: stationary

adf.test(banca_ni$Var_ln_IMAE)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_IMAE
# Dickey-Fuller = -3.3945, Lag order = 6, p-value = 0.05663
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Var_ln_IPC)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_IPC
# Dickey-Fuller = -5.6604, Lag order = 6, p-value < 0.01
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Var_ln_itcer)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_itcer
# Dickey-Fuller = -5.6201, Lag order = 6, p-value < 0.01
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Var_ln_cafe)

# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_cafe
# Dickey-Fuller = -4.8046, Lag order = 6, p-value < 0.01
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Var_ln_azucar)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_azucar
# Dickey-Fuller = -4.7484, Lag order = 6, p-value < 0.01
# alternative hypothesis: stationary


tseries::adf.test(banca_ni$Var_ln_banano)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_banano
# Dickey-Fuller = -4.0235, Lag order = 6, p-value < 0.01
# alternative hypothesis: stationary


tseries::adf.test(banca_ni$Var_ln_inss)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_inss
# Dickey-Fuller = -3.5468, Lag order = 6, p-value = 0.03935
# alternative hypothesis: stationary

#note: Commodities are non-stationary in year-on-year termns


#diff to Morosidad, Tasa_interes_activa, Liquidez

banca_ni <- banca_ni %>%
  dplyr::mutate(Var_Morosidad = Morosidad - dplyr::lag(Morosidad, 1),
                Var_Tasa_interes_activa = Tasa_interes_activa - dplyr::lag(Tasa_interes_activa, 1),
                Var_Liquidez = Liquidez - dplyr::lag(Liquidez, 1),
                ) %>%
  dplyr::select(-Morosidad, -Tasa_interes_activa, -Liquidez) %>%
  stats::na.omit()

tseries::adf.test(banca_ni$Var_Morosidad)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_Morosidad
# Dickey-Fuller = -5.33, Lag order = 5, p-value < 0.01
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Var_Tasa_interes_activa)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_Tasa_interes_activa
# Dickey-Fuller = -8.2793, Lag order = 5, p-value < 0.01
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Var_Liquidez)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_Liquidez
# Dickey-Fuller = -5.6812, Lag order = 5, p-value < 0.01
# alternative hypothesis: stationary

dplyr::glimpse(banca_ni)

#dummies ----
banca_ni <- banca_ni %>%
  mutate(
    crisis_2008 = ifelse(Fecha >= as.Date("2008-04-01") & Fecha <= as.Date("2010-12-01"), 1, 0),
    crisis_2018 = ifelse(Fecha >= as.Date("2018-05-01") & Fecha <= as.Date("2019-12-01"), 1, 0),
    crisis_covid = ifelse(Fecha >= as.Date("2020-01-01") & Fecha <= as.Date("2021-12-01"), 1, 0)
  )

#save csv
readr::write_csv(banca_ni, "csv/banca_nicaragua_estacionarios.csv")

rm(list = ls())

