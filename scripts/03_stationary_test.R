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
  mutate(d_ln_imae = d_ln_imae * 100,
         d_ln_ipc = d_ln_ipc * 100,
         mes = as.numeric(format(fecha, "%m"))
         )

banca <- banca %>%
  arrange(fecha) %>%
  mutate(tiempo = row_number())

dplyr::glimpse(banca)

banca_ni <- banca %>%
  dplyr::rename(
    Tasa_interes_activa = tasa_interes_activo,
    Morosidad = ratio_morosidad_prop,
    Liquidez = ratio_liquidez_prop,
    Var_ln_IMAE = d_ln_imae,
    Var_ln_IPC = d_ln_ipc,
    Tiempo = tiempo,
    Mes = mes
  ) 

banca_ni <- banca_ni %>%
  dplyr::select(fecha,
                Tasa_interes_activa,
                Morosidad,
                Liquidez,
                Var_ln_IMAE,
                Var_ln_IPC,
                Tiempo,
                Mes
  )

dplyr::glimpse(banca_ni)


#check unit root ----

tseries::adf.test(banca_ni$Morosidad)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Morosidad
# Dickey-Fuller = -1.71, Lag order = 6, p-value = 0.6977
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Tasa_interes_activa)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Tasa_interes_activa
# Dickey-Fuller = -2.7235, Lag order = 6, p-value = 0.2724
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Liquidez)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Liquidez
# Dickey-Fuller = -1.9651, Lag order = 6, p-value = 0.5906
# alternative hypothesis: stationary

adf.test(banca_ni$Var_ln_IMAE)
# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_IMAE
# Dickey-Fuller = -3.4664, Lag order = 6, p-value = 0.04697
# alternative hypothesis: stationary

tseries::adf.test(banca_ni$Var_ln_IPC)

# Augmented Dickey-Fuller Test
# 
# data:  banca_ni$Var_ln_IPC
# Dickey-Fuller = -5.6604, Lag order = 6, p-value < 0.01
# alternative hypothesis: stationary

#diff to Morosidad, Tasa_interes_activa, Liquidez

banca_ni <- banca_ni %>%
  dplyr::mutate(Var_Morosidad = Morosidad - dplyr::lag(Morosidad, 1),
                Var_Tasa_interes_activa = Tasa_interes_activa - dplyr::lag(Tasa_interes_activa, 1),
                Var_Liquidez = Liquidez - dplyr::lag(Liquidez, 1)) %>%
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

#save csv
readr::write_csv(banca_ni, "csv/banca_nicaragua_estacionarios.csv")

#dummies ----

names(banca)

banca <- banca %>%
  dplyr::mutate(ratio_morosidad_prop = ratio_morosidad_pct / 100,
                ratio_liquidez_prop = ratio_liquidez_pct / 100,
                tasa_interes_activo = tasa_interes_activo /100,
                d_ln_imae = d_ln_imae / 100,
                d_ln_ipc = d_ln_ipc / 100,
                mes = as.numeric(format(fecha, "%m"))
  )

banca <- banca %>%
  dplyr::mutate(
    tasa_interes_activo_real = tasa_interes_activo - d_ln_ipc
  )

banca <- banca %>%
  arrange(fecha) %>%
  dplyr::mutate(tiempo = row_number())

banca <- banca %>%
  dplyr::select(fecha,
                tasa_interes_activo,
                ratio_morosidad_prop,
                ratio_liquidez_prop,
                d_ln_imae,
                d_ln_ipc,
                tiempo,
                mes,
                tasa_interes_activo_real
                )

banca %>% tibble::as_tibble() %>%
  print(n=300)

banca <- banca %>%
  mutate(
    crisis_2008 = ifelse(fecha >= as.Date("2008-04-01") & fecha <= as.Date("2010-12-01"), 1, 0),
    crisis_2018 = ifelse(fecha >= as.Date("2018-05-01") & fecha <= as.Date("2019-12-01"), 1, 0),
    crisis_covid = ifelse(fecha >= as.Date("2020-01-01") & fecha <= as.Date("2021-12-01"), 1, 0)
  )

readr::write_csv(banca, "csv/datos_bancario.csv")

rm(list = ls())

