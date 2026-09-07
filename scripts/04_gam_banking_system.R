#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-04"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               mgcv, #generalized additive models
               gratia, #tools for extracting smoothed and derivative functions
               nlme, #for mixed models and correlation ar(1)
               itsadug, #additional functions for gam and autocorrelation 
               patchwork #combining charts  
)

if (!exists("banca")) {
  if (file.exists("csv/banca_nicaragua_estacionarios.csv")) {
    banca_ni <- readr::read_csv("csv/banca_nicaragua_estacionarios.csv")
    banca <- readr::read_csv("csv/sistema_bancario.csv")
  } else {
    source("scripts/03_stationary_test.R")
  }
}

#select data ----

banca <- banca %>%
  mutate(ratio_morosidad_prop = ratio_morosidad_pct / 100,
         ratio_liquidez_prop = ratio_liquidez_pct / 100,
         d_ln_imae = d_ln_imae * 100,
         d_ln_ipc = d_ln_ipc * 100,
         mes = as.numeric(format(fecha, "%m"))
  )

banca <- banca %>%
  arrange(fecha) %>%
  mutate(tiempo = row_number())

names(banca)

banca <- banca %>%
  dplyr::select(tasa_interes_activo,
                ratio_morosidad_prop,
                ratio_liquidez_prop,
                d_ln_imae,
                d_ln_ipc,
                tiempo,
                mes
                )

banca <- banca %>%
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
  dplyr::select(Var_Tasa_interes_activa,
                Var_Morosidad,
                Var_Liquidez,
                Var_ln_IMAE,
                Var_ln_IPC,
                Tiempo,
                Mes
                )

banca_nombres <- c(
  "Tasa interés activa (%)",
  "Morosidad (prop.)",
  "Liquidez (prop.)",
  "Var. ln IMAE",
  "Var. ln IPC",
  "Tiempo",
  "Mes"
)

banca_ni_nombres <- c(
  "Var. Tasa interés activa (%)",
  "Var. Morosidad",
  "Var. Liquidez",
  "Var. ln IMAE",
  "Var. ln IPC",
  "Tiempo",
  "Mes"
)

dplyr::glimpse(banca_ni)
dplyr::glimpse(banca)

# gam models -----

## gam model 1 -----
modelo_gam <- gam(
  Morosidad ~ s(Tasa_interes_activa, k = 5) +
    s(Var_ln_IMAE, k = 5) +
    s(Var_ln_IPC, k = 5) +
  Liquidez,   
  family = quasibinomial(link = "logit"),
  method = "REML",
  data = banca
)

#note: a high k value allows the curve to become very wavy and capture 
#sharp peaks, while a low k value forces it to be a simple curve or nearly 
#a straight line

summary(modelo_gam)

mgcv::concurvity(modelo_gam, full = FALSE)

#k > 0.05, the degrees of freedom are adequate
mgcv::gam.check(modelo_gam)

gratia::draw(modelo_gam, residuals = TRUE)

plot(modelo_gam, pages = 1, shade = TRUE, residuals = TRUE)

residuos_gam <- stats::residuals(modelo_gam, type = "deviance")
rho_est <- stats::acf(residuos_gam, plot = TRUE)$acf[2]

## gamm model 2 ----



