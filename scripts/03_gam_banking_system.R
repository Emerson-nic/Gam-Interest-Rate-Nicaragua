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
  if (file.exists("csv/sistema_bancario.csv")) {
    banca <- readr::read_csv("csv/sistema_bancario.csv")
  } else {
    source("scripts/01_cleaning.R")
  }
}

#select data ----
banca <- banca %>%
  arrange(fecha) %>%
  mutate(tiempo = row_number())

dplyr::glimpse(banca)

banca <- banca %>%
  mutate(ratio_morosidad_prop = ratio_morosidad_pct / 100,
         ratio_liquidez_prop = ratio_liquidez_pct / 100,
         d_ln_imae = d_ln_imae * 100,
         d_ln_ipc = d_ln_ipc * 100,
         mes = as.numeric(format(fecha, "%m"))
         )

names(banca)

banca_ni <- banca %>%
  dplyr::select(
    tasa_interes_activo,
    ratio_morosidad_prop,
    ratio_liquidez_prop,
    d_ln_imae,
    d_ln_ipc,
    tiempo,
    mes
  )

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
  dplyr::select(Tasa_interes_activa,
                Morosidad,
                Liquidez,
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

dplyr::glimpse(banca_ni)

# gam models -----

## gam model 1 -----
modelo_banca <- gam(
  formula = Morosidad ~ s(Tasa_interes_activa, k = 5) +
    s(Var_ln_IMAE, k = 5) +
    s(Var_ln_IPC, k = 5) +
    s(Tiempo, k = 20) + # trend
    s(Mes, bs = "cc", k = 12) +  #seasonality
    Liquidez,   
  family = quasibinomial(link = "logit"),
  method = "REML",
  data = banca_ni
)

#note: a high k value allows the curve to become very wavy and capture 
#sharp peaks, while a low k value forces it to be a simple curve or nearly 
#a straight line

summary(modelo_banca)

#k > 0.05, the degrees of freedom are adequate
gam.check(modelo_banca)

draw(modelo_banca, residuals = TRUE)

plot(modelo_banca, pages = 1, shade = TRUE, residuals = TRUE)

residuos_gam <- residuals(modelo_banca, type = "deviance")
rho_est <- acf(residuos_gam, plot = TRUE)$acf[2]
rho_est

## gam model 2 ----

modelo_bam_ar1 <- bam(
  Morosidad ~ s(Tasa_interes_activa, k = 5) +
    s(Var_ln_IMAE, k = 5) +
    s(Var_ln_IPC, k = 5) +
    s(Tiempo, k = 20) +
    s(Mes, bs = "cc", k = 12) +
    Liquidez,
  family = quasibinomial(link = "logit"),
  method = "fREML",
  rho = rho_est,
  data = banca_ni
)

summary(modelo_bam_ar1)
gam.check(modelo_bam_ar1)

residuos_bam <- residuals(modelo_bam_ar1, type = "deviance")
acf(residuos_bam, main = "acf residuos ar(1)")