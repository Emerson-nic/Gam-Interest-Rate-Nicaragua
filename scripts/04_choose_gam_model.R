#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-04"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               lmtest, #normality and heteroscedasticity tests 
               brms, #bayesian regression models using Stan
               mgcv, #generalized additive models
               gratia, #tools for extracting smoothed and derivative functions
               nlme, #for mixed models and correlation ar(1)
               itsadug, #additional functions for gam and autocorrelation 
               patchwork #combining charts  
)

if (!exists("banca")) {
  if (file.exists("csv/banca_nicaragua_estacionarios.csv")) {
    banca_ni <- readr::read_csv("csv/banca_nicaragua_estacionarios.csv")
    banca <- readr::read_csv("csv/datos_bancario.csv")
  } else {
    source("scripts/03_stationary_test.R")
  }
}

#select data ----

banca <- banca %>%
  dplyr::select(tasa_interes_activo,
                ratio_morosidad_prop,
                ratio_liquidez_prop,
                d_ln_imae,
                d_ln_ipc,
                tiempo,
                mes,
                tasa_interes_activo_real,
                crisis_2008,
                crisis_2018,
                crisis_covid
                )

banca <- banca %>%
  dplyr::rename(
    Tasa_interes_activa = tasa_interes_activo,
    Morosidad = ratio_morosidad_prop,
    Liquidez = ratio_liquidez_prop,
    Var_ln_IMAE = d_ln_imae,
    Var_ln_IPC = d_ln_ipc,
    Tiempo = tiempo,
    Mes = mes,
    Tasa_interes_activa_real = tasa_interes_activo_real,
    Crisis_2008 = crisis_2008,
    Crisis_2018 = crisis_2018,
    Crisis_covid = crisis_covid
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
  "Mes",
  "Tasa interés activa real (%)"
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

banca %>% dplyr::select(Tasa_interes_activa, Tasa_interes_activa_real,
                        Var_ln_IPC) %>%
  tibble::as_tibble() %>%
  print(n=300)


# gam models -----

## gam model 1 -----
modelo_gam <- mgcv::gam(
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

modelo_gamm <- mgcv::gamm(
  Morosidad ~ s(Tasa_interes_activa, Liquidez, k = 5) +
    s(Var_ln_IMAE, k = 5) +
    s(Var_ln_IPC, k = 5),
  family = quasibinomial(link = "logit"),
  correlation = corARMA(p=1, q=0),
  method = "REML",
  data = banca
)

summary(modelo_gamm$gam)

residuos_gamm <- resid(modelo_gamm$lme, type = "normalized")
acf(residuos_gamm, main = "ACF de Residuos Normalizados AR(1)")

#nop, this nop

## gam model 3 time ----

modelo_gam_tiempo <- gam(Morosidad ~ s(Tasa_interes_activa, k=5) +
                           s(Var_ln_IMAE, k=5) +
                           s(Var_ln_IPC, k=5) +
                           s(Tiempo, k=30) +
                           Liquidez,
                         family = quasibinomial(link = "logit"),
                         data = banca, method = "REML")

summary(modelo_gam_tiempo)

residuos_gam_tiempo <- stats::residuals(modelo_gam_tiempo, type = "deviance")
rho_est_tiempo <- stats::acf(residuos_gam_tiempo, plot = TRUE)$acf[2]

## brm model 4 ----

# set.seed(57971643)
# modelo_bayesiano <- brm(
#   bf(Morosidad ~ s(Tasa_interes_activa, k=5) +
#        s(Var_ln_IMAE, k=5) +
#        s(Var_ln_IPC, k=5) +
#        Liquidez +
#        ar(p=1)),
#   data = banca,
#   family = Beta(link = "logit"),
#   chains = 4,
#   cores = 4,
#   iter = 4000,          
#   warmup = 1000,
#   control = list(adapt_delta = 0.95, max_treedepth = 12),
#   seed = 123
# )
# 
# summary(modelo_bayesiano)
# 
# plot(modelo_bayesiano)
# 
# pp_check(modelo_bayesiano)
# 
# residuos <- residuals(modelo_bayesiano, type = "pearson")
# acf(residuos[,"Estimate"], main = "ACF residuos modelo bayesiano")


## gamm model 5 ----

modelo_dummies <- mgcv::gamm(
  Morosidad ~ s(Tasa_interes_activa_real, k=4) +
    s(Var_ln_IMAE, k=4) +
    s(Liquidez, k=4) +
    Crisis_2018 + 
    Crisis_covid,
  correlation = corARMA(p=1, q=0, form = ~ Tiempo),
  method = "REML",
  data = banca
)
summary(modelo_dummies$gam)


residuos_dummies <- resid(modelo_dummies$lme, type = "normalized")
acf(residuos_dummies, main = "ACF residuos GAMM")

mgcv::concurvity(modelo_dummies$gam, full = FALSE)

## gamm model 6  ----

modelo_gamm_real <- mgcv::gamm(
  Morosidad ~ s(Tasa_interes_activa_real, k = 5) +
    s(Var_ln_IMAE, k = 5) +
    s(Liquidez, k=5),
  correlation = corARMA(p = 1, q = 0),
  method = "REML",
  data = banca
)

summary(modelo_gamm_real$gam)

residuos_gamm_real <- resid(modelo_gamm_real$lme, type = "normalized")
acf(residuos_gamm_real, main = "ACF de Residuos Normalizados AR(1)")

gratia::draw(modelo_gamm_real$gam, select = "s(Liquidez)", residuals = TRUE)

## gamm model 7 ----

modelo_inter <- mgcv::gamm(
  Morosidad ~ te(Liquidez, Var_ln_IMAE, k=5) +
    s(Tasa_interes_activa_real, k=4),
  correlation = corARMA(p=1, q=0),
  method = "REML",
  data = banca
)

summary(modelo_inter$gam)

residuos_inter <- resid(modelo_inter$lme, type = "normalized")
acf(residuos_inter, main = "ACF residuos GAMM")

mgcv::concurvity(modelo_inter$gam, full = FALSE)

gratia::draw(modelo_inter$gam, select = "te(Liquidez,Var_ln_IMAE)", residuals = TRUE)

## gamm model 8 ----

modelo_inter_tasa <- mgcv::gamm(
  Morosidad ~ te(Liquidez, Tasa_interes_activa_real, k = 5) +
    s(Var_ln_IMAE, k = 5),
  correlation = corARMA(p = 1, q = 0),
  method = "REML",
  data = banca
)

summary(modelo_inter_tasa$gam)

residuos_inter_tasa <- resid(modelo_inter_tasa$lme, type = "normalized")
acf(residuos_inter_tasa, main = "ACF residuos GAMM")

mgcv::concurvity(modelo_inter_tasa$gam, full = FALSE)

gratia::draw(modelo_inter_tasa$gam, select = "te(Liquidez,Tasa_interes_activa_real)", residuals = TRUE)

summary(modelo_inter_tasa$lme)$modelStruct$corStruct
tseries::adf.test(residuos_inter_tasa)

## gamm model 9 ----

modelo_inter_dos <- mgcv::gamm(
  Morosidad ~ te(Liquidez, Tasa_interes_activa_real,Var_ln_IMAE, k = 5),
  # family = quasibinomial(link = "logit"),
  correlation = corARMA(p = 1, q = 0),
  method = "REML",
  data = banca
)

summary(modelo_inter_dos)
summary(modelo_inter_dos$gam)
#summary(modelo_inter_dos$lme)

# mgcv::gam.check(modelo_inter_dos$gam)
#about k check test the p-value is false
#modelo_inter_dos has k=5x5x5=125 available parameters but the edf choose k=14.5 

residuos_inter_dos <- resid(modelo_inter_dos $lme, type = "normalized")
valores_ajustados_inter_dos <- fitted(modelo_inter_dos$gam)

#h0: the distribution of the residuals is normal
stats::shapiro.test(residuos_inter_dos)
#h0: the variance of the residuals is constant
lmtest::bptest(residuos_inter_dos~ valores_ajustados_inter_dos)

acf(residuos_inter_dos, main = "ACF residuos GAMM")

mgcv::concurvity(modelo_inter_dos$gam, full = FALSE)

gratia::draw(modelo_inter_dos$gam, select = "te(Liquidez,Tasa_interes_activa_real,Var_ln_IMAE)", residuals = TRUE)

summary(modelo_inter_dos$lme)$modelStruct$corStruct
tseries::adf.test(residuos_inter_dos)

## gamm model 10 ----

names(banca_ni)

modelo_inter_dos_estacionario <- mgcv::gamm(
Var_Morosidad ~ te(Var_Liquidez, Var_Tasa_interes_activa,
               Var_ln_IMAE, k = 5),
# family = scat(link = "identity"),
# correlation = corARMA(p = 1, q = 0),
method = "REML",
data = banca_ni
)

summary(modelo_inter_dos_estacionario)
summary(modelo_inter_dos_estacionario$gam)
#summary(modelo_inter_dos_estacionario$lme)

mgcv::gam.check(modelo_inter_dos_estacionario$gam)

residuos_inter_dos_estacionario <- resid(modelo_inter_dos_estacionario$lme, type = "normalized")
valores_ajustados_inter_dos_estacionario <- fitted(modelo_inter_dos_estacionario$gam)

#h0: the distribution of the residuals is normal
stats::shapiro.test(residuos_inter_dos_estacionario)
#h0: the variance of the residuals is constant
lmtest::bptest(residuos_inter_dos_estacionario~ valores_ajustados_inter_dos_estacionario)

acf(residuos_inter_dos_estacionario, main = "ACF residuos GAMM")

mgcv::concurvity(modelo_inter_dos_estacionario$gam, full = FALSE)

gratia::draw(modelo_inter_dos_estacionario$gam, select = "te(Var_Liquidez,Var_Tasa_interes_activa,Var_ln_IMAE)", residuals = TRUE)

summary(modelo_inter_dos_estacionario$lme)$modelStruct$corStruct
tseries::adf.test(residuos_inter_dos_estacionario)

## gamm modelo 11 ----

modelo_comparacion <- mgcv::gamm(
  Var_Morosidad ~ te(Var_Liquidez, Var_Tasa_interes_activa, k = 5) +
    s(Var_ln_IMAE, k = 5),
  # family = mgcv::scat(link = "identity"),
  method = "REML",
  data = banca_ni
)

summary(modelo_comparacion$gam)

AIC(modelo_inter_dos_estacionario$lme, modelo_comparacion$lme)

## gam modelo 12 ----

modelo_scat <-mgcv::gam(
  Var_Morosidad ~ te(Var_Liquidez, Var_Tasa_interes_activa, k = 5) +
    s(Var_ln_IMAE, k = 5),
  family = mgcv::scat(link = "identity"),
  method = "REML",
  data = banca_ni
)
summary(modelo_scat)
gam.check(modelo_scat)
shapiro.test(residuals(modelo_scat, type = "deviance"))
acf(residuals(modelo_scat, type = "deviance"))

## gam modelo 13 ----

modelo_scat_inter_3 <-mgcv::gam(
  Var_Morosidad ~ te(Var_Liquidez, Var_Tasa_interes_activa, Var_ln_IMAE, k = 5),
    #s(Var_ln_IMAE, k = 5),
  family = mgcv::scat(link = "identity"),
  method = "REML",
  data = banca_ni
)

summary(modelo_scat_inter_3)
gam.check(modelo_scat_inter_3)
shapiro.test(residuals(modelo_scat_inter_3, type = "deviance"))
acf(residuals(modelo_scat_inter_3, type = "deviance"), main ="ACF residuos GAM, Type = 'deviance'")

rm(list = ls())