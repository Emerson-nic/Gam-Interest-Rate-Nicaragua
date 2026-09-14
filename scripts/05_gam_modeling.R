#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-04"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               lmtest, #normality and heteroscedasticity tests 
               mgcv, #generalized additive models
               gratia, #tools for extracting smoothed and derivative functions
               itsadug, #additional functions for gam and autocorrelation 
               patchwork #combining charts  
)

if (!exists("banca")) {
  if (file.exists("csv/banca_nicaragua_estacionarios.csv") &
      file.exists("csv/banca_nicaragua_estacionarios_pca.csv")) {
    banca_ni <- readr::read_csv("csv/banca_nicaragua_estacionarios.csv")
    banca_pca <- readr::read_csv("csv/banca_nicaragua_estacionarios_pca.csv")
  } else {
    source("scripts/03_stationary_test.R")
    source("scripts/04_pca.R")
  }
}

#select data ----

banca_ni <- banca_ni %>%
  dplyr::select(Fecha,
                Var_Tasa_interes_activa,
                FEDFUNDS,
                Var_Morosidad,
                Var_Liquidez,
                Var_ln_IMAE,
                Var_ln_IPC,
                Var_ln_itcer,
                Var_ln_inss,
                Var_ln_cafe,
                Var_ln_banano,
                Var_ln_azucar,
                Tiempo,
                Mes,
                crisis_2008,
                crisis_2018,
                crisis_covid
  )

banca_pca <- banca_ni %>%
  dplyr::select(Fecha,
                Var_Tasa_interes_activa,
                FEDFUNDS,
                Var_Morosidad,
                Var_Liquidez,
                Var_ln_IMAE,
                Var_ln_IPC,
                Var_ln_itcer,
                Var_ln_inss,
                Var_ln_cafe,
                Var_ln_banano,
                Var_ln_azucar,
                PC1_Moneda,
                PC1_Ciclo,
                PC2_Ciclo,
                Tiempo,
                Mes,
                crisis_2008,
                crisis_2018,
                crisis_covid
  )

banca_ni_nombres <- c(
  "Fecha",
  "Var. Tasa interés activa (%)",
  "FEDFUNDS (%)",
  "Var. Morosidad",
  "Var. Liquidez",
  "Var. ln IMAE",
  "Var. ln IPC",
  "Var. ln ITCER",
  "Var. ln INSS",
  "Var. ln Cafe",
  "Var. ln Banano",
  "Var. ln Azucar",
  "Tiempo",
  "Mes",
  "Dummy 2008",
  "Dummy 2018",
  "Dummy Covid"
)

banca_pca_nombres <- c(
  "Fecha",
  "Var. Tasa interés activa (%)",
  "FEDFUNDS (%)",
  "Var. Morosidad",
  "Var. Liquidez",
  "Var. ln IMAE",
  "Var. ln IPC",
  "Var. ln ITCER",
  "Var. ln INSS",
  "Var. ln Cafe",
  "Var. ln Banano",
  "Var. ln Azucar",
  "PC1 Moneda",
  "PC1 Ciclo",
  "PC2 Ciclo",
  "Tiempo",
  "Mes",
  "Dummy 2008",
  "Dummy 2018",
  "Dummy Covid"
)

dplyr::glimpse(banca_ni)
dplyr::glimpse(banca_pca)


banca_ni %>% dplyr::select(Var_Tasa_interes_activa, FEDFUNDS,
                        Var_ln_IPC) %>%
  tibble::as_tibble() %>%
  print(n=300)

# std gam models -----

## model 1 all variables as spins with shrinkage ----
modelo_1 <- mgcv::gam(
  Var_Morosidad ~ s(Var_Liquidez, k = 5) +
    s(Var_Tasa_interes_activa, k = 5) +
    s(Var_ln_IMAE, k = 5) +
    s(Var_ln_IPC, k = 5) +
    s(Var_ln_itcer, k = 5) +
    s(Var_ln_cafe, k = 5) +
    s(Var_ln_azucar, k = 5) +
    s(Var_ln_banano, k = 5) +
    s(Var_ln_inss, k = 5) +
    s(FEDFUNDS, k = 5) +
    crisis_2008 + crisis_2018 + crisis_covid,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  select = TRUE, #automatic shrinkage per term
  data = banca_ni
)


#edf (effective degrees of freedom)
#if edf = 1 straight line
#if edf > 1 the impact is curved

#Ref.df (reference degrees of freedom)
#chi.sq + ref.df is used to check whether that 
#form is statistically significant

summary(modelo_1)
gam.check(modelo_1)

#concurvity() measures the equivalent of multicollinearity. 
#the metric ranges from 0 (perfect) to 1 (total redundancy).
mgcv::concurvity(modelo_1, full = T)

#autocorrelation
residuos_modelo_1 <- stats::resid(modelo_1, type = "deviance")
stats::acf(residuos_modelo_1, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_1 <- stats::fitted(modelo_1)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_1, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_1~ valores_ajustados_modelo_1)

#draw residuals
gratia::draw(modelo_1, select = "s(Var_Liquidez)", residuals = TRUE)

#plotting variable effects
gratia::draw(modelo_1, ci_level=0.95, select = "s(Var_Liquidez)", residuals = F)
gratia::draw(modelo_1, ci_level=0.95, select = "s(Var_ln_IMAE)", residuals = F)

#extract marginal effect
derivadas_modelo_1 <- gratia::derivatives(modelo_1)
print(derivadas_modelo_1)
gratia::draw(derivadas_modelo_1)

## model 2 spines with shrinkage in some variables ----
modelo_2 <-mgcv::gam(
  Var_Morosidad ~ 
    s(Var_Tasa_interes_activa, k = 5) +             
    s(Var_Liquidez, k = 5) +                          
    s(Var_ln_IMAE, k = 5, bs = "ts") + # bs="ts" = shrinkage
    s(Var_ln_IPC, k = 5, bs = "ts") +
    s(Var_ln_itcer, k = 5, bs = "ts") +
    s(Var_ln_cafe, k = 5, bs = "ts") +
    s(Var_ln_azucar, k = 5, bs = "ts") +
    s(Var_ln_banano, k = 5, bs = "ts") +
    s(Var_ln_inss, k = 5, bs = "ts") +
    s(FEDFUNDS, k = 5, bs = "ts") +
    crisis_2008 + crisis_2018 + crisis_covid,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  data = banca_ni
)

summary(modelo_2)
gam.check(modelo_2)

#autocorrelation
residuos_modelo_2<- stats::resid(modelo_2, type = "deviance")
stats::acf(residuos_modelo_2, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_2<- stats::fitted(modelo_2)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_2, type = "deviance"))
gratia::appraise(modelo_2)
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_2~ valores_ajustados_modelo_2)

mgcv::concurvity(modelo_2, full = T)

#draw residuals
gratia::draw(modelo_2, select = "te(Var_Liquidez,Var_ln_IMAE)", residuals = TRUE)

#plotting variable effects
gratia::draw(modelo_2, ci_level=0.95, select = "te(Var_Liquidez,Var_ln_IMAE)", residuals = F)

#note: The residuals in grita::drawn() generate the same plot 
#regardless of whether the value is true or false, only with te()

#extract marginal effect
# derivadas_modelo_2 <- gratia::derivatives(modelo_2)
# print(derivadas_modelo_2)
# gratia::draw(derivadas_modelo_2)

## model 3 with pca----
message("PCA1 moneda variables are Var_ln_IPC & Var_ln_itcer")
message("PCA1 and PCA2 ciclo variables are Var_ln_IMAE, Var_ln_inss & FEDFUNDS")
modelo_pca_1 <-mgcv::gam(
  Var_Morosidad ~ 
    s(Var_Tasa_interes_activa, k = 5) +             
    s(Var_Liquidez, k = 5) +                          
    s(PC1_Ciclo, k = 5, bs = "ts") + # bs="ts" = shrinkage
    s(PC2_Ciclo, k = 5, bs = "ts") +
    s(PC1_Moneda, k = 5, bs = "ts") +
    s(Var_ln_cafe, k = 5, bs = "ts") +
    s(Var_ln_azucar, k = 5, bs = "ts") +
    s(Var_ln_banano, k = 5, bs = "ts") +
    crisis_2008 + crisis_2018 + crisis_covid,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  data = banca_pca
)
summary(modelo_pca_1)
gam.check(modelo_pca_1)

#autocorrelation
residuos_modelo_pca_1<- stats::resid(modelo_pca_1, type = "deviance")
stats::acf(residuos_modelo_pca_1, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_pca_1<- stats::fitted(modelo_pca_1)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_pca_1, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_pca_1~ valores_ajustados_modelo_pca_1)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_pca_1)

## model 4 with pca and only dummy 2008 ----

modelo_pca_2008 <-mgcv::gam(
  Var_Morosidad ~ 
    s(Var_Tasa_interes_activa, k = 5) +             
    s(Var_Liquidez, k = 5) +                          
    s(PC1_Ciclo, k = 5, bs = "ts") + # bs="ts" = shrinkage
    s(PC2_Ciclo, k = 5, bs = "ts") +
    s(PC1_Moneda, k = 5, bs = "ts") +
    s(Var_ln_cafe, k = 5, bs = "ts") +
    s(Var_ln_azucar, k = 5, bs = "ts") +
    s(Var_ln_banano, k = 5, bs = "ts") +
    crisis_2008,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  data = banca_pca
)
summary(modelo_pca_2008)
gam.check(modelo_pca_2008)

#autocorrelation
residuos_modelo_pca_2008 <- stats::resid(modelo_pca_2008 , type = "deviance")
stats::acf(residuos_modelo_pca_2008, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_pca_2008 <- stats::fitted(modelo_pca_2008)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_pca_2008, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_pca_2008 ~ valores_ajustados_modelo_pca_2008)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_pca_2008)

## model 5 te(cafe, azucar, banano) & without pc2_ciclo ----

modelo_te_commodities <-mgcv::gam(
  Var_Morosidad ~ 
    s(Var_Tasa_interes_activa, k = 5) +             
    s(Var_Liquidez, k = 5) +                          
    s(PC1_Ciclo, k = 5, bs = "ts") + 
    s(PC1_Moneda, k = 5, bs = "ts") +
    te(Var_ln_cafe, Var_ln_azucar,Var_ln_banano, k = 5, bs = "ts") +
    crisis_2008,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  data = banca_pca
)
summary(modelo_te_commodities)
gam.check(modelo_te_commodities)

#autocorrelation
residuos_modelo_te_commodities <- stats::resid(modelo_te_commodities , type = "deviance")
stats::acf(residuos_modelo_te_commodities, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_te_commodities <- stats::fitted(modelo_te_commodities)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_te_commodities, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_te_commodities ~ valores_ajustados_modelo_te_commodities)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_te_commodities)

## model 6 only variables of interest ----

modelo_interest <-mgcv::gam(
  Var_Morosidad ~ 
    s(Var_Tasa_interes_activa, k = 5, bs = "ts") +             
    s(Var_Liquidez, k = 5, bs = "ts") +                          
    s(PC1_Ciclo, k = 5, bs = "ts") + 
    s(PC1_Moneda, k = 5, bs = "ts") +
    crisis_2008,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  data = banca_pca
)
summary(modelo_interest)
gam.check(modelo_interest)

#autocorrelation
residuos_modelo_interest <- stats::resid(modelo_interest , type = "deviance")
stats::acf(residuos_modelo_interest, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_interest <- stats::fitted(modelo_interest)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_interest, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_interest ~ valores_ajustados_modelo_interest)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_interest)

## model 7 only p-value < 0.05 ----

modelo_p_value <-mgcv::gam(
  Var_Morosidad ~ 
    s(PC1_Ciclo, k = 5) + 
    s(PC1_Moneda, k = 5) +
    crisis_2008,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  data = banca_pca
)
summary(modelo_p_value)
gam.check(modelo_p_value)

#autocorrelation
residuos_modelo_p_value <- stats::resid(modelo_p_value , type = "deviance")
stats::acf(residuos_modelo_p_value, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_p_value <- stats::fitted(modelo_p_value)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_p_value, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_p_value ~ valores_ajustados_modelo_p_value)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_p_value)

#draw residuals
gratia::draw(modelo_p_value, select = "s(PC1_Ciclo)", residuals = TRUE)
gratia::draw(modelo_p_value, select = "s(PC1_Moneda)", residuals = TRUE)

#plotting variable effects
gratia::draw(modelo_p_value, ci_level=0.95, select = "s(PC1_Ciclo)", residuals = F)
gratia::draw(modelo_p_value, ci_level=0.95, select = "s(PC1_Moneda)", residuals = F)

#extract marginal effect
derivadas_modelo_p_value <- gratia::derivatives(modelo_p_value)
print(derivadas_modelo_p_value)
gratia::draw(derivadas_modelo_p_value)

## model 8 only p-value < 0.05 te() ----

modelo_p_te <-mgcv::gam(
  Var_Morosidad ~ 
    te(PC1_Ciclo, PC1_Moneda, k = 5) + 
    crisis_2008,
  family = mgcv::scat(link = "identity"),
  # method = "REML",
  method = "ML",
  data = banca_pca
)
summary(modelo_p_te)
gam.check(modelo_p_te)

#autocorrelation
residuos_modelo_p_te <- stats::resid(modelo_p_te , type = "deviance")
stats::acf(residuos_modelo_p_te, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_p_te <- stats::fitted(modelo_p_te)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_p_te, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_p_te ~ valores_ajustados_modelo_p_te)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_p_te)


### compare models ----

message("to campare the models use method = 'ML', not  method = 'REML' ")

stats::AIC(modelo_1, modelo_2, modelo_pca_1, modelo_pca_2008, 
           modelo_te_commodities, modelo_interest, modelo_p_value, 
           modelo_p_te)
# > stats::AIC(modelo_1, modelo_2, modelo_pca_1, modelo_pca_2008, 
#              +            modelo_te_commodities, modelo_interest, modelo_p_value, 
#              +            modelo_p_te)
# df       AIC
# modelo_1              15.65592 -245.1660
# modelo_2              15.54525 -239.3491
# modelo_pca_1          15.47452 -239.2016
# modelo_pca_2008       14.55187 -242.8451
# modelo_te_commodities 14.84642 -239.1418
# modelo_interest       10.42471 -242.6524
# modelo_p_value        10.32291 -243.0963
# modelo_p_te           12.04707 -243.8419

stats::BIC(modelo_1, modelo_2, modelo_pca_1, modelo_pca_2008, 
           modelo_te_commodities, modelo_interest, modelo_p_value, 
           modelo_p_te)
# > stats::BIC(modelo_1, modelo_2, modelo_pca_1, modelo_pca_2008, 
#              +            modelo_te_commodities, modelo_interest, modelo_p_value, 
#              +            modelo_p_te)
# df       BIC
# modelo_1              15.65592 -192.1786
# modelo_2              15.54525 -186.7363
# modelo_pca_1          15.47452 -186.8281
# modelo_pca_2008       14.55187 -193.5943
# modelo_te_commodities 14.84642 -188.8941
# modelo_interest       10.42471 -207.3700
# modelo_p_value        10.32291 -208.1584
# modelo_p_te           12.04707 -203.0687

# simulation ----

#create a massive grid simulating 50x50x50 possible combinations  
#within the Bank of Nicaragua's historical range
cuadricula <- expand.grid(
  Liquidez = seq(min(banca$Liquidez, na.rm = TRUE), max(banca$Liquidez, na.rm = TRUE), length.out = 50),
  Tasa_interes_activa_real = seq(min(banca$Tasa_interes_activa_real, na.rm = TRUE), max(banca$Tasa_interes_activa_real, na.rm = TRUE), length.out = 50),
  Var_ln_IMAE = seq(min(banca$Var_ln_IMAE, na.rm = TRUE), max(banca$Var_ln_IMAE, na.rm = TRUE), length.out = 50)
)

#gam predicts the expected delinquency for each cross-section
cuadricula$Morosidad_Proyectada <- predict(modelo_inter_dos$gam, newdata = cuadricula)

#take the lowest delinquency rate 
escenario_optimo <- cuadricula[which.min(cuadricula$Morosidad_Proyectada), ]

print(escenario_optimo)

#save csv 

names(cuadricula)

escenarios_nombres <- c(
  "Liquidez (prop.)",
  "Tasa interés activa real (%)",
  "Var. ln IMAE",
  "Morosidad proyectada (prop.)"
)

cuadricula <- cuadricula %>%
  dplyr::rename(
    "Liquidez (prop.)" = Liquidez,
    "Tasa interés activa real (%)" = Tasa_interes_activa_real,
    "Var. ln IMAE" = Var_ln_IMAE,
    "Morosidad proyectada (prop.)" = Morosidad_Proyectada   
  )

cuadricula <- cuadricula * 100

readr::write_csv(cuadricula, "csv/estimacion_morosidad.csv")