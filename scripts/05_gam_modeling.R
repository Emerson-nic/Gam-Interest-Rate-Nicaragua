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

# std gam models -----

## model 1 all variables as spins ----
modelo_1 <-mgcv::gam(
  Var_Morosidad ~ s(Var_Liquidez, k=5) +
    s(Var_Tasa_interes_activa, k = 5) +
    s(Var_ln_IMAE, k = 5),
  family = mgcv::scat(link = "identity"),
  method = "REML",
  # method = "ML",
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
gratia::draw(modelo_1, select = "te(Var_Liquidez,Var_ln_IMAE)", residuals = TRUE)

#plotting variable effects
gratia::draw(modelo_1, ci_level=0.95, select = "s(Var_Liquidez)", residuals = F)
gratia::draw(modelo_1, ci_level=0.95, select = "s(Var_ln_IMAE)", residuals = F)

#extract marginal effect
derivadas_modelo_1 <- gratia::derivatives(modelo_1)
print(derivadas_modelo_1)
gratia::draw(derivadas_modelo_1)

## model 2 te in liquidity & imae----
modelo_te_1 <-mgcv::gam(
  Var_Morosidad ~ te(Var_Liquidez, Var_ln_IMAE, k=5) +
    s(Var_Tasa_interes_activa, k = 5),
  family = mgcv::scat(link = "identity"),
  method = "REML",
  # method = "ML",
  data = banca_ni
)

summary(modelo_te_1)
gam.check(modelo_te_1)

#autocorrelation
residuos_modelo_te_1<- stats::resid(modelo_te_1, type = "deviance")
stats::acf(residuos_modelo_te_1, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_te_1<- stats::fitted(modelo_te_1)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_te_1, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_te_1~ valores_ajustados_modelo_te_1)

mgcv::concurvity(modelo_te_1, full = T)

#draw residuals
gratia::draw(modelo_te_1, select = "te(Var_Liquidez,Var_ln_IMAE)", residuals = TRUE)

#plotting variable effects
gratia::draw(modelo_te_1, ci_level=0.95, select = "te(Var_Liquidez,Var_ln_IMAE)", residuals = F)

#note: The residuals in grita::drawn() generate the same plot 
#regardless of whether the value is true or false, only with te()

#extract marginal effect
# derivadas_modelo_te_1 <- gratia::derivatives(modelo_te_1)
# print(derivadas_modelo_te_1)
# gratia::draw(derivadas_modelo_te_1)

## model 3 all variables  as te----
modelo_te_all <-mgcv::gam(
  Var_Morosidad ~ te(Var_Liquidez, Var_ln_IMAE, Var_Tasa_interes_activa, k=5),
  family = mgcv::scat(link = "identity"),
  method = "REML",
  # method = "ML",
  data = banca_ni
)
summary(modelo_te_all)
gam.check(modelo_te_all)

#autocorrelation
residuos_modelo_te_all<- stats::resid(modelo_te_all, type = "deviance")
stats::acf(residuos_modelo_te_all, main = "ACF residuos GAM modelo 1")

valores_ajustados_modelo_te_all<- stats::fitted(modelo_te_all)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_te_all, type = "deviance"))
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_te_all~ valores_ajustados_modelo_te_all)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_te_all)

### compare models ----

message("to campare the models use method = 'ML', not  method = 'REML' ")

stats::AIC(modelo_1, modelo_te_1, modelo_te_all)
# > stats::AIC(modelo_1, modelo_te_1, modelo_te_all)
# df       AIC
# modelo_1       9.550165 -2224.881
# modelo_te_1    8.993136 -2224.170
# modelo_te_all 19.271396 -2223.044

stats::BIC(modelo_1, modelo_te_1, modelo_te_all)
# > stats::BIC(modelo_1, modelo_te_1, modelo_te_all)
# df       BIC
# modelo_1       9.550165 -2192.603
# modelo_te_1    8.993136 -2193.774
# modelo_te_all 19.271396 -2157.909

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