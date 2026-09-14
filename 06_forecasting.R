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

banca_pca <- banca_pca %>%
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

# model ----

modelo_p_value <-mgcv::gam(
  Var_Morosidad ~ 
    s(PC1_Ciclo, k = 5) + 
    s(PC1_Moneda, k = 5),
  # s(Var_ln_cafe),
  # crisis_2008,
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
gratia::appraise(modelo_p_value)
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

#forecast ----

#pca 
pca_moneda_pred <- stats::prcomp(~ Var_ln_IPC + Var_ln_itcer,
                                 data = banca_pca,
                                 center = TRUE, scale. = TRUE)

pca_ciclo_pred <- stats::prcomp(~ Var_ln_IMAE + Var_ln_inss + FEDFUNDS,
                                data = banca_pca,
                                center = TRUE, scale. = TRUE)

## base scenario

base_moneda <- data.frame(
  Var_ln_IPC = 0.008, 
  Var_ln_itcer = -0.002
)

base_ciclo <- data.frame(
  Var_ln_IMAE = 0.010,
  Var_ln_inss = 0.005,
  FEDFUNDS = 4.50     
)

pc1_moneda_base <- predict(pca_moneda_pred, newdata = base_moneda)[, "PC1"]
pc1_ciclo_base  <- predict(pca_ciclo_pred,  newdata = base_ciclo)[, "PC1"]

## forecast in differences
pronostico_diff <- predict(
  modelo_p_value,
  newdata = data.frame(PC1_Ciclo = pc1_ciclo_nuevo, PC1_Moneda = pc1_moneda_nuevo),
  se.fit = TRUE
)

summary(pronostico_diff)
pronostico_diff$fit #projected change in delinquencies
pronostico_diff$se.fit  #standard error 

## make growth rate to nominal
ultimo_digito <- dplyr::last(banca_pca$Var_Morosidad)  

morosidad_nivel_proyectada <- ultimo_digito + pronostico_diff$fit
ic_inferior <- ultimo_digito + (pronostico_diff$fit - 1.96 * pronostico_diff$se.fit)
ic_superior <- ultimo_digito + (pronostico_diff$fit + 1.96 * pronostico_diff$se.fit)

morosidad_nivel_proyectada
ic_inferior
ic_superior


escenario <- data.frame(
  escenario = "Base",
  resultado = morosidad_nivel_proyectada,
  ci_inf = ic_inferior,
  ci_sup = ic_superior
)

rownames(escenario) <- NULL

tibble::as_tibble(escenario)

## funcion to forescast

pronostico_morosidad <- function(base_moneda,
                                 base_ciclo,
                                 pca_moneda = pca_moneda_pred,
                                 pca_ciclo = pca_ciclo_pred,
                                 modelo = modelo_p_value,
                                 ultimo_valor = dplyr::last(banca_pca$Var_Morosidad),
                                 nombre_escenario = "Base") {
  
  pc1_moneda_val <- predict(pca_moneda, newdata = base_moneda)[, "PC1"]
  pc1_ciclo_val  <- predict(pca_ciclo,  newdata = base_ciclo)[, "PC1"]
  
  pronostico_diff <- predict(
    modelo,
    newdata = data.frame(PC1_Ciclo = pc1_ciclo_val, PC1_Moneda = pc1_moneda_val),
    se.fit = TRUE
  )
  
  morosidad_nivel_proyectada <- ultimo_valor + pronostico_diff$fit
  ic_inferior <- ultimo_valor + (pronostico_diff$fit - 1.96 * pronostico_diff$se.fit)
  ic_superior <- ultimo_valor + (pronostico_diff$fit + 1.96 * pronostico_diff$se.fit)
  
  data.frame(
    escenario = nombre_escenario,
    resultado = as.numeric(morosidad_nivel_proyectada),
    ci_inf = as.numeric(ic_inferior),
    ci_sup = as.numeric(ic_superior)
  )
}

##applicated funtion

## the worst scenario
base_moneda <- data.frame(
  Var_ln_IPC   = 0.008, 
  Var_ln_itcer = -0.002
)

base_ciclo <- data.frame(
  Var_ln_IMAE = 0.010,
  Var_ln_inss = 0.005,
  FEDFUNDS    = 4.50     
)

lista_escenarios <- list(
  list(nombre = "Base", moneda = base_moneda, ciclo = base_ciclo),
  list(nombre = "Adverso", moneda = base_moneda_adv, ciclo = base_ciclo_adv),
  list(nombre = "El Peor", moneda = base_moneda_peor, ciclo = base_ciclo_peor)
)

escenarios <- purrr::map_dfr(lista_escenarios, function(esc) {
  pronostico_morosidad(
    base_moneda = esc$moneda,
    base_ciclo = esc$ciclo,
    nombre_escenario = esc$nombre
  )
})

tibble::as_tibble(escenarios)

# simulation ----

#create a massive grid simulating 50x50x50 possible combinations  
#within the Bank of Nicaragua's historical range
cuadricula_macro <- expand.grid(
  #PC1_Moneda
  Var_ln_IPC   = seq(min(banca_pca$Var_ln_IPC, na.rm = TRUE), 
                     max(banca_pca$Var_ln_IPC, na.rm = TRUE), length.out = 15),
  Var_ln_itcer = seq(min(banca_pca$Var_ln_itcer, na.rm = TRUE), 
                     max(banca_pca$Var_ln_itcer, na.rm = TRUE), length.out = 15),
  #PC1_Ciclo
  Var_ln_IMAE  = seq(min(banca_pca$Var_ln_IMAE, na.rm = TRUE), 
                     max(banca_pca$Var_ln_IMAE, na.rm = TRUE), length.out = 15),
  Var_ln_inss  = seq(min(banca_pca$Var_ln_inss, na.rm = TRUE), 
                     max(banca_pca$Var_ln_inss, na.rm = TRUE), length.out = 15),
  FEDFUNDS     = seq(min(banca_pca$FEDFUNDS, na.rm = TRUE), 
                     max(banca_pca$FEDFUNDS, na.rm = TRUE), length.out = 15)
)

#make PC1_Moneda & PC1_Ciclo
cuadricula_macro$PC1_Moneda <- predict(pca_moneda_pred, 
                                       newdata = cuadricula_macro[, c("Var_ln_IPC", "Var_ln_itcer")])[, "PC1"]

cuadricula_macro$PC1_Ciclo  <- predict(pca_ciclo_pred, 
                                       newdata = cuadricula_macro[, c("Var_ln_IMAE", "Var_ln_inss", "FEDFUNDS")])[, "PC1"]

#forecasting the change in delinquency (differences) and transform at the level
prediccion_gam <- predict(
  modelo_p_value,
  newdata = data.frame(
    PC1_Ciclo  = cuadricula_macro$PC1_Ciclo,
    PC1_Moneda = cuadricula_macro$PC1_Moneda
  ),
  se.fit = TRUE
)

cuadricula_macro$Var_Morosidad_Proyectada <- as.numeric(prediccion_gam$fit)
cuadricula_macro$Morosidad_Nivel_Proyectada <- as.numeric(ultimo_digito + prediccion_gam$fit)
cuadricula_macro$IC_Inferior <- as.numeric(ultimo_digito + (prediccion_gam$fit - 1.96 * prediccion_gam$se.fit))
cuadricula_macro$IC_Superior <- as.numeric(ultimo_digito + (prediccion_gam$fit + 1.96 * prediccion_gam$se.fit))


print(tibble::as_tibble(cuadricula_macro))

#save csv 

names(cuadricula_macro)

cuadricula_exportar <- cuadricula_macro %>%
  dplyr::rename(
    "Var. ln IPC" = Var_ln_IPC,
    "Var. ln ITCER"= Var_ln_itcer,
    "Var. ln IMAE" = Var_ln_IMAE,
    "Var. ln INSS" = Var_ln_inss,
    "FEDFUNDS (%)" = FEDFUNDS,
    "PC1 Moneda" = PC1_Moneda,
    "PC1 Ciclo" = PC1_Ciclo,
    "Var. Morosidad Proyectada" = Var_Morosidad_Proyectada,
    "Morosidad Nivel Proyectada" = Morosidad_Nivel_Proyectada,
    "IC Inferior" = IC_Inferior,
    "IC Superior" = IC_Superior
  )

#note: its to havy
#readr::write_csv(cuadricula_exportar, "csv/estimacion_morosidad.csv")
