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
  method = "REML",
  # method = "ML",
  data = banca_pca
)
summary(modelo_p_value)
gam.check(modelo_p_value)

mgcv::concurvity(modelo_p_value, full = T)

#autocorrelation
residuos_modelo_p_value <- stats::resid(modelo_p_value , type = "deviance")
stats::acf(residuos_modelo_p_value, main = "ACF residuos GAM modelo 1")

message("h0: no autocorrelation")
Box.test(residuos_modelo_p_value, lag = 1,  type = "Ljung-Box")
Box.test(residuos_modelo_p_value, lag = 6,  type = "Ljung-Box")
Box.test(residuos_modelo_p_value, lag = 12, type = "Ljung-Box")

valores_ajustados_modelo_p_value <- stats::fitted(modelo_p_value)

message("h0: the distribution of the residuals is normal")
stats::shapiro.test(residuals(modelo_p_value, type = "deviance"))
gratia::appraise(modelo_p_value)
message("h0: the variance of the residuals is constant")
lmtest::bptest(residuos_modelo_p_value ~ valores_ajustados_modelo_p_value)
message("ho: the residuals are not stationary")
tseries::adf.test(residuos_modelo_p_value)

#draw residuals
p_res_ciclo  <- gratia::draw(modelo_p_value, select = "s(PC1_Ciclo)", residuals = TRUE)
p_res_moneda <- gratia::draw(modelo_p_value, select = "s(PC1_Moneda)", residuals = TRUE)

ggplot2::ggsave("plots/residuos_pc1_ciclo.pdf",  plot = p_res_ciclo,  width = 8, height = 5, device = "pdf")
ggplot2::ggsave("plots/residuos_pc1_moneda.pdf", plot = p_res_moneda, width = 8, height = 5, device = "pdf")

#plotting variable effects
p_eff_ciclo  <- gratia::draw(modelo_p_value, ci_level=0.95, select = "s(PC1_Ciclo)", residuals = F)
p_eff_moneda <- gratia::draw(modelo_p_value, ci_level=0.95, select = "s(PC1_Moneda)", residuals = F)

ggplot2::ggsave("plots/efecto_pc1_ciclo.pdf",  plot = p_eff_ciclo,  width = 8, height = 5, device = "pdf")
ggplot2::ggsave("plots/efecto_pc1_moneda.pdf", plot = p_eff_moneda, width = 8, height = 5, device = "pdf")

#extract marginal effect
derivadas_modelo_p_value <- gratia::derivatives(modelo_p_value)
print(derivadas_modelo_p_value)

readr::write_csv(derivadas_modelo_p_value, "csv/derivas_modelo.csv")

p_derivadas <- gratia::draw(derivadas_modelo_p_value)
ggplot2::ggsave("plots/derivadas_marginales.pdf", plot = p_derivadas, width = 9, height = 5, device = "pdf")

#extract marginal statistically significant

derivadas <- derivadas_modelo_p_value %>%
  dplyr::filter(.lower_ci > 0 | .upper_ci < 0)

derivas_ciclo <- derivadas %>%
  dplyr::filter(.smooth == "s(PC1_Ciclo)") %>%
  dplyr::select(PC1_Ciclo, .derivative, .se , .lower_ci, .upper_ci)

readr::write_csv(derivas_ciclo, "csv/derivas_ciclo.csv")

#note: the ciclo variable's range is from 0.42 to 4.3

derivas_moneda <- derivadas %>%
  dplyr::filter(.smooth == "s(PC1_Moneda)") %>%
  dplyr::select(PC1_Moneda, .derivative, .se , .lower_ci, .upper_ci)

readr::write_csv(derivas_moneda, "csv/derivas_moneda.csv")

#note: the moneda variable's range is from -4.5 to 0.75

message("for a scenario analysis, both variables must be significant")

#forecast ----

#pca 
pca_moneda_pred <- stats::prcomp(~ Var_ln_IPC + Var_ln_itcer,
                                 data = banca_pca,
                                 center = TRUE, scale. = TRUE)

summary(pca_moneda_pred)
pca_moneda_pred$rotation

pca_ciclo_pred <- stats::prcomp(~ Var_ln_IMAE + Var_ln_inss + FEDFUNDS,
                                data = banca_pca,
                                center = TRUE, scale. = TRUE)

summary(pca_ciclo_pred)
pca_ciclo_pred$rotation


# ## base scenario
# 
# base_moneda <- data.frame(
#   Var_ln_IPC = 0.008, 
#   Var_ln_itcer = -0.002
# )
# 
# base_ciclo <- data.frame(
#   Var_ln_IMAE = 0.010,
#   Var_ln_inss = 0.005,
#   FEDFUNDS = 4.50     
# )
# 
# pc1_moneda_base <- predict(pca_moneda_pred, newdata = base_moneda)[, "PC1"]
# pc1_ciclo_base  <- predict(pca_ciclo_pred,  newdata = base_ciclo)[, "PC1"]
# 
# ## forecast in differences
# pronostico_diff <- predict(
#   modelo_p_value,
#   newdata = data.frame(PC1_Ciclo = pc1_moneda_base, PC1_Moneda = pc1_ciclo_base),
#   se.fit = TRUE
# )
# 
# summary(pronostico_diff)
# pronostico_diff$fit #projected change in delinquencies
# pronostico_diff$se.fit  #standard error 
# 
# ## make growth rate to nominal
# ultimo_digito <- dplyr::last(banca_pca$Var_Morosidad)  
# 
# morosidad_nivel_proyectada <- ultimo_digito + pronostico_diff$fit
# ic_inferior <- ultimo_digito + (pronostico_diff$fit - 1.96 * pronostico_diff$se.fit)
# ic_superior <- ultimo_digito + (pronostico_diff$fit + 1.96 * pronostico_diff$se.fit)
# 
# morosidad_nivel_proyectada
# ic_inferior
# ic_superior
# 
# 
# escenario <- data.frame(
#   escenario = "Base",
#   resultado = morosidad_nivel_proyectada,
#   ci_inf = ic_inferior,
#   ci_sup = ic_superior
# )
# 
# rownames(escenario) <- NULL
# 
# tibble::as_tibble(escenario)

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

## base
base_moneda <- data.frame(
  Var_ln_IPC = 0.008, 
  Var_ln_itcer = -0.002
)

base_ciclo <- data.frame(
  Var_ln_IMAE = 0.010,
  Var_ln_inss = 0.005,
  FEDFUNDS = 4.50     
)

## adv
base_moneda_adv <- data.frame(
  Var_ln_IPC = 0.08, 
  Var_ln_itcer = -0.02
)

base_ciclo_adv <- data.frame(
  Var_ln_IMAE = 0.10,
  Var_ln_inss = 0.05,
  FEDFUNDS = 2.50     
)

## worst
base_moneda_peor <- data.frame(
  Var_ln_IPC = 0.2, 
  Var_ln_itcer = -0.010
)

base_ciclo_peor <- data.frame(
  Var_ln_IMAE = -0.005,
  Var_ln_inss = -0.005,
  FEDFUNDS = 8.50     
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


