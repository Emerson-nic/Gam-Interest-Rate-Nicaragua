rm(list = ls())

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
  if (file.exists("csv/banca_nicaragua_estacionarios_pca.csv")) {
    banca_pca <- readr::read_csv("csv/banca_nicaragua_estacionarios_pca.csv")
  } else {
    source("scripts/03_stationary_test.R")
    source("scripts/04_pca.R")
  }
}

#select data ----

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

# select range ----

if(F){
  "
  
  The variables range must be:
  (a) moneda [-4.5, 0.75]
  (b) ciclo [0.42, 4.3]
  
  Moneda's Rotation (a):
  
                         PC1       PC2
 moneda$Var_ln_IPC   -0.7071068 0.7071068
 moneda$Var_ln_itcer  0.7071068 0.7071068
 
  Ciclo's Rotation (b):
  
                  PC1         PC2        PC3
 Var_ln_IMAE -0.6151287 -0.48550414  0.6212105
 Var_ln_inss -0.6976334 -0.03191167 -0.7157438
 FEDFUNDS     0.3673205 -0.87365175 -0.3190741
  
  "
}

derivas_moneda <- readr::read_csv("csv/derivas_moneda.csv")
derivas_ciclo <- readr::read_csv("csv/derivas_ciclo.csv")


pca_filter <- banca_pca %>%
  dplyr::filter(PC1_Ciclo >= 0.42 & PC1_Ciclo <= 4.3, 
                   PC1_Moneda >= -4.5 & PC1_Moneda <= 0.75)

pca_filter <- pca_filter %>%
  dplyr::select(Fecha, Var_Morosidad, 
                PC1_Ciclo, Var_ln_IMAE, Var_ln_inss, FEDFUNDS,
                PC1_Moneda, Var_ln_itcer, Var_ln_IPC)

pca_filter %>% dplyr::slice_min(order_by = PC1_Moneda, n = 1) %>%
  print()

pca_filter %>% dplyr::slice_max(order_by = PC1_Moneda, n = 1) %>%
  print()

utils::tail(pca_filter, n = 12)

fecha_significativa <- zoo::as.Date(c(
  "2024-07-01", "2024-08-01", "2024-09-01", "2024-10-01", "2024-11-01",
  "2024-12-01", "2025-01-01", "2024-02-01", "2025-10-01", "2025-12-01",
  "2026-02-01", "2026-03-01"
))

banca_pca <- banca_pca %>%
  dplyr::arrange(Fecha) %>% 
  dplyr::mutate(Var_Morosidad_lag = dplyr::lag(Var_Morosidad, n = 1)) %>%
  tidyr::drop_na(Var_Morosidad_lag)

#select data by subtracting one year & pca ----

n_origenes <- 30

backtest <- purrr::map_dfr(seq_len(n_origenes), function(h) {
  
  corte <- nrow(banca_pca) - h #last one data 
  train <- banca_pca[1:corte, ] #iteracion
  test <- banca_pca[corte + 1, ] #one moth only 
  
  
  ##pca 
  pca_m <- stats::prcomp(~ Var_ln_IPC + Var_ln_itcer,
                         data = train, scale. = TRUE)
  pca_c <- stats::prcomp(~ Var_ln_IMAE + Var_ln_inss + FEDFUNDS,
                         data = train, scale. = TRUE)
  
  test_pc <- tibble::tibble(
    PC1_Moneda = as.numeric(predict(pca_m, newdata = test)[, 1]),
    PC1_Ciclo = as.numeric(predict(pca_c, newdata = test)[, 1])
  )
  
  test$PC1_Moneda <- test_pc$PC1_Moneda
  test$PC1_Ciclo <- test_pc$PC1_Ciclo
  
  ## gam model
  fit <- mgcv::gam(Var_Morosidad ~ s(Var_Morosidad_lag, k = 5) +
                     s(PC1_Ciclo, k = 5) + 
                     s(PC1_Moneda, k = 5),
                   family = mgcv::scat(link = "identity"),
                   method = "REML",
                   data = train)
  
  pred <- predict(fit, newdata = test, se.fit = TRUE)
  


  tibble::tibble(
    fecha = test$Fecha,
    real = test$Var_Morosidad,
    pred_gam = as.numeric(pred$fit),
    se_gam = as.numeric(pred$se.fit),
    pred_bm = 0
  )
})

#plot ----
backtest %>%
  dplyr::summarise(
    rmse_gam = sqrt(mean((real - pred_gam)^2)),
    rmse_bm = sqrt(mean((real - pred_bm)^2)),
    mae_gam = mean(abs(real - pred_gam)),
    mae_bm = mean(abs(real - pred_bm))
  )

out_of_sample_plot <- ggplot2::ggplot(backtest, ggplot2::aes(x = fecha)) +
  ggplot2::geom_vline(xintercept = fecha_significativa, 
                      color = "grey70", 
                      alpha = 0.4, 
                      linewidth = 3) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = pred_gam - 1.96*se_gam,
                                    ymax = pred_gam + 1.96*se_gam),
                       fill = "salmon", alpha = 0.2) +
  ggplot2::geom_line(ggplot2::aes(y = real, color = "Real")) +
  ggplot2::geom_line(ggplot2::aes(y = pred_gam, color = "GAM")) +
  ggplot2::labs(color = "", y = "Var. Morosidad", x = "")

print(out_of_sample_plot)

