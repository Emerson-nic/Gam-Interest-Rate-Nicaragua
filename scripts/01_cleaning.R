#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-04"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(readxl,
               tidyverse,
               janitor,
               dplyr,
               zoo
)

#load csv ----

imae <- readr::read_csv("dataset/Producto Interno Bruto anual.csv", 
                                         skip = 6)
colnames(imae)[1] <- "fecha"

imae <- imae %>%
  dplyr::rename(imae = IMAE)

imae <- as.data.frame(imae)

tibble::as_tibble(imae)

imae <- imae %>%
  dplyr::mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"))

#also it can do like this
# imae <- imae %>%
#   mutate(fecha = dmy(fecha))  
# tibble::as_tibble(imae)

#index  
ipc <- readr::read_csv("dataset/Índice de precios al consumidor.csv", 
                       skip = 6)

colnames(ipc)[1] <- "fecha"

ipc <- ipc %>%
  dplyr::rename(ipc = "IPC general")

ipc <- ipc %>%
  dplyr::mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"))

tibble::as_tibble(ipc)

#SECMCA (BCN) interest rate in cordoba 
tasa_interes <- readr::read_csv("dataset/Tasas de interés en moneda nacional.csv", 
                                       skip = 6)

colnames(tasa_interes)[1] <- "fecha"

tasa_interes <- tasa_interes %>%
  dplyr::rename( tasa_interes_activo = "Tasa de interés activa nominal en MN")

tasa_interes <- tasa_interes %>%
  dplyr::mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"))

tibble::as_tibble(tasa_interes)


#load xlsx ----

# bancos <- readxl::read_xlsx("dataset/ib_balance_general_0.xlsx", 
#                                       sheet = "SISTEMA_BANCARIO")

#function to pivot the Siboif data (from accounting format to time series) ----

procesar_siboif <- function(file_path, sheet_name) {
  #read the file skipping th initial header 
  df_raw <- read_xlsx(file_path, sheet = sheet_name, skip = 9) %>%
    clean_names()
  
  #transponse and cleaning date
  df_clean <- df_raw %>%
    dplyr::rename(cuenta = 1) %>% 
    dplyr::filter(!is.na(cuenta)) %>% 
    tidyr::pivot_longer(cols = -cuenta, names_to = "fecha_texto", values_to = "valor") %>%
    dplyr::mutate(
      fecha_texto = stringr::str_remove(fecha_texto, "^x"),
      fecha = dmy(fecha_texto),
      fecha = floor_date(fecha, "month") 
    ) %>%
    tidyr::drop_na(fecha, valor) %>% 
    dplyr::distinct(fecha, cuenta, .keep_all = TRUE) %>%
    dplyr::select(fecha, cuenta, valor) %>%
    tidyr::pivot_wider(names_from = cuenta, values_from = valor) %>%
    clean_names()
  
  return(df_clean)
}

## applied function ----

banco_2019 <- procesar_siboif("dataset/ib_balance_general_0.xlsx", "SISTEMA_BANCARIO")
banco_2008 <- procesar_siboif("dataset/ib_balance_general.xlsx", "SISTEMA_BANCARIO")

resultados_2019 <- procesar_siboif("dataset/ib_estado_resultados_0.xlsx", "SISTEMA_BANCARIO")
resultados_2008 <- procesar_siboif("dataset/ib_estado_resultados.xlsx", "SISTEMA_BANCARIO")

tibble::as_tibble(banco_2019)
tibble::as_tibble(banco_2008)

tibble::as_tibble(resultados_2019)
tibble::as_tibble(resultados_2008)

message("All year are expressed in thousands of córdobas")

# cat("since 2019 names:", names(banco_2019))
# cat("through 2018:", names(banco_2008))
message("since 2019 names:")
names(banco_2019)

message("through 2018 names:")
names(banco_2008)

message("since 2019 names:")
names(resultados_2019)

message("through 2018 names:")
names(resultados_2008)

### calculation of financial ratios ----

#pre-2018 data

ingresos_2008 <- resultados_2008 %>%
  dplyr::select(fecha, ingresos_financieros_por_cartera_de_creditos) %>%
  dplyr::arrange(fecha) %>%
  dplyr::mutate(
    mes = lubridate::month(fecha),
    ingresos_mes = dplyr::if_else(
      mes == 1,
      ingresos_financieros_por_cartera_de_creditos,
      ingresos_financieros_por_cartera_de_creditos - dplyr::lag(ingresos_financieros_por_cartera_de_creditos)
    )
  ) %>%
  dplyr::select(fecha, ingresos_mes)

banco_2008_clean <- banco_2008 %>%
  dplyr::inner_join(ingresos_2008, by = "fecha") %>%
  dplyr::arrange(fecha) %>%
  dplyr::mutate(
    disponibilidades_total = disponibilidades,
    depositos_totales = obligaciones_con_el_publico,
    ratio_liquidez = disponibilidades_total / depositos_totales,
    cartera_mora = creditos_vencidos + creditos_en_cobro_judicial,
    cartera_bruta = cartera_de_creditos_neta + provisiones_por_incobrabilidad_de_cartera_de_creditos,
    ratio_morosidad = cartera_mora / cartera_bruta,
    tasa_implicita = (ingresos_mes * 12) / ((cartera_bruta + dplyr::lag(cartera_bruta)) / 2)
  ) %>%
  dplyr::select(fecha, ratio_liquidez, ratio_morosidad, tasa_implicita)

#post-2019 data 

ingresos_2019 <- resultados_2019 %>%
  dplyr::select(fecha, ingresos_financieros_por_cartera_de_creditos) %>%
  dplyr::arrange(fecha) %>%
  dplyr::mutate(
    mes = lubridate::month(fecha),
    ingresos_mes = dplyr::if_else(
      mes == 1,
      ingresos_financieros_por_cartera_de_creditos,
      ingresos_financieros_por_cartera_de_creditos - dplyr::lag(ingresos_financieros_por_cartera_de_creditos)
    )
  ) %>%
  dplyr::select(fecha, ingresos_mes)

banco_2019_clean <- banco_2019 %>%
  dplyr::inner_join(ingresos_2019, by = "fecha") %>%
  dplyr::arrange(fecha) %>%
  dplyr::mutate(
    disponibilidades_total = efectivo_y_equivalentes_de_efectivo,
    depositos_totales = obligaciones_con_el_publico,
    ratio_liquidez = disponibilidades_total / depositos_totales,
    cartera_mora = vencidos + cobro_judicial,
    cartera_bruta = cartera_de_creditos_neta + provision_de_cartera_de_creditos,
    ratio_morosidad = cartera_mora / cartera_bruta,
    tasa_implicita = (ingresos_mes * 12) / ((cartera_bruta + dplyr::lag(cartera_bruta)) / 2)
  ) %>%
  dplyr::select(fecha, ratio_liquidez, ratio_morosidad, tasa_implicita)

#merging both periods 
datos_bancarios <- dplyr::bind_rows(banco_2008_clean, banco_2019_clean) %>%
  dplyr::arrange(fecha) %>%
  dplyr::distinct(fecha, .keep_all = TRUE)


tibble::as_tibble(datos_bancarios)

#merging all variables ----

banca <- datos_bancarios %>%
  dplyr::select(fecha, ratio_liquidez, ratio_morosidad, tasa_implicita) %>%
  dplyr::inner_join(imae, by = "fecha") %>%
  dplyr::inner_join(ipc, by = "fecha") %>%
  dplyr::inner_join(tasa_interes, by = "fecha") %>%
  dplyr::arrange(fecha)

tibble::as_tibble(banca)

banca <- banca %>%
  dplyr::mutate(
    tasa_implicita_pct = tasa_implicita * 100,
    ratio_morosidad_pct = ratio_morosidad * 100,
    ratio_liquidez_pct = ratio_liquidez * 100
  ) %>%
  dplyr::select(fecha, tasa_interes_activo, tasa_implicita_pct, ratio_morosidad_pct, ratio_liquidez_pct, dplyr::everything())

banca <- banca %>%
  na.omit()

tibble::as_tibble(banca)

banca %>% 
  dplyr::select(tasa_interes_activo, tasa_implicita_pct) %>% 
  tibble::as_tibble() %>%
  print(n = 300)

#

readr::write_csv(banca, "csv/sitema_bancario.csv")
