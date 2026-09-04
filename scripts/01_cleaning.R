#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-04"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(readxl,
               tidyverse,
               janitor,
               dplyr
)

#load csv ----

imae <- readr::read_csv("dataset/Producto Interno Bruto anual.csv", 
                                         skip = 6)
colnames(imae)[1] <- "fecha"

tibble::as_tibble(imae)

imae <- imae %>%
  dplyr::mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"))

#also it can do like this
# imae <- imae %>%
#   mutate(fecha = dmy(fecha))  
# tibble::as_tibble(imae)

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

tibble::as_tibble(banco_2019)
tibble::as_tibble(banco_2008)

message("Both year are expressed in thousands of córdobas")

# cat("since 2019 names:", names(banco_2019))
# cat("through 2018:", names(banco_2008))
message("since 2019 names:")
names(banco_2019)

message("through 2018 names:")
names(banco_2008)

### calculation of financial ratios ----

#pre-2018 data
banco_2008_clean <- banco_2008 %>%
  dplyr::mutate(
    #variables for liquidity
    disponibilidades_total = disponibilidades,
    depositos_totales = obligaciones_con_el_publico,
    
    #calculating of liquidity ratio
    ratio_liquidez = disponibilidades_total / depositos_totales,
    
    ##calculating Mora
    cartera_mora = creditos_vencidos + creditos_en_cobro_judicial,
    cartera_bruta = cartera_de_creditos_neta + provisiones_por_incobrabilidad_de_cartera_de_creditos,
    ratio_morosidad = cartera_mora / cartera_bruta
  ) %>%
  dplyr::select(fecha, ratio_liquidez, ratio_morosidad, disponibilidades_total, depositos_totales)

#post-2019 data 
banco_2019_clean <- banco_2019 %>%
  dplyr::mutate(
    #variables for liquidity
    disponibilidades_total = efectivo_y_equivalentes_de_efectivo,
    depositos_totales = obligaciones_con_el_publico,
    
    #calculating of liquidity ratio
    ratio_liquidez = disponibilidades_total / depositos_totales,
    
    #calculating Mora
    cartera_mora = vencidos + cobro_judicial,
    cartera_bruta = cartera_de_creditos_neta + provision_de_cartera_de_creditos,
    ratio_morosidad = cartera_mora / cartera_bruta
  ) %>%
  dplyr::select(fecha, ratio_liquidez, ratio_morosidad, disponibilidades_total, depositos_totales)

#merging both periods 
datos_bancarios <- dplyr::bind_rows(banco_2008_clean, banco_2019_clean) %>%
  dplyr::arrange(fecha) %>%
  dplyr::distinct(fecha, .keep_all = TRUE)


tibble::as_tibble(datos_bancarios)

#merging all variables ----

banca <- datos_bancarios %>%
  dplyr::select(fecha, ratio_liquidez, ratio_morosidad) %>%
  dplyr::inner_join(imae, by = "fecha") %>%
  dplyr::arrange(fecha)

tibble::as_tibble(banca)
  
