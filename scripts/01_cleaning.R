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

### Calculating liquidity ----
