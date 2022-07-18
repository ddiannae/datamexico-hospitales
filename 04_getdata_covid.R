library(vroom)
library(dplyr)

# Aumentar el timeout para descarga
options(timeout = max(300, getOption("timeout")))

# Desgarcar datos de covid
temp <- tempfile()
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",
              temp)

# El archivo pesa alrededor de 3GB
unzip(temp,  exdir = "data", junkpaths = TRUE, unzip = "/usr/bin/unzip")
unlink(temp)

covid_data <- vroom::vroom("data/220717COVID19MEXICO.csv")

covid_data <- covid_data %>% 
  dplyr::filter(CLASIFICACION_FINAL %in% 1:3) %>% #covid confirmado
  dplyr::mutate(DEFUNCION = FECHA_DEF > "2020-01-01") %>% 
  dplyr::mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES, FECHA_SINTOMAS, DEFUNCION) %>% 
  tally() %>% 
  collect()
