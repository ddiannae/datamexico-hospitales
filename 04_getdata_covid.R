library(vroom)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(zoo)

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

## Obtener Cumulative case fatality rate (CFR) por municipio y ola de covid
# Seleccionar registros con status de  covid confirmado. 
# Agrupar por municipio y fecha de síntomas
letalidad_mun_ola <- covid_data %>%
  filter(CLASIFICACION_FINAL %in% 1:3) %>% #covid confirmado
  mutate(DEFUNCION = FECHA_DEF > "2020-01-01") %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES, FECHA_SINTOMAS, DEFUNCION) %>% 
  tally() 

# Clasificar en olas temporales de acuerdo a la semana de síntomas 
letalidad_mun_ola <- 
  letalidad_mun_ola %>% 
  mutate(semana_epi_sintomas = str_pad(string = epiweek(FECHA_SINTOMAS), width = 2, side = "left", pad = 0),
         annum               = year(FECHA_SINTOMAS)
  ) %>%
  mutate(semana_epi_sintomas = paste0(annum, "-", semana_epi_sintomas)) %>%
  mutate(semana_epi_sintomas = ifelse(FECHA_SINTOMAS=="2022-01-01", "2021-52", semana_epi_sintomas)
  ) %>%
  mutate(semana_epi_sintomas = ifelse(semana_epi_sintomas=="2021-53", "2020-53", semana_epi_sintomas)) %>%
  mutate(semana_epi_sintomas  = as.factor(semana_epi_sintomas)) %>%
  #24 35 57 68 86 94 111 numericos de fronteras de olas
  mutate(ola = case_when(as.numeric(semana_epi_sintomas) < 24 ~ "01_ola_1",
                         as.numeric(semana_epi_sintomas) < 35 ~ "02_interola_1",
                         as.numeric(semana_epi_sintomas) < 57 ~ "03_ola_2",
                         as.numeric(semana_epi_sintomas) < 68 ~ "04_interola_2",
                         as.numeric(semana_epi_sintomas) < 86 ~ "05_ola_3",
                         as.numeric(semana_epi_sintomas) < 94 ~ "06_interola_3",
                         as.numeric(semana_epi_sintomas) < 111 ~ "07_ola_4",
  )
  ) %>%
  filter(as.numeric(semana_epi_sintomas) < 111) %>%
  mutate(ENTIDAD_RES = str_pad(string = ENTIDAD_RES, width = 2, side = "left", pad = 0),
         MUNICIPIO_RES = str_pad(string = MUNICIPIO_RES, width = 3, side = "left", pad = 0)
  )

# Agrupar casos de acuerdo a la clave geográfica y ola de clasificación
letalidad_mun_ola_tally <- 
  letalidad_mun_ola %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(CVEGEO, ola, DEFUNCION) %>% 
  summarise(n = sum(n))

# Obtener cfr (Cumulative case fatality rate)
letalidad_mun_ola_cfr <- 
  letalidad_mun_ola_tally %>% 
  ungroup() %>% 
  complete(CVEGEO, ola, DEFUNCION, fill=list(n=0)) %>% 
  group_by(CVEGEO, ola) %>% 
  mutate(casos_totales = sum(n)) %>% 
  mutate(pc = 100*n/sum(n)) %>% 
  filter(DEFUNCION == 1) %>% 
  rename(cfr = pc)

# Guardad datos
letalidad_mun_ola_cfr %>% 
  select(-DEFUNCION) %>% 
  rename(defunciones = n) %>% 
  vroom_write("data/letalidad_por_olas_covid.txt")

## Obtener pruebas realizadas en cada ola de covid
# Seleccionar registros con pruebas realizadas en laboratorio
# Agrupar por municipio y fecha de síntomas
pruebas_ola <- 
  covid_data %>% 
  filter(CLASIFICACION_FINAL %in% c(3,7)) %>% #prueba de laboratorio
  mutate(DEFUNCION = FECHA_DEF > "2020-01-01") %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES, FECHA_SINTOMAS) %>% 
  tally() %>% 
  collect()

# Clasificar en olas temporales de acuerdo a la semana de síntomas 
pruebas_ola <- 
  pruebas_ola %>% 
  mutate(semana_epi_sintomas = str_pad(string = epiweek(FECHA_SINTOMAS), width = 2, side = "left", pad = 0),
         annum               = year(FECHA_SINTOMAS)
  ) %>%
  mutate(semana_epi_sintomas = paste0(annum, "-", semana_epi_sintomas)) %>%
  mutate(semana_epi_sintomas = ifelse(FECHA_SINTOMAS=="2022-01-01", "2021-52", semana_epi_sintomas)
  ) %>%
  mutate(semana_epi_sintomas = ifelse(semana_epi_sintomas=="2021-53", "2020-53", semana_epi_sintomas)) %>%
  mutate(semana_epi_sintomas  = as.factor(semana_epi_sintomas)) %>%
  #24 35 57 68 86 94 111 numericos de mis fronteras de olas
  mutate(ola = case_when(as.numeric(semana_epi_sintomas) < 24 ~ "01_ola_1",
                         as.numeric(semana_epi_sintomas) < 35 ~ "02_interola_1",
                         as.numeric(semana_epi_sintomas) < 57 ~ "03_ola_2",
                         as.numeric(semana_epi_sintomas) < 68 ~ "04_interola_2",
                         as.numeric(semana_epi_sintomas) < 86 ~ "05_ola_3",
                         as.numeric(semana_epi_sintomas) < 94 ~ "06_interola_3",
                         as.numeric(semana_epi_sintomas) < 111 ~ "07_ola_4",
  )
  ) %>%
  mutate(dias_ola = 
           case_when(as.numeric(semana_epi_sintomas) < 24 ~ (24-9)*7, #empezo la semana 9 
                     as.numeric(semana_epi_sintomas) < 35 ~ (35-24)*7,
                     as.numeric(semana_epi_sintomas) < 57 ~ (57-35)*7,
                     as.numeric(semana_epi_sintomas) < 68 ~ (68-57)*7,
                     as.numeric(semana_epi_sintomas) < 86 ~ (86-68)*7,
                     as.numeric(semana_epi_sintomas) < 94 ~ (94-86)*7,
                     as.numeric(semana_epi_sintomas) < 111 ~ (111-94)*7,
           )
  ) %>%
  filter(as.numeric(semana_epi_sintomas) < 111) %>%
  mutate(ENTIDAD_RES = str_pad(string = ENTIDAD_RES, width = 2, side = "left", pad = 0),
         MUNICIPIO_RES = str_pad(string = MUNICIPIO_RES, width = 3, side = "left", pad = 0)
  )

# Obtener número de pruebas promedio realizadas al día en 
# cada ola
pruebas_ola_tally <- 
  pruebas_ola %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(CVEGEO, ola, dias_ola) %>% 
  summarise(n = sum(n)) %>% 
  mutate(pruebas_dia_promedio = n/dias_ola)

# Guardar datos
pruebas_ola_tally %>% 
  vroom_write("data/pruebas_por_ola.txt")
