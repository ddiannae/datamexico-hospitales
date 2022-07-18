library(tidyverse)
library(vroom)
library(lubridate)
library(zoo)


remota <- vroom("datos_covid.txt") #esto se lee de la pagina del gobierno 

letalidad_mun_ola <- 
  filter(CLASIFICACION_FINAL%in%1:3) %>% #covid confirmado
  mutate(DEFUNCION = FECHA_DEF > "2020-01-01") %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES, FECHA_SINTOMAS, DEFUNCION) %>% 
  tally() 

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
  filter(as.numeric(semana_epi_sintomas) < 111) %>%
  mutate(ENTIDAD_RES = str_pad(string = ENTIDAD_RES, width = 2, side = "left", pad = 0),
         MUNICIPIO_RES = str_pad(string = MUNICIPIO_RES, width = 3, side = "left", pad = 0)
  )

letalidad_mun_ola.tally <- 
  letalidad_mun_ola %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(CVEGEO, ola, DEFUNCION) %>% 
  summarise(n = sum(n))

letalidad_mun_ola.cfr <- 
  letalidad_mun_ola.tally %>% 
  ungroup() %>% 
  complete(CVEGEO, ola, DEFUNCION, fill=list(n=0)) %>% 
  group_by(CVEGEO, ola) %>% 
  mutate(casos_totales = sum(n)) %>% 
  mutate(pc = 100*n/sum(n)) %>% 
  filter(DEFUNCION==1) %>% 
  rename(cfr = pc)

letalidad_mun_ola.cfr %>% 
  select(-DEFUNCION) %>% 
  rename(defunciones = n) %>% 
  vroom_write("letalidad_por_olas_covid.txt")

################################################################################

#pruebas ---- 

pruebas_ola <- 
  remota %>% 
  filter(CLASIFICACION_FINAL%in%c(3,7)) %>% #prueba lab
  mutate(DEFUNCION = FECHA_DEF > "2020-01-01") %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES, FECHA_SINTOMAS) %>% 
  tally() %>% 
  collect()

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


pruebas_ola.total <- 
  pruebas_ola %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  mutate(n = as.numeric(n)) %>% 
  group_by(CVEGEO, ola, dias_ola) %>% 
  summarise(n = sum(n)) %>% 
  mutate(pruebas_dia_promedio = n/dias_ola)

pruebas_ola.total %>% 
  vroom_write("pruebas_por_ola.txt")
