library(tidyverse)
library(vroom)
library(tidymodels)
library(sf)

mun_shp <- read_sf("00mun.shp")
pop     <- vroom("conjunto_de_datos_iter_00CSV20.csv")

#pop$NOM_LOC %>% head(10)

pop.mun <- 
  pop %>% 
  filter(NOM_LOC=="Total del Municipio") %>% 
  mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
  select(CVEGEO, 
         POBTOT)

mis_datos <- fs::dir_ls(path = "data/")

recursos_salud <- vroom(mis_datos[[1]])
unidades_salud <- vroom(mis_datos[[2]])

unidades_salud <- 
  unidades_salud %>% 
  mutate(state_id = str_pad(string = state_id, width = 2, side = "left", pad = 0),
         municipality_id = str_pad(string = municipality_id, width = 5, side = "left", pad = 0),
         ) %>% 
  mutate(CVEGEO = municipality_id) %>% 
  select(CVEGEO, everything())

recursos_salud <- 
  recursos_salud %>% 
  mutate(state_id = str_pad(string = state_id, width = 2, side = "left", pad = 0),
         municipality_id = str_pad(string = municipality_id, width = 5, side = "left", pad = 0),
  ) %>% 
  mutate(CVEGEO = municipality_id) %>% 
  select(CVEGEO, everything())

unidades_salud %>% colnames()

unidades_salud.mun.camas <- 
  unidades_salud %>% 
  group_by(CVEGEO) %>% 
  summarize(beds = sum(beds, na.rm = T))

left_join(mun_shp, unidades_salud.mun.camas) %>% 
  left_join(pop.mun) %>% 
  mutate(beds_per_100khab = 100000 * beds/POBTOT) %>% 
  #filter(beds_per_100khab < 2000) %>% 
  ggplot() + 
  #geom_sf(aes(fill=beds), lwd=0.05) +
  geom_sf(aes(fill=beds_per_100khab), lwd=0.05) + 
  scale_fill_viridis_b(option = "A", 
                       #trans="pseudo_log"
                       ) + 
  theme_minimal() + 
  ggtitle("camas/100mil habitantes") #+ 
  #facet_wrap(vars(CVE_ENT), scales="free")



  
  
camas_por_100h <- 
  left_join(pop.mun, unidades_salud.mun.camas) %>% 
  mutate(beds_per_100khab = 100000 * beds/POBTOT)

camas_por_100h %>% drop_na()


################################################################################
# conteos de la db covid ----

remota <- tbl(con, "sisver_public")

letalidad_mun_ola <- 
  remota %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>% #covid confirmado
  mutate(DEFUNCION = FECHA_DEF > "2020-01-01") %>% 
  # mutate(semana_epi_sintomas = str_pad(string = epiweek(FECHA_SINTOMAS), width = 2, side = "left", pad = 0),
  #        annum               = year(FECHA_SINTOMAS)
  # ) %>% 
  # mutate(semana_epi_sintomas = paste0(annum, "-", semana_epi_sintomas)) %>% 
  # mutate(semana_epi_sintomas = ifelse(FECHA_SINTOMAS=="2022-01-01", "2021-52", semana_epi_sintomas)
  # ) %>%
  # mutate(semana_epi_sintomas = ifelse(semana_epi_sintomas=="2021-53", "2020-53", semana_epi_sintomas)) %>% 
  # mutate(semana_epi_sintomas  = as.factor(semana_epi_sintomas)) %>% 
  # # group_by(semana_epi_sintomas) %>%
  # # tally() %>%
  # # filter(semana_epi_sintomas%in%c("2020-32", "2020-43", "2021-12", "2021-23", "2021-41", "2021-49", "2022-14")) %>%
  # # mutate(semana_epi_sintomas.numerica = as.numeric(semana_epi_sintomas))
  # #24 35 57 68 86 94 111 numericos de mis fronteras de olas
  # mutate(ola = case_when(as.numeric(semana_epi_sintomas) < 24 ~ "01_ola_1", 
  #                        as.numeric(semana_epi_sintomas) < 35 ~ "02_interola_1", 
  #                        as.numeric(semana_epi_sintomas) < 57 ~ "03_ola_2",
  #                        as.numeric(semana_epi_sintomas) < 68 ~ "04_interola_2", 
  #                        as.numeric(semana_epi_sintomas) < 86 ~ "05_ola_3", 
  #                        as.numeric(semana_epi_sintomas) < 94 ~ "06_interola_3",
  #                        as.numeric(semana_epi_sintomas) < 111 ~ "07_ola_4", 
  # )
  # ) %>% 
  # filter(as.numeric(semana_epi_sintomas) < 111) %>% 
  # mutate(ENTIDAD_RES = str_pad(string = ENTIDAD_RES, width = 2, side = "left", pad = 0),
  #        MUNICIPIO_RES = str_pad(string = MUNICIPIO_RES, width = 3, side = "left", pad = 0)
  #        ) %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  group_by(ENTIDAD_RES, MUNICIPIO_RES, FECHA_SINTOMAS, DEFUNCION) %>% 
  tally() %>% 
  collect()
  
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
  arrange(desc(cfr))

left_join(mun_shp, letalidad_mun_ola.cfr) %>% 
  drop_na() %>% 
  ggplot() + 
  geom_sf(aes(geometry=geometry, fill=cfr), lwd= 0.05) + 
  scale_fill_viridis_b(option="B") + 
  facet_wrap(vars(ola)) + 
  theme_minimal()


left_join(letalidad_mun_ola.cfr, camas_por_100h) %>% 
  filter(casos_totales > 100) %>% 
  ggplot() + 
  aes(x = beds_per_100khab, y = cfr) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(vars(ola)) + 
  ggpubr::stat_regline_equation(aes(label = ..rr.label..)) + 
  scale_x_log10() + 
  scale_y_log10()


left_join(letalidad_mun_ola.cfr, camas_por_100h) %>% 
  glm(formula = cfr ~ beds_per_100khab + ola, data = .) %>% 
  broom::tidy() %>% 
  arrange(p.value)

