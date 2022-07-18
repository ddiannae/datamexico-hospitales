library(tidyverse)
library(vroom)
library(tidymodels)
library(sf)
library(spdep)
library(igraph)
library(tidygraph)
library(ggraph)

mun_df <- vroom("etl/red_adyacencia_muns.txt")


#calcular distancias coseno entre las variables predictoras ---- 
distancias_coseno_predictores <- 
  x %>% 
  select(CVEGEO, ola, cfr, pruebas_dia_capita, mun_derechohabientes_PDER_IMSS, POB65_MAS, POBFEM) %>% 
  pivot_wider(id_cols = CVEGEO,
              names_from = ola, 
              values_from = -c(CVEGEO, ola)
  ) %>% 
  rename(imss = mun_derechohabientes_PDER_IMSS_01_ola_1,
         elder = POB65_MAS_01_ola_1,
         fem   = POBFEM_01_ola_1
         ) %>% 
  select(-contains("mun_derechohabientes"),
         -contains("POB")
         ) %>% 
  select(-CVEGEO) %>% 
  proxy::dist(x = ., method = "cosine") %>% 
  as.matrix()

munis_ordenados <- 
  x %>% 
  select(CVEGEO, ola, cfr, pruebas_dia_capita, mun_derechohabientes_PDER_IMSS, POB65_MAS, POBFEM) %>% 
  pivot_wider(id_cols = CVEGEO,
              names_from = ola, 
              values_from = -c(CVEGEO, ola)
  ) %>% pull(CVEGEO)

rownames(distancias_coseno_predictores) <- munis_ordenados
colnames(distancias_coseno_predictores) <- munis_ordenados

distancias_coseno_predictores.df <- 
  distancias_coseno_predictores %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "V1") %>% 
  pivot_longer(cols = -V1, names_to = "V2", values_to = "cosine_distance")

# juntarla con el dataframe de red ---

mun_df <- left_join(mun_df, distancias_coseno_predictores.df)

#hacerla red ---- 

g <- mun_df %>% rename(weight = cosine_distance) %>%   graph_from_data_frame(directed = F) %>% simplify() %>% as_tbl_graph() 

set.seed(725)

g <- 
  g %>% 
  mutate(comm_infomap = group_infomap(weights = weight, trials = 100))

g %>% 
  as_tibble() %>% 
  group_by(comm_infomap) %>% 
  tally(sort = T)

set.seed(283)
mi_paleta <- tibble(comm_infomap = 1:283, 
                    color = sample(x = viridis::mako(283) , size = 283)
                    )

mis_comms <- 
  g %>% 
  as_tibble() %>% 
  select(CVEGEO = name, comm_infomap) %>% 
  left_join(mi_paleta)

mun_shp <- read_sf("data/00mun.shp")
camas_capita <- vroom("data/recursos_poblacion_by_institucion_municipio.tsv")
poblacion.light <- poblacion %>% mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% select(CVEGEO, POBTOT)

camas_capita.light <- 
  camas_capita %>% 
  select(CVEGEO = municipality_id, institucion, poblacion, camas, clinicas, personal) %>% 
  mutate(CVEGEO = str_pad(CVEGEO, 5, "left", 0))

left_join(mun_shp, mis_comms) %>% 
  ggplot() + 
  geom_sf(aes(fill = color), lwd=0.1) + 
  scale_fill_identity() + 
  theme_minimal()


################################################################################

# ahora responder unas preguntas de quien incide donde ----

# que institucion tiene mas afiliados por comunidad ? ---- 

comm.afiliados <- 
  mis_comms %>% 
  left_join(camas_capita.light) %>% 
  group_by(comm_infomap, institucion) %>% 
  summarise(poblacion = sum(poblacion)) %>% 
  filter(poblacion==max(poblacion))

left_join(mis_comms, (comm.afiliados %>% select(-poblacion))) %>% 
left_join(mun_shp, .) %>% 
  ggplot() + 
  geom_sf(aes(fill = institucion), lwd=0.1) + 
  #scale_fill_identity() + 
  theme_minimal()

# que institucion pone mas camas por comunidad ? ---- 

comm.camas <- 
  mis_comms %>% 
  left_join(camas_capita.light) %>% 
  group_by(comm_infomap, institucion) %>% 
  summarise(camas = sum(camas)) %>% 
  filter(camas==max(camas))

por_contribucion <- 
  left_join(comm.camas, comm.afiliados, by="comm_infomap") %>% 
  mutate(contribuye_el_mas_grande = institucion.x==institucion.y) %>% 
  mutate(grupo = case_when(contribuye_el_mas_grande ~ glue::glue("{institucion.y} mayor contribuyente camas"),
                           TRUE ~ glue::glue("{institucion.y} debe contribuir más camas")
                           )
         ) %>% 
  select(comm_infomap, grupo)

  
left_join(mis_comms, por_contribucion) %>% 
  left_join(mun_shp, .) %>% 
  ggplot() + 
  geom_sf(aes(fill = grupo), lwd=0.1) + 
  #scale_fill_identity() + 
  theme_minimal()


#camas per capita deficiencia aumento ----

#

deficiencia_sobrecontribucion_camas <- 
  mis_comms %>% 
  left_join(poblacion.light) %>% 
  left_join(camas_capita.light) %>% 
  select(CVEGEO, comm_infomap, POBTOT, camas) %>% 
  group_by(CVEGEO, comm_infomap, POBTOT) %>% 
  summarize(camas = sum(camas)) %>% 
  group_by(comm_infomap) %>% 
  mutate(camas.comm.sum = sum(camas),
         camas.comm.avg = mean(camas),
         camas.comm.sd = sd(camas),
         ) %>% 
  mutate(z_score = (camas - camas.comm.avg)/camas.comm.sd)
  

left_join(mun_shp, deficiencia_sobrecontribucion_camas) %>% 
  ggplot()  + 
  geom_sf(aes(fill=z_score), lwd=0.05) + 
  #scale_fill_distiller("Z-score", palette = "RdBu", direction = -1, ) +
  scale_fill_gradient2("Z-score", low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "blue") + 
  ggtitle("Contribución de camas del municipio a su Comunidad de Servicios de Salud")  + 
  labs(caption = "Los municipios azules aportan menos camas a la CSS;\n
       los municipios rojos sobrecompensan la necesidad de camas")
# mis_comms %>% 
#   left_join(poblacion.light) %>% 
#   left_join(camas_capita.light) %>% 
#   group_by(comm_infomap, color, institucion) %>% 
#   rename(derechohabientes = poblacion) %>% 
#   mutate(derechohabientes.comm = sum(derechohabientes), 
#          camas.comm = sum(camas), 
#          clinicas.comm = sum(clinicas), 
#          personal.comm = sum()
#          )
# 
# 
# 
# 
