library(tidyverse)
library(vroom)
library(tidymodels)
library(sf)
library(spdep)
library(igraph)
library(tidygraph)
library(ggraph)

# Leer shapefile
shp_mun <- read_sf("data/00mun.shp")

# Obtener polígonos vecinos
adj_nw <- spdep::poly2nb(shp_mun)

# Construir una red a partir de los pares de municipios vecinos
mun_g <- lapply(seq_along(adj_nw), 
       FUN = function(i){tibble(V1 = shp_mun$CVEGEO[i],
                                V2 = shp_mun$CVEGEO[adj_nw[[i]]]
  )}) %>% 
  bind_rows() %>% 
  graph_from_data_frame(directed = F) %>% 
  simplify() %>% 
  as_tbl_graph()

# Obtener las claves geográficas de los vecinos en la red
mun_df <- lapply(seq_along(adj_nw), 
         FUN = function(i){tibble(V1 = shp_mun$CVEGEO[i],
                                  V2 = shp_mun$CVEGEO[adj_nw[[i]]]
         )}) %>% 
  bind_rows()

# Leer datos de recursos de salud y obtener los recursos per cápita
recursos_capita <- vroom("data/recursos_poblacion_by_institucion_municipio.tsv")
poblacion <- vroom("data/poblacion_demo.tsv")

recursos_capita <- 
  recursos_capita %>% 
  mutate(state_id = str_pad(string = state_id, width = 2, side = "left", pad = 0),
         municipality_id = str_pad(string = municipality_id, width = 5, side = "left", pad = 0),
         CVEGEO = municipality_id) %>% 
  mutate(camas_capita = camas/poblacion, clinicas_capita = clinicas/poblacion) 

# Seleccionar variables 
recursos_folded <- 
  recursos_capita %>% 
  select(CVEGEO, institucion_inegi, derechohabientes = poblacion, camas, clinicas, personal)

# Seleccionar variables en formato largo
recursos_unfolded <- 
  recursos_capita %>% 
  select(CVEGEO, institucion_inegi, derechohabientes = poblacion, camas, clinicas, personal) %>% 
  pivot_wider(id_cols = CVEGEO, 
                names_from = institucion_inegi, 
                values_from = c(camas, clinicas, personal, derechohabientes) 
    ) %>% 
  left_join((poblacion %>% 
               mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
               select(CVEGEO, POBTOT)))


# Identificar recursos por parejas de municipios adyacentes
recursos_neighborhood <- 
  left_join(mun_df, recursos_unfolded, by=c("V2"="CVEGEO")) %>% 
  rename(CVEGEO=V1) %>% 
  group_by(CVEGEO) %>% 
  summarize(across(where(is.numeric), sum))

vroom_write(recursos_neighborhood, "etl/recursos_vecindario.txt")
vroom_write(mun_df, file = "etl/red_adyacencia_muns.txt")


# Identificar recursos normalizados por población 
# derechoabiente y recursos normalizados por población del municipio
recursos_vecinos_long <- 
  left_join(mun_df, recursos_folded, by=c("V2"="CVEGEO")) %>% 
  group_by(V1, institucion_inegi) %>% 
  summarize(across(where(is.numeric), sum)) %>% 
  pivot_longer(cols = -c("V1", "institucion_inegi"), 
               names_to = "variable", 
               values_to = "vecino") %>% 
  rename(CVEGEO = V1)

recursos_locales_long <- 
  recursos_folded %>% 
  pivot_longer(cols = -c("CVEGEO", "institucion_inegi"), 
               names_to = "variable", 
               values_to = "mun")

# Guardar los datos
left_join(recursos_locales.long, recursos_vecinos.long) %>% 
  mutate(full = mun + vecino) %>% 
  mutate(mun.contrib = 100*mun/full) %>% 
  pivot_wider(names_from = c(variable, institucion_id), 
              values_from = c(mun, vecino, full, mun.contrib)
              ) %>% 
    vroom_write(., file = "etl/recursos_propios_vecinos.txt")
