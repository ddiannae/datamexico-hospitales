library(tidyverse)
library(vroom)
library(tidymodels)
library(sf)
library(spdep)
library(igraph)
library(tidygraph)
library(ggraph)

# shapefiles de municipios, para mapitas ---- 

shp_mun <- read_sf("data/conjunto_de_datos/00mun.shp")

adj_nw <- spdep::poly2nb(shp_mun)

mun_g <- 
  lapply(seq_along(adj_nw), 
       FUN = function(i){tibble(V1 = shp_mun$CVEGEO[i],
                                V2 = shp_mun$CVEGEO[adj_nw[[i]]]
  )}) %>% 
  bind_rows() %>% 
  graph_from_data_frame(directed = F) %>% 
  simplify() %>% 
  as_tbl_graph()




mun_df <- 
  lapply(seq_along(adj_nw), 
         FUN = function(i){tibble(V1 = shp_mun$CVEGEO[i],
                                  V2 = shp_mun$CVEGEO[adj_nw[[i]]]
         )}) %>% 
  bind_rows()

#join neighbors bed, pop so that we can have 


recursos_folded <- 
  camas_capita %>% 
  select(CVEGEO, institucion_id, derechohabientes = poblacion, beds, clinics)

recursos_unfolded <- 
  camas_capita %>% 
  select(CVEGEO, institucion_id, derechohabientes = poblacion, beds, clinics) %>% 
  pivot_wider(id_cols = CVEGEO, 
                names_from = institucion_id, 
                values_from = c(beds, clinics, derechohabientes) 
    ) %>% 
  left_join((poblacion %>% 
               mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
               select(CVEGEO, POBTOT)))


recursos.neighborhood <- 
  left_join(mun_df, recursos_unfolded, by=c("V2"="CVEGEO")) %>% 
  rename(CVEGEO=V1) %>% 
  group_by(CVEGEO) %>% 
  summarize(across(where(is.numeric), sum))

vroom_write(recursos.neighborhood, "etl/recursos_vecindario.txt")
vroom_write(mun_df, file = "etl/red_adyacencia_muns.txt")



#quiero 
# camas normalizadas por poblacion derechohabiente 
# camas normalizadas por poblacion del municipio i 

recursos_vecinos.long <- 
  left_join(mun_df, recursos_folded, by=c("V2"="CVEGEO")) %>% 
  group_by(V1, institucion_id) %>% 
  #summarize(across(where(is.numeric), sum, .names = "{.col}.neighbors"))
  summarize(across(where(is.numeric), sum)) %>% 
  pivot_longer(cols = -c("V1", "institucion_id"), 
               names_to = "variable", 
               values_to = "vecino") %>% 
  rename(CVEGEO = V1)

recursos_locales.long <- 
  recursos_folded %>% 
  pivot_longer(cols = -c("CVEGEO", "institucion_id"), 
               names_to = "variable", 
               values_to = "mun")



  left_join(recursos_locales.long, recursos_vecinos.long) %>% 
  mutate(full = mun + vecino) %>% 
  mutate(mun.contrib = 100*mun/full) %>% 
  pivot_wider(names_from = c(variable, institucion_id), 
              values_from = c(mun, vecino, full, mun.contrib)
              ) %>% 
    vroom_write(., file = "etl/recursos_propios_vecinos.txt")
