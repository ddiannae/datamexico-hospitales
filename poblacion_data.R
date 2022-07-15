library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)

poblacion <- vroom("raw_data/conjunto_de_datos_iter_00CSV20.csv",
                         col_select = c(ENTIDAD, NOM_ENT, MUN, NOM_MUN, LOC, NOM_LOC, 
                                        POBTOT, PSINDER, PDER_SS, PDER_IMSS, PDER_ISTE,
                                        PDER_ISTEE, PAFIL_PDOM, PDER_SEGP, PDER_IMSSB,
                                        PAFIL_IPRIV, PAFIL_OTRAI), 
                   col_types = cols(.default = col_character()))

poblacion <- poblacion %>% 
  filter(NOM_LOC == "Total del Municipio") %>%
  select(-LOC, -NOM_LOC) %>%
  mutate(ENTIDAD = as.numeric(ENTIDAD), 
         municipality_id = paste0(ENTIDAD, MUN))

poblacion <- poblacion %>%
  select(state_id = ENTIDAD, state = NOM_ENT, municipality_id, municipality = NOM_MUN,
         everything(), -MUN) %>%
  pivot_longer(cols = starts_with("P"), names_to = "variable", values_to = "valor") %>%
  mutate(valor = as.numeric(valor))

dicc <- tibble(variable = c("POBTOT", "PSINDER", "PDER_SS", "PDER_IMSS", "PDER_ISTE",
          "PDER_ISTEE", "PAFIL_PDOM", "PDER_SEGP", "PDER_IMSSB",
          "PAFIL_IPRIV", "PAFIL_OTRAI"),
          nombre = c("poblacion", "sin_afiliacion", "con_afiliacion", "IMSS", "ISSSTE", 
                     "ISSSTE estatal", "PEMEX, SEDENA, SEMAR", "SSA", "IMSS-BIENESTAR", 
                     "SMP", "OTRO"))

poblacion <- poblacion %>% inner_join(dicc)
vroom::vroom_write(poblacion, "data/poblacion.tsv")  
