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
               nombre = c("Población Total", "Sin afiliación", "Con afiliación", "IMSS", "ISSSTE", 
                          "ISSSTE estatal", "PEMEX, SEDENA, SEMAR", "SSA", "IMSS-BIENESTAR", 
                          "SMP", "Otro"))

poblacion <- poblacion %>% inner_join(dicc)
vroom::vroom_write(poblacion, "data/poblacion.tsv")

poblacion[poblacion["state_id"] == 5, "state"] = "Coahuila"
poblacion[poblacion["state_id"] == 16, "state"] = "Michoacán"
poblacion[poblacion["state_id"] == 30, "state"] = "Veracruz"

pob_by_state <- poblacion %>% 
  filter(variable %in% c("POBTOT", "PSINDER", "PDER_SS")) %>%
  pivot_wider(id_cols = c(state, state_id, municipality_id, municipality), 
              names_from = "variable", values_from = "valor") %>%
  group_by(state, state_id) %>%
  summarise(POBTOT = sum(POBTOT), PSINDER = sum(PSINDER), PDER_SS = sum(PDER_SS)) %>%
  mutate(frac_sin = PSINDER/POBTOT, frac_con = PDER_SS/POBTOT) %>%
  select(starts_with(c("state", "frac"))) %>%
  pivot_longer(cols = starts_with("frac"), names_to = "tipo", values_to = "valor")
  
p <- pob_by_state %>%
  ggplot(aes(x = tipo, y = valor, fill = tipo)) +
  geom_bar(stat = "identity") +
  theme_clean(base_size = 25) +
  facet_wrap(~paste0(state))+ 
  labs(x = "", y = "Porcentaje de población", title = "Porcentaje de población con y sin afiliación a servicios de salud") +
  scale_fill_stata(name = "Afiliación", labels = c("Con afiliación", "Sin afiliación")) +
  theme(axis.text.x = element_blank(), legend.position = "bottom", 
        axis.ticks.x = element_blank())

png(paste0("plots/poblacion_afiliacion.png"), width = 1200, height = 1200)
print(p)
dev.off()

pob_by_state_inst <- poblacion %>% 
  filter(!variable %in% c("PSINDER", "PDER_SS")) %>%
  pivot_wider(id_cols = c(state, state_id, municipality_id, municipality), 
              names_from = "variable", values_from = "valor") %>%
  group_by(state, state_id)  %>%
  summarise(across(.cols = starts_with("P"), .fns = sum)) %>%
  mutate(across(.cols = starts_with("P"), .fns = ~(.x/POBTOT)*100)) %>%
  select(-POBTOT) %>%
  pivot_longer(cols = starts_with("P"), names_to = "variable", values_to = "fraccion") %>%
  inner_join(dicc)

p <- pob_by_state_inst %>%
  ggplot(aes(x = nombre, fill = nombre, y = fraccion)) +
  geom_bar(stat = "identity") +
  theme_clean(base_size = 25) +
  facet_wrap(~paste0(state))+ 
  labs(x = "", y = "Porcentaje de población", title = "Porcentaje de población por entidad afiliada a instituciones de salud") + 
  scale_fill_stata(name = "Institución") +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) 


png(paste0("plots/porcentaje_pobl_institucion.png"), width = 1800, height = 1400)
print(p)
dev.off()
