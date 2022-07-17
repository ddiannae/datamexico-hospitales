library(vroom)
library(dplyr)
library(tidyr)

# Desgarcar los datos del censo de población del 2020 INEGI
# ya que incluye al IMSS Bienestar
temp <- tempfile()
download.file("https://www.inegi.org.mx/contenidos/programas/ccpv/2020/datosabiertos/iter/iter_00_cpv2020_csv.zip",
              temp)

# Seleccionar solamente las variables de afiliación a 
# instituciones de salud
poblacion <- vroom(unz(temp, "iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv"),
                  col_select = c(ENTIDAD, NOM_ENT, MUN, NOM_MUN, LOC, NOM_LOC, 
                                 POBTOT, PSINDER, PDER_SS, PDER_IMSS, PDER_ISTE,
                                 PDER_ISTEE, PAFIL_PDOM, PDER_SEGP, PDER_IMSSB,
                                 PAFIL_IPRIV, PAFIL_OTRAI), 
                  col_types = cols(.default = col_character()))
unlink(temp)

# Filtrar datos por municipio y generar id igual al
# identificador utilizado en datamexico
poblacion <- poblacion %>% 
  filter(NOM_LOC == "Total del Municipio") %>%
  select(-LOC, -NOM_LOC) %>%
  mutate(ENTIDAD = as.numeric(ENTIDAD), 
         municipality_id = paste0(ENTIDAD, MUN))

# Generar tibble en formato largo
poblacion <- poblacion %>%
  select(state_id = ENTIDAD, state = NOM_ENT, municipality_id, municipality = NOM_MUN,
         everything(), -MUN) %>%
  pivot_longer(cols = starts_with("P"), names_to = "variable", values_to = "valor") %>%
  mutate(valor = as.numeric(valor))

# Agregar diccionario para las variables
dicc <- tibble(variable = c("POBTOT", "PSINDER", "PDER_SS", "PDER_IMSS", "PDER_ISTE",
                            "PDER_ISTEE", "PAFIL_PDOM", "PDER_SEGP", "PDER_IMSSB",
                            "PAFIL_IPRIV", "PAFIL_OTRAI"),
               nombre = c("Población Total", "Sin afiliación", "Con afiliación", "IMSS", "ISSSTE", 
                          "ISSSTE estatal", "PEMEX, SEDENA, SEMAR", "SSA", "IMSS-BIENESTAR", 
                          "SMP", "Otro"))

poblacion <- poblacion %>% inner_join(dicc)

# Acortar nombres de estados
poblacion[poblacion["state_id"] == 5, "state"] = "Coahuila"
poblacion[poblacion["state_id"] == 16, "state"] = "Michoacán"
poblacion[poblacion["state_id"] == 30, "state"] = "Veracruz"

# Guardar datos
vroom::vroom_write(poblacion, "data/poblacion.tsv")
