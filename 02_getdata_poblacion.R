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
poblacion_salud <- vroom::vroom(unz(temp, "iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv"),
                  col_select = c(ENTIDAD, NOM_ENT, MUN, NOM_MUN, LOC, NOM_LOC, 
                                 POBTOT, PSINDER, PDER_SS, PDER_IMSS, PDER_ISTE,
                                 PDER_ISTEE, PAFIL_PDOM, PDER_SEGP, PDER_IMSSB,
                                 PAFIL_IPRIV, PAFIL_OTRAI), 
                  col_types = cols(.default = col_character()))

# Seleccionar solamente las variables demográficas
poblacion_demo <- vroom::vroom(unz(temp, "iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv"),
                               col_select = c(ENTIDAD, NOM_ENT, MUN, NOM_MUN, LOC, NOM_LOC, 
                                              POBTOT, POBFEM, POB0_14, POB15_64, POB65_MAS))

unlink(temp)

# Filtrar datos por municipio y generar id igual al
# identificador utilizado en datamexico
poblacion_salud <- poblacion_salud %>% 
  filter(NOM_LOC == "Total del Municipio") %>%
  select(-LOC, -NOM_LOC) %>%
  mutate(ENTIDAD = as.numeric(ENTIDAD), 
         municipality_id = paste0(ENTIDAD, MUN))

# Generar tibble en formato largo
poblacion_salud <- poblacion_salud %>%
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

poblacion_salud <- poblacion_salud %>% inner_join(dicc)

# Acortar nombres de estados
poblacion_salud[poblacion_salud["state_id"] == 5, "state"] = "Coahuila"
poblacion_salud[poblacion_salud["state_id"] == 16, "state"] = "Michoacán"
poblacion_salud[poblacion_salud["state_id"] == 30, "state"] = "Veracruz"

# Guardar datos
vroom::vroom_write(poblacion_salud, "data/poblacion_salud.tsv")

# Seleccionar unicamente los valores por municipio
poblacion_demo <- poblacion_demo %>% 
  filter(NOM_LOC == "Total del Municipio") %>%
  select(-LOC, -NOM_LOC) 

# Acortar nombres de estados
poblacion_demo[poblacion_demo["state_id"] == 5, "state"] = "Coahuila"
poblacion_demo[poblacion_demo["state_id"] == 16, "state"] = "Michoacán"
poblacion_demo[poblacion_demo["state_id"] == 30, "state"] = "Veracruz"

# Guardar datos
vroom::vroom_write(poblacion_demo, "data/poblacion_demo.tsv")

