# Este script une los datos de recursos y unidades de salud con los datos 
# de población del INEGI para agruparlos por municipio y por institución de salud. 
# Requirió un análisis manual de las variables asociadas a las instituciones de salud
# para poder unir los datasets

library(vroom)
library(dplyr)

# Leer los datos
poblacion <- vroom::vroom("data/poblacion_salud.tsv")
unidades <- vroom::vroom("data/unidades_salud.tsv") %>%
  filter(municipality != "No Informado")
recursos <-  vroom::vroom("data/recursos_salud.tsv")

# Filtrar recursos del año 2021 y elegir solamente los datos asociados al personal
# de salud, ya que el dataset de unidades contiene el número de camas y clínicas
recursos <- recursos %>%
  dplyr::filter(year == 2021 &
    grepl("personal|médico", resources_categories, ignore.case = TRUE)) %>%
  dplyr::group_by(clues) %>%
  summarize(personal = sum(total))

# Agrupar los recursos en unidades de salud por municipio e institución
cb_by_mun_inst <- unidades %>%
  dplyr::left_join(recursos, by = "clues") %>%
  dplyr::group_by(municipality_id, municipality, institution) %>%
  dplyr::summarise(camas = sum(beds, na.rm = T), clinicas = sum(clinics, na.rm = T), 
                   personal = sum(personal, na.rm = T))

# Filtrar los datos de población para tener solamente los de 
# afiliados a instituciones 
population_by_mun_inst <- poblacion %>%
  dplyr::filter(!variable %in% c("PSINDER", "PDER_SS", "POBTOT"))

# Nombres de las instuciones de salud en datamexico
sort(unique(cb_by_mun_inst$institution))
# [1] "CIJ"            "CRO"            "DIF"            "FGE"            "HUN"            "IMSS"           "IMSS-BIENESTAR"
# [8] "ISSSTE"         "PEMEX"          "PGR"            "SCT"            "SEDENA"         "SEMAR"          "SME"           
# [15] "SMM"            "SMP"            "SSA"

# Nombres de las variables en INEGI
sort(unique(population_by_mun_inst$variable))
# [1] "PAFIL_IPRIV" "PAFIL_OTRAI" "PAFIL_PDOM"  "PDER_IMSS"   "PDER_IMSSB"  "PDER_ISTE"   "PDER_ISTEE"  "PDER_SEGP" 
# "PAFIL_IPRIV" - Institucion privada.   "PAFIL_OTRAI"  - Otra
# "PAFIL_PDOM" -  Pemex, sedena, SEMAR.  "PDER_IMSS" - IMSS 
# "PDER_IMSSB" - IMSS-BIENESTAR.         "PDER_ISTE"  - ISSSTE
# "PDER_ISTEE" - ISSSTE Estatal.         "PDER_SEGP"  - SSA, mediante en el Instituto de Salud para el Bienestar.

# Nombres completos de las instuciones de salud en datamexico.
# El diccionario se obtuvo de: 
# http://revista.ibd.senado.gob.mx/index.php/PluralidadyConsenso/article/download/681/641
# ya que no se encontró en datamexico
ninst_salud <- c("Centros de Integración Juvenil", "Cruz Roja Mexicana", 
                 "Sistema Nacional para el Desarrollo Integral de la Familia", "Fiscalía General del Estado",
                 "Servicios Médicos Universitarios", "Instituto Mexicano del Seguro Social", 
                 "IMSS-Bienestar", "Instituto de Seguridad y Servicios Sociales de los Trabajadores del Estado",
                 "Pemex", "Procuraduría General de la República", "Secretaría de Comunicaciones y Transportes",
                 "Secretaría de la Defensa Nacional", "Secretaría de Marina", "Servicios Médicos Estatales", 
                 "Servicios Médicos Municipales", "Servicios Médicos Privados", "Secretaría de Salud")

# Asoción manual de nombres de instituciones en datamexico con 
# variables del INEGI
var_salud <- c("PAFIL_OTRAI", "PAFIL_OTRAI", "PAFIL_OTRAI", "PAFIL_OTRAI", "PAFIL_OTRAI", "PDER_IMSS", "PDER_IMSSB",
               "PDER_ISTE", "PAFIL_PDOM", "PAFIL_OTRAI", "PAFIL_OTRAI", "PAFIL_PDOM", "PAFIL_PDOM", "PAFIL_OTRAI", 
               "PAFIL_OTRAI", "PAFIL_IPRIV", "PDER_SEGP")

dicc <- tibble(institution = sort(unique(cb_by_mun_inst$institution)), 
               nombre_institucion = ninst_salud, variable = var_salud)

# Unir datos de unidades de salud con diccionario. 
# Seleccionar unicamente la columna asociada a las variables del inegi
cb_by_mun_inst <- cb_by_mun_inst %>%
  dplyr::inner_join(dicc, by = "institution") %>%
  dplyr::select(-nombre_institucion, -institution)

# Quitar las unidades clasificadas como PAFIL_OTRAI (Otro) y agregar para sumar 
# PEMEX, SEDENA y SEMAR, como se encuentran en los datos del INEGI 
cb_by_mun_inst <- cb_by_mun_inst %>%
  dplyr::filter(variable != "PAFIL_OTRAI") %>%
  dplyr::group_by(across(-c(camas, clinicas, personal))) %>%
  dplyr::summarise(camas = sum(camas), clinicas = sum(clinicas), personal = sum(personal)) %>%
  dplyr::ungroup()

# Cambiar ISSSTE estatal por PAFIL_OTRAI, ya que no hay datos del ISSSTE estatal 
# en la base de datos de salud
population_by_mun_inst[population_by_mun_inst["variable"] == "PDER_ISTEE", "nombre"] <- "Otro"
population_by_mun_inst[population_by_mun_inst["variable"] == "PDER_ISTEE", "variable"] <- "PAFIL_OTRAI"

# Filtrar el conjunto de datos del INEGI para eliminar los datos de afiliados a
# instituciones PAFIL_OTRAI (Otro)
population_by_mun_inst <- population_by_mun_inst %>%
  dplyr::group_by(across(-valor)) %>%
  dplyr::summarise(valor = sum(valor)) %>%
  dplyr::filter(variable != "PAFIL_OTRAI") %>%
  dplyr::rename("poblacion" = "valor") %>%
  dplyr::ungroup()

# Unir datos de población con datos de recursos de unidades de salud
recursos_pop_by_mun_inst <- population_by_mun_inst %>% 
  dplyr::left_join(cb_by_mun_inst, by = c("municipality", "municipality_id", "variable")) %>%
  dplyr:: left_join(dicc %>% select(variable, nombre_institucion), by = "variable") %>%
  dplyr::select(state_id, state, municipality_id, municipality, variable_inegi = variable, 
         institucion = nombre, nombre_institucion,poblacion, camas, clinicas, personal) %>%
  dplyr::mutate(across(where(is.numeric), .fns = ~replace(., is.na(.), 0)))

# Guardar datos
vroom::vroom_write(recursos_pop_by_mun_inst, "data/recursos_poblacion_by_institucion_municipio.tsv")
