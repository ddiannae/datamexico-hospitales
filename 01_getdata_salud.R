## Script para descargar datos de las unidades de salud y 
## sus recursos asociados desde datamexico

library(vroom)
library(dplyr)
library(janitor)
library(jsonlite)

# Crear folder para guardar datos
dir.create("data")

# Datos de unidades de salud
# Constuir la url para hacer consulta al api de datamexico
dm_hospitales <- "https://api.datamexico.org/tesseract/data.jsonrecords?cube=health_establishments"
dds <- c("State", "Municipality", "Areas", "CLUES", "Establishment+Type",  "Institution", "Latitud", "Longitud")
mms <- c("Clinics", "Beds")
meas <- "measures="
drill <- "drilldowns="
hosp_url <- paste0(dm_hospitales, "&",
                   drill, paste(dds, collapse = "%2C"), "&",
                   meas,  paste(mms, collapse = "%2C"), "&locale=es")

# Ejecutar consulta
resp <- jsonlite::fromJSON(txt = hosp_url)

# Limpiar nombres de variables
resp$data <- resp$data %>% 
  janitor::clean_names() %>% 
  as_tibble()

# Guardar datos
vroom::vroom_write(resp$data, "data/unidades_salud.tsv")

# Datos de recursos en unidades de salud
# Constuir la url para hacer consulta al api de datamexico
dm_resources <- "https://api.datamexico.org/tesseract/data.jsonrecords?cube=health_resources"
mms_resources <- c("Total")
dds_resources <- c("State", "Municipality",  "CLUES", "Type", "Resources+Categories", "Year")
meas <- "measures="
drill <- "drilldowns="
yr <- "Year="

# La consulta tiene que hacerse en dos pasos ya que si se solicitan
# datos de más de 4 años, el servidor regresa un error
years <- as.character(2017:2021)
resources_url <- paste0(dm_resources, "&",
                        yr, paste(years, collapse = "%2C"), "&",
                        drill, paste(dds_resources, collapse = "%2C"), "&",
                        meas,  paste(mms_resources, collapse = "%2C"))

# Ejecutar consulta
resp <- jsonlite::fromJSON(txt = resources_url)

# Limpiar nombres de variables y filtrar filas que no contienen datos de recursos
tb1 <- resp$data %>% 
  janitor::clean_names() %>% 
  as_tibble() %>%
  filter(total > 0)

# Segunda consulta
years <- as.character(2012:2016)
resources_url <- paste0(dm_resources, "&",
                        yr, paste(years, collapse = "%2C"), "&",
                        drill, paste(dds_resources, collapse = "%2C"), "&",
                        meas,  paste(mms_resources, collapse = "%2C"))

# Ejecutar consulta
resp <- jsonlite::fromJSON(txt = resources_url)

# Limpiar nombres de variables y filtrar filas que no contienen datos de recursos
tb2 <- resp$data %>% 
  janitor::clean_names() %>% 
  as_tibble() %>%
  filter(total > 0)

# Unir tibbles
data <- dplyr::bind_rows(tb1, tb2)

# Guardar datos
vroom::vroom_write(data, "data/recursos_salud.tsv")
