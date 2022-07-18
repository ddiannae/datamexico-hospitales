## Script para descargar shapefile del marco estadístico 2020
## directamente de la página del INEGI

# Aumentar el timeout para descarga
options(timeout = max(300, getOption("timeout")))

# Desgarcar shape file
temp <- tempfile()
download.file("https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/mg_2020_integrado.zip",
              temp)

# Guardar archivo
unzip(temp, files = c("conjunto_de_datos/00mun.shp", "conjunto_de_datos/00mun.shx"), exdir = "data", 
      junkpaths = TRUE)
