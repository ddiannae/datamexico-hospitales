library(vroom)
library(dplyr)
library(ggplot2)
library(binsreg)
library(ggthemes)
library(scales)

population <- vroom::vroom("data/poblacion_salud.tsv")
unidades <- vroom::vroom("data/unidades_salud.tsv")

population_by_mun <- population %>%
  group_by(municipality_id, municipality) %>%
  summarise(population = sum(population)) 

camas_by_mun <- unidades %>%
  group_by(municipality_id, municipality) %>%
  summarise(beds = sum(beds)) 

camas_population <- population_by_mun %>% 
  left_join(camas_by_mun, by = c("municipality_id", "municipality"))


p <- binsreg(y = camas_population$beds, x = camas_population$population,  ci=c(3,3))$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Camas por municipio", title = "Población versus camas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) 

png(paste0("plots_corr/hab_camas.png"), width = 1200, height = 800)
print(p)
dev.off()

quantile(camas_population$population)

p <- binsreg(y = camas_population$beds, x = camas_population$population,  ci=c(3,3), plotxrange = c(0, 75000))$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Camas por municipio", title = "Población versus camas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) 

png(paste0("plots_corr/hab_camas_75mil.png"), width = 1200, height = 800)
print(p)
dev.off()

population_by_mun_inst <- population %>%
  group_by(municipality_id, municipality, health_care_id, health_care) %>%
  summarise(population = sum(population)) 

camas_by_mun_inst <- unidades %>%
  group_by(municipality_id, municipality, institution_id, institution) %>%
  summarise(beds = sum(beds)) 

# unique(population_by_mun_inst$health_care)
# c("IMSS (Seguro social)", "ISSSTE", "ISSSTE estatal", "Pemex, Defensa o Marina",
#   "Pemex, Defensa o Marina",  "Pemex, Defensa o Marina"
#   
# [5] "Seguro Popular"          "Privado"                 "Consultorio de farmacia" "Otro lugar"             
# [9] "No se atiende"           "No especificado"        
# 
# c("IMSS"  "ISSSTE"  "ISSSTE", "PEMEX",  "SEDENA", "SEMAR"   
# unique(camas_by_mun_inst$institution)
# "DIF"            "HUN"                            "SCT"                 "SMP"           
# [8] "SSA"            "CRO"            "IMSS-BIENESTAR"          "SME"            "CIJ"            "SMM"           
# [15] "PGR"            "FGE"                     
# 
# unique(population_by_mun_inst$health_care)
# camas_by_mun_inst %>% 
#   ungroup() %>%
#   count(institution, sort = T)
