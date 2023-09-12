library(vroom)
library(dplyr)
library(ggplot2)
library(binsreg)
library(ggthemes)
library(scales)

poblacion <- vroom::vroom("data/poblacion.tsv")
hospitales <- vroom::vroom("data/unidades_salud.tsv") %>%
  filter(municipality != "No Informado")

pop_by_mun <- poblacion %>%
  filter(variable == "POBTOT") %>%
  select(municipality_id, poblacion = valor, -variable, -nombre)

cb_by_mun <- hospitales %>%
  group_by(municipality_id, municipality) %>%
  summarise(clinics = sum(clinics), beds = sum(beds)) 
 
cb_by_mun <- pop_by_mun %>% 
  left_join(cb_by_mun, by = c("municipality_id")) %>%
  mutate(across(where(is.numeric), .fns = ~replace(., is.na(.), 0)))

p <- binsreg(y = cb_by_mun$beds, x = cb_by_mun$poblacion,  ci=c(3,3))$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Camas por municipio", title = "Población vs camas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) 

png(paste0("plots_corr/hab_camas.png"), width = 1200, height = 800)
print(p)
dev.off()

quantile(cb_by_mun$poblacion)
# 0%     25%     50%     75%    100% 
# 81    4489   13552   35284 1922523

p <- binsreg(y = cb_by_mun$beds, x = cb_by_mun$poblacion,  ci=c(3,3), plotxrange = c(0, 75000))$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Camas por municipio", title = "Población vs camas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) 

png(paste0("plots_corr/hab_camas_75mil.png"), width = 1200, height = 800)
print(p)
dev.off()

p <- binsreg(y = cb_by_mun$clinics, x = cb_by_mun$poblacion,  ci=c(3,3))$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Clínicas por municipio", title = "Población vs clínicas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) 

png(paste0("plots_corr/hab_clinicas.png"), width = 1200, height = 800)
print(p)
dev.off()

p <- binsreg(y = cb_by_mun$clinics, x = cb_by_mun$poblacion,  ci=c(3,3),  plotxrange = c(0, 75000))$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Clínicas por municipio", title = "Población vs clínicas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) 

png(paste0("plots_corr/hab_clinicas_75mil.png"), width = 1200, height = 800)
print(p)
dev.off()


vroom_write(beds_clinics_pop_by_mun_inst, "data/poblacion_camas_clinicas_institucion.tsv")

con_camas <- beds_clinics_pop_by_mun_inst %>%
  filter(beds > 0)

p <- binsreg(y = con_camas$beds, x = con_camas$poblacion,  ci=c(3,3), 
             by = con_camas$institucion, legendTitle = "Institución")$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Camas por municipio", title = "Población afiliada vs camas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) +
  theme(legend.position = "bottom")

png(paste0("plots_corr/afiliados_camas.png"), width = 1200, height = 600)
print(p)
dev.off()

p <- binsreg(y = con_camas$beds, x = con_camas$poblacion,  ci=c(3,3), plotxrange = c(0, 75000),
             by = con_camas$institucion, legendTitle = "Institución")$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Camas por municipio", title = "Población afiliada vs camas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) +
  theme(legend.position = "bottom")

png(paste0("plots_corr/afiliados_camas_75mil.png"), width = 1200, height = 600)
print(p)
dev.off()

con_clinicas <- beds_clinics_pop_by_mun_inst %>%
  filter(clinics > 0)

p <- binsreg(y = con_clinicas$clinics, x = con_clinicas$poblacion,  ci=c(3,3), 
             by = con_clinicas$institucion, legendTitle = "Institución")$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Clínicas por municipio", title = "Población afiliada vs clínicas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) +
  theme(legend.position = "bottom")

png(paste0("plots_corr/afiliados_clinicas.png"), width = 1200, height = 600)
print(p)
dev.off()

p <- binsreg(y = con_clinicas$clinics, x = con_clinicas$poblacion,  ci=c(3,3), plotxrange = c(0, 75000),
             by = con_clinicas$institucion, legendTitle = "Institución")$bins_plot +
  theme_clean(base_size = 25) +
  labs(x = "Habitantes por municipio", y = "Clínicas por municipio", title = "Población afiliada vs clínicas en unidades de salud") +
  scale_y_continuous(label=comma) +
  scale_x_continuous(label=comma) +
  theme(legend.position = "bottom")

png(paste0("plots_corr/afiliados_clinicas_75mil.png"), width = 1200, height = 600)
print(p)
dev.off()
