library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)

unidades <- vroom::vroom("data/unidades_salud.tsv")
poblacion <- vroom::vroom("data/poblacion_demo.tsv")

poblacion <- poblacion %>% 
  group_by(NOM_ENT) %>% 
  summarise(pobtotal = sum(POBTOT))

unidades <- unidades %>% 
  group_by(state, institution_id) %>%
  summarise(camas = sum(beds), 
            clinicas = sum(clinics))

unidades <- unidades %>% 
  left_join(poblacion, by = c("state" = "NOM_ENT"))

inst_mas100 <- resp$data %>% 
  count(institution, sort = T) %>%
  filter(n > 100) %>%
  pull(institution)

### Unidades de salud por institución, solo las que tienen más de 100
p <- ggplot(resp$data %>%
              filter(institution %in% inst_mas100)
            , aes(x = state, fill = institution)) +
  geom_bar() +
  theme_clean(base_size = 25) + 
  labs(x = "Estado", y = "Total de unidades", title = "Unidades de salud por institución") +
  scale_fill_stata(name = "Institución") +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

png("plots/unidad_institucion.png", width = 1200, height = 800)
print(p)
dev.off()

con_camas <- resp$data %>%
  filter(beds > 0)

### Unidades de salud con camas
p <- ggplot(con_camas , aes(x = state, fill = institution)) +
  geom_bar() +
  theme_clean(base_size = 25) + 
  labs(x = "Estado", y = "Total de unidades", title = "Unidades de salud con camas por institución") +
  scale_fill_stata(name = "Institución") +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

png("plots/unidadcamas_institucion.png", width = 1200, height = 800)
print(p)
dev.off()

con_camas_by_inst <- con_camas %>% 
  group_by(state, institution) %>%
  summarise(beds = sum(beds))

con_camas_by_est <- con_camas %>%
  group_by(state) %>%
  summarise(total_beds = sum(beds)) %>%
  mutate(category = case_when(
    total_beds > 10000 ~ "mas_diez",
    total_beds > 5000 ~ "mas_cinco",
    total_beds > 3000 ~ "mas_tres",
    TRUE ~ "menos_tres"
  ))

### Camas totales por estado
p <- ggplot(con_camas_by_est , aes(x = state, y = total_beds, label = total_beds)) +
  geom_bar(stat = "identity") +
  geom_text( nudge_y = 1000) +
  theme_clean(base_size = 25) + 
  labs(x = "Estado", y = "Total de camas", title = "Camas por estado") +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

png("plots/camas_estado.png", width = 1200, height = 800)
print(p)
dev.off()

st_categories <- c("mas_diez", "mas_cinco","mas_tres", "menos_tres" )
names(st_categories) <- c("Más de diez mil camas", "Entre 5 mil y 10 mil camas",
                          "Entre 3 mil y 5 mil camas", "Menos de 3 mil camas")

con_camas_by_inst <- con_camas_by_inst %>% 
  inner_join(con_camas_by_est, by = "state")
institutions <- unique(con_camas_by_inst$institution)
colors <- stata_pal("s2color")(length(institutions))
names(colors) <- institutions

for (i in 1:length(st_categories)) {
  p <- con_camas_by_inst %>%
    filter(category == st_categories[i]) %>%
    ggplot(aes(x = institution, fill = institution, y = beds)) +
    geom_bar(stat = "identity") +
    theme_clean(base_size = 25) + 
    labs(x = "", y = "Total de camas", title = "Camas en unidades de salud por institución", 
         subtitle = names(st_categories[i])) +
    scale_fill_manual(values = colors, name = "Tipo") +
    theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    facet_wrap(~paste0(state, "\n", total_beds))
  
  png(paste0("plots/camas_institucion_", st_categories[i], ".png"), width = 1400, height = 1000)
  print(p)
  dev.off()
}

camas_by_state <- con %>% 
  group_by(state, institution) %>%
  summarise(beds = sum(beds))
