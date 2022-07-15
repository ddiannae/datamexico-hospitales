library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(jsonlite)

dm_population <- "https://api.datamexico.org/tesseract/data.jsonrecords?cube=inegi_population"
mms_population <- c("Population")
dds_population <- c("State", "Municipality",  "Age+Range", "Sex", "Health+Care")
meas <- "measures="
drill <- "drilldowns="

population_url <- paste0(dm_population, "&",
                        drill, paste(dds_population, collapse = "%2C"), "&",
                        meas,  paste(mms_population, collapse = "%2C"), "&locale=es")
resp <- jsonlite::fromJSON(txt = population_url)
resp$data <- resp$data %>% 
  janitor::clean_names() %>% 
  as_tibble()

rangos_edades <- c("0 a 4 años", "5 a 9 años", "10 a 14 años", "15 a 19 años", 
                   "20 a 24 años", "25 a 29 años", "30 a 34 años", "35 a 39 años",
                   "40 a 44 años", "45 a 49 años", "50 a 54 años", "55 a 59 años",  
                   "60 a 64 años", "65 a 69 años", "70 a 74 años", "75 a 79 años", 
                   "80 a 84 años", "85 años o más")


rangos_20 <- c("0 a 19 años", "0 a 19 años", "0 a 19 años", "0 a 19 años",
               "20 a 39 años", "20 a 39 años", "20 a 39 años", "20 a 39 años", 
               "40 a 59 años", "40 a 59 años", "40 a 59 años", "40 a 59 años",
               "60 a 79 años", "60 a 79 años", "60 a 79 años", "60 a 79 años", 
               "más de 80 años", "más de 80 años")

rangos <- tibble(rangos_edades = rangos_edades, rangos_20 = rangos_20)

resp$data[resp$data["health_care"] == "Centro de Salud u Hospital de la SSA (Seguro Popular)", "health_care"] = "Seguro Popular"
resp$data[resp$data["health_care"] == "Consultorio, clínica u hospital privado", "health_care"] = "Privado"

vroom::vroom_write(resp$data, "data/poblacion_salud.tsv")  

 age_data_by_state <- resp$data %>% 
  group_by(state, health_care, age_range) %>%
  summarise(population = sum(population)) 

data_by_state <- resp$data %>%
  group_by(state) %>%
  summarise(total_population = sum(population))   %>%
  mutate(category = case_when(
    total_population > 10000000 ~ "mas_diez",
    total_population > 5000000 ~ "mas_cinco",
    TRUE ~ "menos_cinco"
  ))

age_data_by_state <- age_data_by_state %>% ungroup() %>%
  inner_join(data_by_state, by = "state") %>%
  inner_join(rangos, by = c("age_range" = "rangos_edades"))

st_categories <- c("mas_diez", "mas_cinco", "menos_cinco")
names(st_categories) <- c("Más de 10 mill. hab.", "Más de 5 mill. hab.", "Menos de 5 mill. hab.")

for (i in 1:length(st_categories)) {
  p <- age_data_by_state %>%
        filter(category == st_categories[i]) %>%
        ggplot(aes(x = health_care, fill = rangos_20, y = population)) +
        geom_bar(stat = "identity") +
        theme_clean(base_size = 25) +
        facet_wrap(~paste0(state))+ 
        labs(x = "", y = "Total de habitantes", title = "Habitantes por institución de salud", 
        subtitle = names(st_categories[i])) +
        scale_fill_stata(name = "Rango de edades") +
        theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) 

  png(paste0("plots/hab_institucion_", st_categories[i], ".png"), width = 1400, height = 1000)
  print(p)
  dev.off()
}

age_data_by_state <- age_data_by_state %>%
  mutate(fraction = (population/total_population)*100)

p <- age_data_by_state %>%
  ggplot(aes(x = health_care, fill = rangos_20, y = fraction)) +
  geom_bar(stat = "identity") +
  theme_clean(base_size = 25) +
  facet_wrap(~paste0(state))+ 
  labs(x = "", y = "Porcentaje de habitantes", title = "Porcentaje de habitantes por institución de salud") + 
  scale_fill_stata(name = "Rango de edades") +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) 

png(paste0("plots/porcentaje_habitantes.png"), width = 1800, height = 1400)
print(p)
dev.off()
