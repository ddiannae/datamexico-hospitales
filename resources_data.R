library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(jsonlite)

dm_resources <- "https://api.datamexico.org/tesseract/data.jsonrecords?cube=health_resources"
mms_resources <- c("Total")
dds_resources <- c("State", "Municipality",  "CLUES", "Type", "Resources+Categories", "Year")
meas <- "measures="
drill <- "drilldowns="
yr <- "Year="
years <- as.character(2017:2021)

resources_url <- paste0(dm_resources, "&",
                        yr, paste(years, collapse = "%2C"), "&",
                        drill, paste(dds_resources, collapse = "%2C"), "&",
                        meas,  paste(mms_resources, collapse = "%2C"))

resp <- jsonlite::fromJSON(txt = resources_url)
tb1 <- resp$data %>% 
  janitor::clean_names() %>% 
  as_tibble() %>%
  filter(total > 0)

years <- as.character(2012:2016)
resources_url <- paste0(dm_resources, "&",
                        yr, paste(years, collapse = "%2C"), "&",
                        drill, paste(dds_resources, collapse = "%2C"), "&",
                        meas,  paste(mms_resources, collapse = "%2C"))

resp <- jsonlite::fromJSON(txt = resources_url)
tb2 <- resp$data %>% 
  janitor::clean_names() %>% 
  as_tibble() %>%
  filter(total > 0)

data <- dplyr::bind_rows(tb1, tb2)

vroom::vroom_write(data, "data/recursos_salud.tsv")

data_by_state <- data %>% 
  group_by(state, resources_categories, year) %>%
  summarise(total = sum(total)) %>%
  mutate(resource_type = case_when(
    grepl("cama", resources_categories, ignore.case = TRUE) ~ "camas",
    grepl("personal|médico", resources_categories, ignore.case = TRUE) ~ "personal",
    TRUE ~ "otro"
  )) 

data_by_state %>% ungroup() %>%
  select(resources_categories, resource_type) %>%
  distinct()

max_by_state <- data_by_state %>%
  group_by(state, resource_type) %>%
  summarise(max_total = max(total)) %>%
  mutate(category = case_when(
    max_total > 5000 ~ "mas_cinco",
    max_total > 3000 ~ "mas_tres",
    max_total > 1000 ~ "mas_mil",
    TRUE ~ "menos_mil"
  )) %>%
  select(-max_total)

max_by_state %>% ungroup() %>% 
  count(category, resource_type)

st_categories <- c("mas_cinco", "mas_tres", "mas_mil", "menos_mil" )
rs_categories <- c("camas","personal", "otro")
names(rs_categories) <- paste0(toupper(substr(rs_categories, 1, 1)), substring(rs_categories, 2))

data_by_state <- data_by_state %>% ungroup() %>%
  inner_join(max_by_state, by = c("state", "resource_type"))

recursos <- unique(data_by_state$resources_categories)
colors <- stata_pal("s2color")(length(recursos))
names(colors) <- recursos

for(j in 1:length(rs_categories)) {
  for (i in 1:length(st_categories)) {
    dd <- data_by_state %>%
      filter(category == st_categories[i] & resource_type == rs_categories[j])
    print(head(dd))
    if(nrow(dd) > 0) {
      p <- ggplot(dd, aes(x = year, y = total, color = resources_categories)) +
        geom_line(size = 1.2) +
      theme_clean(base_size = 25) +
      scale_color_stata(name = "Recurso") +
      labs(x = "Año", y = "Total", title = "Recursos en unidades de salud",
           subtitle = names(rs_categories[j])) +
      theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = "bottom", legend.direction = "vertical") +
      facet_wrap(~state)
    
      png(paste0("plots/recursos_unidad_", st_categories[i], "_", rs_categories[j], ".png"), width = 1400, height = 1000)
      print(p)
      dev.off()
    }
  }
}

