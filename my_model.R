library(tidyverse)
library(vroom)
library(tidymodels)
library(sf)

#modelo de la forma 

#letalidad ~ camas totales + camas imss + camas issste + camas otras + %pop joven + %pop media + %pop mayor

#un modelo para cada periodo del tipo 

#alta transmision, no intervencion farmacologica
#baja transmision, no intervencion farmacologica
#alta transmisión, intervencion farmacologica
#baja transmisión, intervención farmacologica 


#segundo modelo con distancia a las zonas 

#datos ---- 

poblacion <- vroom("~/DATA/censo_2020/iter_00_cpv2020_csv/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv") %>% filter(NOM_LOC=="Total del Municipio")


pob.mun <- 
  poblacion %>% 
  mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
  select(CVEGEO, POBTOT, POBFEM, POB0_14, POB15_64, POB65_MAS) %>% 
  mutate(across(-CVEGEO, as.numeric)) %>% 
  mutate(POB0_14 = 100*POB0_14/POBTOT, 
         POB15_64 = 100*POB15_64/POBTOT, 
         POB65_MAS = 100*POB65_MAS/POBTOT, 
         POBFEM = 100*POBFEM/POBTOT
         ) %>% 
  select(-POBTOT)

pob.mun %>% vroom_write("etl/vars_pop.txt")
# letalidad ---- 

letalidad_covid <- vroom("letalidad_por_olas_covid.txt")

# pruebas ---- 

num_pruebas <- vroom("pruebas_por_ola.txt")

num_pruebas <- 
  poblacion %>% 
  mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
  select(CVEGEO, POBTOT) %>% 
  left_join(num_pruebas, .) %>% 
  mutate(pruebas_dia_capita = pruebas_dia_promedio/POBTOT)

num_pruebas.clean <- 
  num_pruebas %>% 
  select(CVEGEO, ola, pruebas_dia_capita)

#camas per capita --- 

camas_capita <- vroom("data/poblacion_camas_clinicas_institucion.tsv")

camas_capita <- 
  camas_capita %>% 
  mutate(state_id = str_pad(string = state_id, width = 2, side = "left", pad = 0),
         municipality_id = str_pad(string = municipality_id, width = 5, side = "left", pad = 0),
         CVEGEO = municipality_id
  ) %>% 
  #hay algunos repetidos, vamos a resumirlos
  group_by(CVEGEO, state, municipality, institucion_id, institucion, poblacion) %>% 
    summarise(beds = sum(beds),
              clinics = sum(clinics)
              ) %>% 
  ungroup() %>% 
  mutate(beds_capita = beds/poblacion, 
         clinics_capita = clinics/poblacion
  ) 


#vamos a pivotear de una vez no 
camas_capita.pivot <- 
  camas_capita %>% 
  pivot_wider(id_cols = CVEGEO, 
              names_from = institucion_id, 
              values_from = c(beds_capita, clinics_capita) 
              )

#sacar la cobertura per capita 

cobertura_per_capita <- 
  camas_capita %>% 
  select(CVEGEO, institucion_id, poblacion) %>% 
  left_join(y = (poblacion %>% mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% select(CVEGEO, POBTOT))
            ) %>% 
  mutate(cobertura = 100*poblacion/POBTOT) %>% 
  select(CVEGEO, institucion_id, cobertura) %>% 
  pivot_wider(id_cols = CVEGEO, names_from = institucion_id, names_prefix = "cobertura_pc_", values_from = cobertura)

# shapefiles de municipios, para mapitas ---- 

shp_mun <- read_sf("data/conjunto_de_datos/00mun.shp")

# left_join(shp_mun, camas_capita) %>% 
#   ggplot()  + 
#   aes(fill = beds_capita) + 
#   geom_sf(lwd=0.1) + 
#   facet_wrap(vars(institucion)) + 
#   theme_minimal() + 
#   scale_fill_viridis_c(option="B")


# integrar el input ---  

x <- left_join(letalidad_covid, camas_capita.pivot)
x <- left_join(x, pob.mun)
x <- left_join(x, num_pruebas.clean)
x <- left_join(x, cobertura_per_capita)

# definir periodo de transmision 

x$ola %>% table()

#scenario 1: HTNPI  - 03_ola_2
#scenario 2: LTNPI  - 02_interola_1, 04_interola_2
#scenario 3: HTPI   - 05_ola_3
#scenario 4: LTPI   - 06_interola_3 

#oob scenarios: 
#scenario x: LowDetection - 01_ola_1 
#scenario omega: ultrahigh transmission - 07_ola_4 

scenario_1 <- x %>% filter(ola == "03_ola_2")
scenario_2 <- x %>% filter(ola%in%c("02_interola_1", "04_interola_2"))
scenario_3 <- x %>% filter(ola%in%c("05_ola_3"))
scenario_4 <- x %>% filter(ola%in%c("06_interola_3"))


################################################################################

# hacemos el modelo de RF, regresion 

tempus <- Sys.time()
#hago los splits

y <- scenario_1 %>% drop_na() %>% select(-CVEGEO, -ola, -casos_totales, -defunciones)

set.seed(725)
my_split <- initial_split(y, prop = 0.75)
my_train <- training(my_split)
my_test  <- testing(my_split)

#y los cv
set.seed(725)
my_folds <- vfold_cv(data = my_train, v = 10)

#defino mi modelo
rf_mod <- 
  rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") 

#defino un workflow 

rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(cfr ~ .)

#hago el fit sobre los resamples

set.seed(725)
my_cv_fits <- 
  rf_wf %>% 
  #parsnip::fit(data = my_train)
  fit_resamples(my_folds)

my_metrics <- collect_metrics(my_cv_fits)

#vamos a tunear parametros no? 

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

set.seed(123)
tree_res <- 
  rf_wf %>% 
  tune_grid(
    resamples = my_folds,
    grid = tree_grid
  )


my_metrics.tune <- collect_metrics(tree_res)

best_tree <- tree_res %>%
  select_best("rsq")

final_wf <- 
  rf_wf %>% 
  finalize_workflow(best_tree)

#hago fit sobre el training set 

set.seed(725)
my_fit <- 
  final_wf %>% 
  last_fit(my_split)
  
tempus <- Sys.time()   - tempus
  
xxx <- 
  my_fit %>% pluck(".workflow", 1) %>%   
  extract_fit_parsnip() 

var_importance <- 
  xxx$fit$variable.importance %>% tibble(variable = names(.), 
                                       importance = .
                                       )


var_importance %>% arrange(desc(importance))

y %>% 
  ggplot() + 
  aes(POB65_MAS, cfr) + 
  geom_point() 

y %>% 
  ggplot() + 
  aes(POB65_MAS, cfr) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10()


#ajustar para cada escenario ---- 

mis_escenarios <- 
  list(HTMPI = scenario_1,
       LTMPI = scenario_2,
       HTPI  = scenario_3, 
       LTPI  = scenario_4
       )

mis_modelos <- 
  lapply(mis_escenarios, FUN = function(i){
    
    # hacemos el modelo de RF, regresion 
    
    tempus <- Sys.time()
    #hago los splits
    
    y <- i %>% drop_na() %>% select(-CVEGEO, -ola, -casos_totales, -defunciones)
    
    set.seed(725)
    my_split <- initial_split(y, prop = 0.75)
    my_train <- training(my_split)
    my_test  <- testing(my_split)
    
    #y los cv
    set.seed(725)
    my_folds <- vfold_cv(data = my_train, v = 10)
    
    #defino mi modelo
    rf_mod <- 
      rand_forest(mtry = 10, trees = 2000) %>%
      set_engine("ranger", importance = "impurity") %>%
      set_mode("regression") 
    
    #defino un workflow 
    
    rf_wf <- 
      workflow() %>%
      add_model(rf_mod) %>%
      add_formula(cfr ~ .)
    
    #hago el fit sobre los resamples
    
    set.seed(725)
    my_cv_fits <- 
      rf_wf %>% 
      #parsnip::fit(data = my_train)
      fit_resamples(my_folds)
    
    my_metrics <- collect_metrics(my_cv_fits)
    
    #vamos a tunear parametros no? 
    
    tree_grid <- grid_regular(cost_complexity(),
                              tree_depth(),
                              levels = 5)
    
    set.seed(123)
    tree_res <- 
      rf_wf %>% 
      tune_grid(
        resamples = my_folds,
        grid = tree_grid
      )
    
    
    my_metrics.tune <- collect_metrics(tree_res)
    
    best_tree <- tree_res %>%
      select_best("rsq")
    
    final_wf <- 
      rf_wf %>% 
      finalize_workflow(best_tree)
    
    #hago fit sobre el training set 
    
    set.seed(725)
    my_fit <- 
      final_wf %>% 
      last_fit(my_split)
    
    tempus <- Sys.time()   - tempus
    
    xxx <- 
      my_fit %>% pluck(".workflow", 1) %>%   
      extract_fit_parsnip() 
    
    var_importance <- 
      xxx$fit$variable.importance %>% tibble(variable = names(.), 
                                             importance = .
      )
    
    
    var_importance <-var_importance %>% arrange(desc(importance))
    
    resultados = list(var_importance = var_importance, 
                      metrics_tune   = my_metrics.tune, 
                      my_time        = tempus 
                      )
    
    return(resultados)
    
    
  })

mis_modelos %>% 
  lapply(function(i){i[["metrics_tune"]]})

mis_modelos %>% 
  lapply(function(i){i[["var_importance"]]}) %>% 
  bind_rows(.id = "Escenario")

write_rds(mis_modelos, "modelo_v1.rds")
