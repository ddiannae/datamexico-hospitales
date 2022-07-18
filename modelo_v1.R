library(tidyverse)
library(vroom)
library(tidymodels)
library(sf)

# Modelo de la forma:
# letalidad ~ camas totales + camas imss + camas issste + camas otras + %pop joven + %pop media + %pop mayor

#un modelo para cada periodo del tipo 
#alta transmision, no intervencion farmacologica
#baja transmision, no intervencion farmacologica
#alta transmisión, intervencion farmacologica
#baja transmisión, intervención farmacologica 

poblacion <- vroom("data/poblacion_demo.tsv")

# Obtener porcentaje de población femenina y grupos de edad
pob_mun <- poblacion %>% 
  mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
  select(CVEGEO, POBTOT, POBFEM, POB0_14, POB15_64, POB65_MAS) %>% 
  mutate(across(-CVEGEO, as.numeric)) %>% 
  mutate(POB0_14 = 100*POB0_14/POBTOT, 
         POB15_64 = 100*POB15_64/POBTOT, 
         POB65_MAS = 100*POB65_MAS/POBTOT, 
         POBFEM = 100*POBFEM/POBTOT
         ) %>% 
  select(-POBTOT)

# Guardar datos
pob_mun %>% vroom_write("etl/vars_pop.txt")

# Leer datos de letalidad y pruebas de covid
letalidad_covid <- vroom("data/letalidad_por_olas_covid.txt")
num_pruebas <- vroom("data/pruebas_por_ola.txt")

# Obtener pruebas per capita por cada cola
num_pruebas <- 
  poblacion %>% 
  mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
  select(CVEGEO, POBTOT) %>% 
  left_join(num_pruebas, .) %>% 
  mutate(pruebas_dia_capita = pruebas_dia_promedio/POBTOT) %>%
  select(CVEGEO, ola, pruebas_dia_capita)

# Obtener datos de recursos y sacar camas y clinicas per capita 
recursos_capita <- vroom("data/recursos_poblacion_by_institucion_municipio.tsv")

recursos_capita <- 
  recursos_capita %>% 
  mutate(state_id = str_pad(string = state_id, width = 2, side = "left", pad = 0),
         municipality_id = str_pad(string = municipality_id, width = 5, side = "left", pad = 0),
         CVEGEO = municipality_id) %>% 
  mutate(camas_capita = camas/poblacion, clinicas_capita = clinicas/poblacion) 

# Obtener dataframe en formato ancho
recursos_capita_pivot <- 
  recursos_capita %>% 
  pivot_wider(id_cols = CVEGEO, names_from = institucion_inegi, 
              values_from = c(camas_capita, clinicas_capita))

# Obtener cobertura per capita 
cobertura_per_capita <- 
  recursos_capita %>% 
  select(CVEGEO, institucion_inegi, poblacion) %>% 
  left_join(y = (poblacion %>% mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% select(CVEGEO, POBTOT))) %>% 
  mutate(cobertura = 100*poblacion/POBTOT) %>% 
  select(CVEGEO, institucion_inegi, cobertura) %>% 
  pivot_wider(id_cols = CVEGEO, names_from = institucion_inegi, names_prefix = "cobertura_pc_", values_from = cobertura)

# Integrar todos los datasets 
x <- left_join(letalidad_covid, recursos_capita_pivot)
x <- left_join(x, pob_mun)
x <- left_join(x, num_pruebas)
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

# Hacer el modelo de Random Forest, regresion 
tempus <- Sys.time()

# Hacer los splits
y <- scenario_1 %>% drop_na() %>% select(-CVEGEO, -ola, -casos_totales, -defunciones)

set.seed(725)
my_split <- initial_split(y, prop = 0.75)
my_train <- training(my_split)
my_test  <- testing(my_split)

# folds para cross validation
set.seed(725)
my_folds <- vfold_cv(data = my_train, v = 10)

# Definir el modelo
rf_mod <- rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") 

# Crear un workflow 
rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_formula(cfr ~ .)

# Hacer fit sobre los resamples
set.seed(725)
my_cv_fits <-  rf_wf %>% 
  fit_resamples(my_folds)

my_metrics <- collect_metrics(my_cv_fits)

# Crear grid para evaluar parámetros
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

set.seed(123)

# Evaluar parametros en grid
tree_res <- rf_wf %>% 
  tune_grid(resamples = my_folds, grid = tree_grid)

# Extraer métricas de evaluación de parámetros
my_metrics_tune <- collect_metrics(tree_res)

# Elegir el modelo con mejor r cuadrada
best_tree <- tree_res %>%
  select_best("rsq")

final_wf <- rf_wf %>% 
  finalize_workflow(best_tree)

# Hacer fit sobre el training set 
set.seed(725)
my_fit <- final_wf %>% 
  last_fit(my_split)
  
tempus <- Sys.time() - tempus
  
# Obtener las variables asociadas al modelo
xxx <- my_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() 

# Obtener la importancia de las variables
var_importance <- xxx$fit$variable.importance %>% 
  tibble(variable = names(.), importance = .)

var_importance %>% arrange(desc(importance))

# Ajustar para cada escenario 
mis_escenarios <- 
  list(HTMPI = scenario_1,
       LTMPI = scenario_2,
       HTPI  = scenario_3, 
       LTPI  = scenario_4)

mis_modelos <- 
  lapply(mis_escenarios, FUN = function(i){
    
    # Hacer el modelo de Random Forest, regresion 
    tempus <- Sys.time()
    
    # Hacer los splits
    y <- i %>% drop_na() %>% select(-CVEGEO, -ola, -casos_totales, -defunciones)
    
    set.seed(725)
    my_split <- initial_split(y, prop = 0.75)
    my_train <- training(my_split)
    my_test  <- testing(my_split)
    
    # folds para cross validation
    set.seed(725)
    my_folds <- vfold_cv(data = my_train, v = 10)

    # Definir el modelo
    rf_mod <- rand_forest(mtry = 10, trees = 2000) %>%
      set_engine("ranger", importance = "impurity") %>%
      set_mode("regression") 
    
    # Crear un workflow 
    rf_wf <- workflow() %>%
      add_model(rf_mod) %>%
      add_formula(cfr ~ .)
    
    # Hacer fit sobre los resamples
    set.seed(725)
    my_cv_fits <-  rf_wf %>% 
      fit_resamples(my_folds)
    
    my_metrics <- collect_metrics(my_cv_fits)
    
    # Crear grid para evaluar parámetros
    tree_grid <- grid_regular(cost_complexity(),
                              tree_depth(),
                              levels = 5)
    
    set.seed(123)
    
    # Evaluar parametros en grid
    tree_res <- rf_wf %>% 
      tune_grid(resamples = my_folds, grid = tree_grid)
    
    # Extraer métricas de evaluación de parámetros
    my_metrics_tune <- collect_metrics(tree_res)
    
    # Elegir el modelo con mejor r cuadrada
    best_tree <- tree_res %>%
      select_best("rsq")
    
    # Finalizar workflow
    final_wf <- rf_wf %>% 
      finalize_workflow(best_tree)
    
    set.seed(725)
    # Hacer fit con el training set
    my_fit <- final_wf %>% 
      last_fit(my_split)
    
    tempus <- Sys.time() -tempus
    
    # Obtener las variables asociadas al modelo
    xxx <- my_fit %>% pluck(".workflow", 1) %>%   
      extract_fit_parsnip() 
    
    # Obtener la importancia de las variables
    var_importance <- xxx$fit$variable.importance %>% 
      tibble(variable = names(.), importance = .)
    var_importance <- var_importance %>% arrange(desc(importance))
    
    resultados = list(var_importance = var_importance, 
                      metrics_tune   = my_metrics_tune, 
                      my_time        = tempus)
    
    return(resultados)
  })

# Métricas obtenidas por los modelos para los diferentes escenarios
mis_modelos %>% 
  lapply(function(i){i[["metrics_tune"]]})

# Variables con mayor importancia en cada escenario
mis_modelos %>% 
  lapply(function(i){i[["var_importance"]]}) %>% 
  bind_rows(.id = "Escenario")

# Guardar resultados del modelo
write_rds(mis_modelos, "resultados/modelo_v1.rds")
