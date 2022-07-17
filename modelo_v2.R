################################################################################
#bibliotecas --- 
################################################################################

library(tidyverse)
library(vroom)
library(tidymodels)
library(sf)





################################################################################
#leer datos --- 
################################################################################

#datos covid cfr ----
cfr <- vroom("letalidad_por_olas_covid.txt")

#datos poblacion ----
pop <- vroom("etl/vars_pop.txt")

#datos recursos de salud vecinos ---- 
recursos <- vroom("etl/recursos_propios_vecinos.txt")

#pruebas por mun  ----

num_pruebas <- vroom("pruebas_por_ola.txt")

#para normalizar necesito poblaciones 
poblacion <- vroom("~/DATA/censo_2020/iter_00_cpv2020_csv/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv") %>% filter(NOM_LOC=="Total del Municipio")

num_pruebas <- 
  poblacion %>% 
  mutate(CVEGEO = paste0(ENTIDAD, MUN)) %>% 
  select(CVEGEO, POBTOT) %>% 
  left_join(num_pruebas, .) %>% 
  mutate(pruebas_dia_capita = pruebas_dia_promedio/POBTOT) %>% 
  select(CVEGEO, ola, pruebas_dia_capita)


################################################################################
#unir datos --- 
################################################################################

x <- 
  left_join(cfr, pop) %>% 
  left_join(recursos) %>% 
  left_join(y = num_pruebas)

x <- 
  x %>% 
  select(!where(is.logical)) %>% # hay unas columnas que son puros NA por el pivoteo; tumbar
  mutate(across(contains("contrib"), .fns = function(i,...){ifelse(is.na(i), 0, i)}))  #hay unos NA que me conviene sean 0s

#splitties! ----

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

mis_escenarios <- 
  list(HTMPI = scenario_1,
       LTMPI = scenario_2,
       HTPI  = scenario_3, 
       LTPI  = scenario_4
  )
################################################################################
#modelar --- 
################################################################################
mis_modelos <- 
  lapply(mis_escenarios, FUN = function(i){
    
    # hacemos el modelo de RF, regresion 
    
    tempus <- Sys.time()
    #hago los splits
    
    y <- 
      i %>% 
      drop_na() %>% 
      select(-CVEGEO, -ola, -casos_totales, -defunciones)
    
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
################################################################################
#extraer importancias --- 
################################################################################

mis_modelos$HTMPI$metrics_tune
lapply(mis_modelos, function(i){i[["var_importance"]]})

write_rds(mis_modelos, "modelo_v2.rds")

# x %>%
#   #filter(casos_totales > 1000) %>% 
#   ggplot() + 
#   aes(mun_derechohabientes_PDER_IMSS, cfr) + 
#   geom_smooth(method = "lm")  + 
#   geom_point() + 
#   facet_wrap(vars(ola)) + 
#   ggpubr::stat_regline_equation(aes(label = ..rr.label..))
