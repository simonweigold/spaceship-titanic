library(here)
source(here::here("scripts", "Load packages and data.R"))


# GBT model ---------------------------------------------------------------
# Specify model type and computational engine
gbt_model <-
  boost_tree()%>% #Model type: Gradient Boosted Tree
  set_engine("xgboost")%>% # omputational engine: xgboost
  set_mode("classification")%>% #Specify model mode
  set_args(mtry = 3, trees = tune(),
           learn_rate = tune(),
           tree_depth = 6, min_n = 1)
# Create a regular tune grid
gbt_grid <- grid_regular(range_set(trees(), c(300,600)),
                         range_set(learn_rate(trans = NULL), c(0.005, 0.02)),
                         levels = 4)
# Create workflow
gbt_wflow <- 
  workflow() %>% 
  add_model(gbt_model) %>%
  add_recipe(train_rec)
# Analyse resamples
# Start parallelisation
#cores <- parallel::detectCores(logical = FALSE)
#cl <- parallel::makePSOCKcluster(cores)
#doParallel::registerDoParallel(cl)
# Resamples
system.time(
  gbt_res <- 
    gbt_wflow %>% 
    tune_grid(resamples = train_folds, grid = gbt_grid,
              #metrics = metric_set(huber_loss), #perhaps delete
              control = control_grid(verbose = T)) #print process in console
)
# End parallelisation
#parallel::stopCluster(cl)
#closeAllConnections()
#showConnections()

# Collect metrics
collect_metrics(gbt_res)
autoplot(gbt_res)


# Finalise model workflow
best_gbt <- select_best(x = gbt_res)
final_wf <- 
  gbt_wflow %>% 
  finalize_workflow(best_gbt)

# Fit and predict
final_gbt <- fit(final_wf, train_data)
gbt_pred <- predict(final_gbt, new_data = test_data)
predictions <- test_data %>% 
  bind_cols(., gbt_pred)

# Check results
predictions$Transported <- as.factor(predictions$Transported)
metrics(predictions, truth = Transported, estimate = .pred_class)
conf_mat(predictions, truth = Transported, estimate = .pred_class)



# Neural network ----------------------------------------------------------
# Specify model type and computational engine
nn_model <-
  mlp() %>% # defines a multilayer perceptron model
  set_engine("nnet") %>% # Computational engine: nnet
  set_mode("classification") %>% # Specify model mode
  set_args(hidden_units = tune(), penalty = tune(), epochs = tune())


# Create a regular tuning grid
nn_grid <- grid_random(range_set(hidden_units(), c(5, 15)),
                       range_set(penalty(trans = NULL), c(0, 1)),
                       range_set(epochs(), c(100, 500)),
                       size = 10)

# Create workflow
nn_wflow <- 
  workflow() %>% 
  add_model(nn_model) %>%
  add_recipe(train_rec)


# Analyse resamples
system.time(
  nn_res <- 
    nn_wflow %>% 
    tune_grid(resamples = train_folds, grid = nn_grid,
              #metrics = metric_set(huber_loss),
              control = control_grid(verbose = T))
)


# Collect metrics
collect_metrics(nn_res)
autoplot(nn_res)
show_best(x = nn_res)


# Finalise model workflow
best_nn <- select_best(x = nn_res)

final_wf <- 
  nn_wflow %>% 
  finalize_workflow(best_nn)


# Fit and predict
final_nn <- fit(final_wf, train_data)

nn_pred <- predict(final_nn, new_data = test_data)

predictions <- test_data %>% 
  select(Transported) %>% # keep target variable (also known as the truth)
  bind_cols(., nn_pred)


# Calculate performance metrics
predictions$Transported <- as.factor(predictions$Transported)
metrics(predictions, truth = Transported, estimate = .pred_class)
conf_mat(predictions, truth = Transported, estimate = .pred_class)





# Final application of model ----------------------------------------------
final_pred <- predict(final_gbt, new_data = clean_test)

clean_test$CryoSleep[clean_test$CryoSleep == "TRUE"] <- TRUE
clean_test$CryoSleep[clean_test$CryoSleep == "FALSE"] <- FALSE
clean_test$VIP[clean_test$VIP == "TRUE"] <- TRUE
clean_test$VIP[clean_test$VIP == "FALSE"] <- FALSE
clean_test <- inner_join(clean_test, test)

final_predictions <- test %>% 
  select(PassengerId) %>% 
  bind_cols(., final_pred)

