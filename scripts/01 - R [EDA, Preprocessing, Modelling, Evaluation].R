# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(rio)
library(tidymodels)
library(corrplot)


# Data import -------------------------------------------------------------
train <- rio::import(here::here("data", "train.csv"))
test <- rio::import(here::here("data", "test.csv"))


# Pre-processing 1 --------------------------------------------------------
# Train data
clean_train <- separate(train, col = Cabin,
                        into = c("Cabin_deck", "Cabin_num", "Cabin_side"),
                        sep = "/") #categorise cabin position
clean_train$CryoSleep <- as.character(clean_train$CryoSleep)
clean_train$VIP <- as.character(clean_train$VIP)
clean_train$Transported <- as.character(clean_train$Transported)
clean_train$Cabin_num <- as.numeric(clean_train$Cabin_num)
clean_train$PassengerId = substr(clean_train$PassengerId, start = 1, stop = 4) #define whether passenger is alone or in a group
clean_train$group<- ifelse(duplicated(clean_train$PassengerId) |
                             duplicated(clean_train$PassengerId,
                                        fromLast = TRUE),
                           "Multiple", "Unique")
clean_train <- clean_train %>% select(-c(PassengerId, Name)) #remove redundant variables
clean_train <- clean_train %>% mutate_all(~ifelse(. == "", NA, .)) #define empty entries as NA
clean_train <- clean_train %>% drop_na() #remove NAs

# Test data
clean_test <- separate(test, col = Cabin,
                       into = c("Cabin_deck", "Cabin_num", "Cabin_side"),
                       sep = "/") #categorise cabin position
clean_test$CryoSleep <- as.character(clean_test$CryoSleep)
clean_test$VIP <- as.character(clean_test$VIP)
clean_test$Cabin_num <- as.numeric(clean_test$Cabin_num)
clean_test$PassengerId = substr(clean_test$PassengerId, start = 1, stop = 4) #define whether passenger is alone or in a group
clean_test$group<- ifelse(duplicated(clean_test$PassengerId) |
                            duplicated(clean_test$PassengerId,
                                       fromLast = TRUE),
                          "Multiple", "Unique")
clean_test <- clean_test %>% select(-c(PassengerId, Name)) #remove redundant variables
clean_test <- clean_test %>% mutate_all(~ifelse(. == "", NA, .)) #define empty entries as NA
clean_test <- clean_test %>% drop_na() #remove NAs


# Exploratory Data Analysis (EDA) -----------------------------------------
# Distribution of target variable
table(clean_train$Transported)
# Create plot_data
plot_data <- clean_train %>%
  select(-c(HomePlanet, CryoSleep, Cabin_deck, Cabin_side, Destination, VIP,
            group)) %>% 
  #mutate(HomePlanet = factor(HomePlanet)) %>% 
  #mutate(CryoSleep = factor(CryoSleep)) %>% 
  #mutate(Cabin_deck = factor(Cabin_deck)) %>% 
  #mutate(Cabin_side = factor(Cabin_side)) %>% 
  #mutate(Destination = factor(Destination)) %>% 
  #mutate(VIP = factor(VIP)) %>% 
  #mutate(group = factor(group)) %>% 
  pivot_longer(cols = -Transported, names_to = "key", values_to = "value")

# Visualise relation distribution between variables and quality
plot_data %>%
  ggplot(aes(x = value, fill = Transported)) + 
  geom_density(alpha = 0.6) +
  facet_wrap(~ key, scales = "free")
# Visualise relation between variables and quality in boxplots
plot_data %>%
  ggplot(aes(y = value, fill = Transported))+
  geom_boxplot(alpha = 0.6)+
  facet_wrap(~key, scales = "free")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
# Visualise correlation between variables and quality
clean_train %>%
  select(-c(HomePlanet, CryoSleep, Cabin_deck, Cabin_side, Destination, VIP,
            group)) %>% 
  cor() %>% 
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos = "lt",
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)


# Pre-processing 2 --------------------------------------------------------
train_rec <-recipe(Transported ~., data = clean_train) %>%
  step_normalize(Age, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck) %>%
  step_dummy(CryoSleep, Cabin_side, VIP, group, one_hot = TRUE) %>% 
  step_dummy(HomePlanet, Cabin_deck, Destination, one_hot = FALSE)

train_folds <- vfold_cv(data = clean_train, v = 5) #cross validation


# Specify model type and computational engine -----------------------------
gbt_model <-
  boost_tree()%>% #Model type: Gradient Boosted Tree
  set_engine("xgboost")%>% # omputational engine: xgboost
  set_mode("classification")%>% #Specify model mode
  set_args(mtry = 3, trees = tune(),
           learn_rate = tune(),
           tree_depth = 6, min_n = 1)


# Create a regular tune grid ----------------------------------------------
gbt_grid <- grid_regular(range_set(trees(), c(300,600)),
                         range_set(learn_rate(trans = NULL), c(0.005, 0.02)),
                         levels = 4)


# Create workflow ---------------------------------------------------------
gbt_wflow <- 
  workflow() %>% 
  add_model(gbt_model) %>%
  add_recipe(train_rec)


# Analyse resamples -------------------------------------------------------
system.time(
  gbt_res <- 
    gbt_wflow %>% 
    tune_grid(resamples = train_folds, grid = gbt_grid,
              #metrics = metric_set(huber_loss), #perhaps delete
              control = control_grid(verbose = T)) #print process in console
)


# Collect metrics ---------------------------------------------------------
collect_metrics(gbt_res)
autoplot(gbt_res)


# Finalise model workflow -------------------------------------------------
best_gbt <- select_best(x = gbt_res)

final_wf <- 
  gbt_wflow %>% 
  finalize_workflow(best_gbt)


# Fit and predict ---------------------------------------------------------
final_gbt <- fit(final_wf, clean_train)

gbt_pred <- predict(final_gbt, new_data = clean_test)

predictions <- clean_test %>% 
  bind_cols(., gbt_pred)

