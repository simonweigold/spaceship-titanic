# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(rio)
library(tidymodels)
library(corrplot)
library(tune)
library(scales)
library(parallel)
library(doParallel)


# Data import -------------------------------------------------------------
train <- rio::import(here::here("data", "train.csv"))
test <- rio::import(here::here("data", "test.csv"))


# Pre-processing ----------------------------------------------------------
# Load pre-processing function
source(here::here("scripts", "R data preprocessing function.R"))
# Train data
clean_train <- clean_data(train)
clean_train$Transported <- as.character(clean_train$Transported) # Transported only exists in Train!
# Test data
clean_test <- clean_data(test)
# get some validation on train data set by splitting "train" by 80/20 ratio
set.seed(42)
train_split <- clean_train %>% initial_split(prop = 0.80, strata = Transported)
# Create DFs
train_data <- training(train_split)
test_data <- testing(train_split)
# prepare for modelling
train_rec <-recipe(Transported ~., data = train_data) %>%
  step_normalize(Age, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck) %>%
  step_dummy(CryoSleep, Cabin_side, VIP, group, one_hot = TRUE) %>% 
  step_dummy(HomePlanet, Cabin_deck, Destination, one_hot = FALSE)
train_folds <- vfold_cv(data = train_data, v = 5) #cross validation
