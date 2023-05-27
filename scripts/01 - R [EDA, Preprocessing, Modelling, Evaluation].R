# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(rio)
library(tidymodels)


# Data import -------------------------------------------------------------
train <- rio::import(here::here("data", "train.csv"))
test <- rio::import(here::here("data", "test.csv"))


# Pre-processing ----------------------------------------------------------
clean_train <- separate(train, col = Cabin,
                        into = c("Cabin_deck", "Cabin_num", "Cabin_side"),
                        sep = "/") #categorise cabin position
clean_train$PassengerId = substr(clean_train$PassengerId, start = 1, stop = 4) #define whether passenger is alone or in a group
clean_train$group<- ifelse(duplicated(clean_train$PassengerId) |
                             duplicated(clean_train$PassengerId, fromLast = TRUE),
                           "Multiple", "Unique")
clean_train <- clean_train %>% select(-c(PassengerId, Name)) #remove redundant variables
clean_train <- clean_train %>% mutate_all(~ifelse(. == "", NA, .)) #define empty entries as NA
clean_train <- clean_train %>% drop_na() #remove NAs
