# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(rio)
library(tidymodels)


# Data import -------------------------------------------------------------
train <- rio::import(here::here("data", "train.csv"))
test <- rio::import(here::here("data", "test.csv"))



