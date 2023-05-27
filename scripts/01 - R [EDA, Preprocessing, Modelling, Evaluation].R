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
clean_train <- separate(train, col = Cabin,
                        into = c("Cabin_deck", "Cabin_num", "Cabin_side"),
                        sep = "/") #categorise cabin position
clean_train$Cabin_num <- as.numeric(clean_train$Cabin_num)
clean_train$PassengerId = substr(clean_train$PassengerId, start = 1, stop = 4) #define whether passenger is alone or in a group
clean_train$group<- ifelse(duplicated(clean_train$PassengerId) |
                             duplicated(clean_train$PassengerId,
                                        fromLast = TRUE),
                           "Multiple", "Unique")
clean_train <- clean_train %>% select(-c(PassengerId, Name)) #remove redundant variables
clean_train <- clean_train %>% mutate_all(~ifelse(. == "", NA, .)) #define empty entries as NA
clean_train <- clean_train %>% drop_na() #remove NAs


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


# Modelling ---------------------------------------------------------------

# Model evaluation --------------------------------------------------------
