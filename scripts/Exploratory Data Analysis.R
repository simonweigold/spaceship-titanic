library(here)
source(here::here("scripts", "Load packages and data.R"))

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
            Transported, group)) %>% 
  cor() %>% 
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos = "lt",
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)