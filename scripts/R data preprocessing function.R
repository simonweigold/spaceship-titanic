clean_data <- function(data) {
  library(dplyr)
  library(tidyr)
  
  # Separate Cabin column into three variables
  data <- separate(data, col = Cabin, into = c("Cabin_deck", "Cabin_num", "Cabin_side"), sep = "/")
  
  # Categorize CryoSleep, and VIP variables as characters
  data$CryoSleep <- as.character(data$CryoSleep)
  data$VIP <- as.character(data$VIP)
  
  # Convert Cabin_num to numeric
  data$Cabin_num <- as.numeric(data$Cabin_num)
  
  # Define whether passenger is alone or in a group based on PassengerId
  data$PassengerId <- substr(data$PassengerId, start = 1, stop = 4)
  data$group <- ifelse(duplicated(data$PassengerId) | duplicated(data$PassengerId, fromLast = TRUE),
                       "Multiple", "Unique")
  
  # Remove redundant variables
  data <- select(data, -c(PassengerId, Name))
  
  # Define empty entries as NA
  data <- mutate_all(data, ~ifelse(. == "", NA, .))
  
  # Engineer NAs
  #data$example[is.na(data$example)] <- median(data$example, na.rm = TRUE)
  data$Cabin_num[is.na(data$Cabin_num)] <- median(data$Cabin_num, na.rm = TRUE)
  data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)
  data$RoomService[is.na(data$RoomService)] <- median(data$RoomService, na.rm = TRUE)
  data$FoodCourt[is.na(data$FoodCourt)] <- median(data$FoodCourt, na.rm = TRUE)
  data$ShoppingMall[is.na(data$ShoppingMall)] <- median(data$ShoppingMall, na.rm = TRUE)
  data$Spa[is.na(data$Spa)] <- median(data$Spa, na.rm = TRUE)
  data$VRDeck[is.na(data$VRDeck)] <- median(data$VRDeck, na.rm = TRUE)
  #data$example[is.na(data$example)] <- names(which.max(table(data$example)))
  data$HomePlanet[is.na(data$HomePlanet)] <- names(which.max(table(data$HomePlanet)))
  data$CryoSleep[is.na(data$CryoSleep)] <- names(which.max(table(data$CryoSleep)))
  data$Cabin_deck[is.na(data$Cabin_deck)] <- names(which.max(table(data$Cabin_deck)))
  data$Cabin_side[is.na(data$Cabin_side)] <- names(which.max(table(data$Cabin_side)))
  data$Destination[is.na(data$Destination)] <- names(which.max(table(data$Destination)))
  data$VIP[is.na(data$VIP)] <- names(which.max(table(data$VIP)))
  
  return(data)
}

