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
  
  # Remove rows with NAs
  data <- drop_na(data)
  
  return(data)
}

