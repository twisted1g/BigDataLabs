install.packages("learningtower")
install.packages("ggplot2")

library(learningtower)
library(dplyr)
library(ggplot2)

??students

students_data <- load_student("all")
data(countrycode)
colnames(students_data)

new_students_data <- left_join(students_data, countrycode, 
          by = c("country" = "country"))


colnames(new_students_data)
sort(unique(new_students_data$country_name))



# Австралия, Япония, Китай, Индия, Индонезия, США, Турция, Греция

students_australia <- subset(new_students_data, country_name=="Australia")
students_australia

students_japan <- subset(new_students_data, country_name=="Japan")
students_japan

students_china <- subset(new_students_data, grepl("China", country_name))
students_china

students_india <- subset(new_students_data, country_name=="India")
students_india

students_indonesia <- subset(new_students_data, country_name=="Indonesia")
students_indonesia

students_usa <- subset(new_students_data, country_name=="United States")
students_usa

students_turkey <- subset(new_students_data, country_name=="Turkey")
students_turkey

students_greece <- subset(new_students_data, country_name=="Greece")
students_greece


prepare_data <- function(df) {
  df$mother_educ <- NULL
  df$father_educ <- NULL
  
  ## Дропнуть лишние столбцы и обработать строки с NA
  df$computer[is.na(df$computer)] <- 0
  df$math[is.na(df$math)] <- 0
  df$read[is.na(df$read)] <- 0
  df$science[is.na(df$science)] <- 0
  return(df)
}



