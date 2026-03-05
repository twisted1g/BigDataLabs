install.packages("learningtower")
install.packages("ggplot2")

library(learningtower)
library(dplyr)
library(ggplot2)

??students

# 1. Выбрать данные согласно варианту по указанным 7-ми странам, освободить от NA.

# Скачиивание данных

#students_data <- load_student("all")
#students_data

#saveRDS(students_data, "students_data.rds")

# Загрузка данных
students_data <- readRDS("students_data.rds")
students_data

data(countrycode)
colnames(students_data)


# Подготовка данных
new_students_data <- left_join(students_data, countrycode, 
          by = c("country" = "country"))


colnames(new_students_data)
sort(unique(new_students_data$country_name))


# Австралия, Япония, Китай, Индия, Индонезия, США, Турция, Греция

# Выборка по стране
subset_data <- function(df, target_country, is_grep = FALSE){
  if (is_grep){
    return(subset(df, grepl(target_country, country_name)))
  }
  else{
    return(subset(df, country_name==target_country))
  }
}

# Обработка na
prepare_data <- function(df) {
  # Дропаю ненужные для анализа данные
  cols_to_drop <- c("mother_educ", "father_educ", "computer", "internet", 
                    "stu_wgt", "desk", "room", "dishwasher", "television",
                    "car", "book", "wealth", "escs")
  
  df[, cols_to_drop] <- NULL
  
  # Замена na на 0
  df$computer_n[is.na(df$computer_n)] <- 0
  df$math[is.na(df$math)] <- 0
  df$read[is.na(df$read)] <- 0
  df$science[is.na(df$science)] <- 0
  return(df)
}

# Формирования выборок
students_australia <- subset_data(df=new_students_data, target_country="Australia") %>% prepare_data
students_australia

students_japan <- subset_data(df=new_students_data, target_country="Japan") %>% prepare_data
students_japan

students_china <- subset_data(df=new_students_data, target_country="China", is_grep=TRUE) %>% prepare_data
students_china

students_india <- subset_data(df=new_students_data, target_country="India", is_grep=TRUE) %>% prepare_data
students_india
 
students_indonesia <- subset_data(df=new_students_data, target_country="Indonesia") %>% prepare_data
students_indonesia

students_usa <- subset_data(df=new_students_data, target_country="United States") %>% prepare_data
students_usa

students_turkey <- subset_data(df=new_students_data, target_country="Turkey") %>% prepare_data
students_turkey

students_greece <- subset_data(df=new_students_data, target_country="Greece") %>% prepare_data
students_greece


students_countries <- list(students_australia, students_japan, students_china, students_india,
                        students_indonesia, students_usa, students_turkey, students_greece)


# 2. Выполнить подсчет средних оценок по трем признакам для каждого года.

calc_and_viz_mean_scores <- function(df){
  mean_scores_by_year <- aggregate(
    cbind(math, read, science) ~ year,
    data = df,
    FUN = mean,
    na.rm = TRUE
  )
  
  print(mean_scores_by_year)
}


for (i in students_countries){
  calc_and_viz_mean_scores(i)
}

# 
