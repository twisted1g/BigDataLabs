library(learningtower)
library(dplyr)
library(zoo)

setwd("~/Code/r/BigDataLabs/lab3")
# 1. Выбрать данные согласно варианту по указанным 7-ми странам, освободить от NA.

students_data <- readRDS("./students_data.rds")
students_data

countrycode

# Подготовка данных
# "China"и "India" 

new_students_data <- left_join(students_data, countrycode, 
                               by = c("country" = "country"))

length(unique(new_students_data$country_name))

new_students_data$country_name[grepl("China",
                new_students_data$country_name) ] <- "China"

new_students_data$country_name[grepl("India",
                new_students_data$country_name) ] <- "India"

length(unique(new_students_data$country_name))

countries <- c(
  "Australia",
  "Japan",
  "China",
  "India",
  "Indonesia",
  "United States",
  "Turkey",
  "Greece"
)

# Выборка по странам
students <- new_students_data %>%
  filter(country_name %in% countries)

unique(students$country_name)

# Удаление na
# students <- students %>%
#   filter(
#     !is.na(math),
#     !is.na(read),
#     !is.na(science),
#     !is.na(computer_n)
#   )

# Так получилось точнее, но есть выбросы
# students$math[is.na(students$math)] <- 0
# students$read[is.na(students$read)] <- 0
# students$science[is.na(students$science)] <- 0
# students$computer_n[is.na(students$computer_n)] <- 0


# Линейная интерполяция
# library(zoo)
# 
# safe_approx <- function(x, year) {
#   if (sum(!is.na(x)) < 2) return(x)
#   tryCatch(
#     na.approx(x, x = year, rule = 2, na.rm = FALSE),
#     error = function(e) x  # при любой ошибке возвращаем оригинал
#   )
# }
# 
# students <- students %>%
#   arrange(country_name, year) %>%
#   group_by(country_name) %>%
#   mutate(
#     math = safe_approx(math, year),
#     read = safe_approx(read, year),
#     science = safe_approx(science, year),
#   ) %>%
#   ungroup()


# 2. Выполнить подсчет средних оценок по трем признакам для каждого года.

mean_scores <- students %>% 
  group_by(country_name, year) %>%
  summarise(
    mean_math = mean(math),
    mean_read = mean(read),
    mean_science = mean(science),
    .groups = "drop"
  )

mean_scores

# 3. Вывести графики динамики изменения средней оценки по трем параметрам – математика,
# чтение и наука (функциональные графики с легендой), столбчатую диаграмму для сравнения
# средних оценок (математика, чтение и наука) по одной и той же стране в крайний год в
# выборке.


for (country in countries){
  # Графики средних оценок по годам
  country_data <- mean_scores %>%
    filter(country_name == country)
  
  y_min <- min(c(country_data$mean_math,
                country_data$mean_read,
                country_data$mean_science), na.rm = TRUE) * 0.95

  y_max <- max(c(country_data$mean_math,
                country_data$mean_read,
                country_data$mean_science), na.rm = TRUE) * 1.05
  
  plot(country_data$year, country_data$mean_math,
       type = "b",
       lty = 5,
       col = "blue",
       ylim = c(y_min, y_max) ,
       xlab = "Год",
       ylab = "Средние оценки",
       main = country,
       axes = FALSE)
  
  axis(1, at=country_data$year, las=1)
  
  axis(2, at = pretty(c(y_min, y_max), n=8), las = 1)
  
  lines(country_data$year, country_data$mean_read, 
        col = "red", type = "b", lty = 5)
  
  lines(country_data$year, country_data$mean_science, 
        col = "green", type = "b", lty = 5)
  
  legend("topright",
         legend = c("Математика", "Чтение", "Наука"),
         col = c("blue", "red", "green"),
         pch = 1,
         lty = 5)

# Столбчатые диаграммы за последний год
    
  year_country_data <- country_data %>%
      filter(year == max(year))
  
  bp <- barplot(c(year_country_data$mean_math,
                  year_country_data$mean_read,
                  year_country_data$mean_science),
                names.arg = c("Математика", "Чтение", "Наука"),
                col = c("blue", "red", "green"),
                ylab = "Средние оценки",
                ylim = c(0, y_max),
                main = paste("Средние оценки в", country, "за", 
                             max(year_country_data$year), "год"),
                width = 0.6,
                space = 0.8,
                axes = FALSE)
  
  axis(1, at = bp, labels = c("Математика", "Чтение", "Наука"))
  axis(2, at = pretty(c(0, y_max), n = 8), las = 1)
}


# 4. Разделить датасет по гендерному признаку, построить круговую диаграмму (pie) средних по
# трем признакам для мужчин и женщин (шесть долек) подписями и легендой.

gender_counts <- table(students$gender)
percent <- round(gender_counts / sum(gender_counts) * 100, 1)

pie(gender_counts,
    labels = paste(names(gender_counts), percent, "%"),
    col = c("lightblue", "pink"),
    main = "Распределение студентов по полу")

# 5. Построить по одной гистограмме распределений оценок по математике для мужчин для
#женщин.
par(mfrow = c(1,2))
for (subj in subjects <- c("math", "read", "science")){
  subj
  hist(students[[subj]][students$gender == "male"],
       main = paste("Распределение оценок по", subj, "(мужчины)"),
       xlab = "Оценка",
       col = "lightblue")
  
  hist(students[[subj]][students$gender == "female"],
       main = paste("Распределение оценок по", subj, "(женщины)"),
       xlab = "Оценка",
       col = "pink")
}
par(mfrow = c(1,1))

# Данные распределены нормально, но есть выбросы. 
# По правилу трех сигм отсеиваем выбросы

mean_math <- mean(students$math, na.rm = TRUE)
sd_math <- sd(students$math, na.rm = TRUE)

mean_read <- mean(students$read, na.rm = TRUE)
sd_read <- sd(students$read, na.rm = TRUE)

mean_science <- mean(students$science, na.rm = TRUE)
sd_science <- sd(students$science, na.rm = TRUE)

students <- students %>%
  filter(
    math >= mean_math - 3*sd_math & math <= mean_math + 3*sd_math,
    read >= mean_read - 3*sd_read & read <= mean_read + 3*sd_read,
    science >= mean_science - 3*sd_science & science <= mean_science + 3*sd_science
  )



# 6. Построить в одной плоскости графики по 7-ми заданным странам (в моем примере сделано по
# двум) для сопоставления динамики изменение способностей учеников в разных странах.

colors <- rainbow(length(countries))

for (subj in c("math", "read", "science")) {
  plot(NULL,
       xlim = range(mean_scores$year),
       ylim = c(min(mean_scores$mean_math, mean_scores$mean_read, mean_scores$mean_science),
                max(mean_scores$mean_math, mean_scores$mean_read, mean_scores$mean_science) * 1.1),
       xlab = "Год",
       ylab = "Средние оценки",
       main = paste("Динамика оценок по", subj, "по странам"),
       xaxt = "n"
       )
  
  axis(1, at=mean_scores$year, las=1)
  
  for (i in seq_along(countries)) {
    country_data <- mean_scores %>% filter(country_name == countries[i])
    
    lines(country_data$year, country_data[[paste0("mean_", subj)]],
          type = "b",
          lty = 3,
          col = colors[i],
          pch = i)
  }
  
    legend("top",
         legend = countries,
         lty = 3,
         col = colors,
         pch = seq_along(countries),
         ncol = 2)
}


# 7. Выполните подсчет среднего количества компьютеров у учащихся в первом в вашей выборке
# году, последнем и в середине выборки, постройте круговую диаграмму.

unique(students$computer_n)

# Среднее количество особо не меняется для разных способов обработки na
# students <- students %>%
#   filter(!is.na(computer_n))

students$computer_n <- as.numeric(as.character(
  recode(as.character(students$computer_n), "3+" = "3")
))

students


for (country_full in countries) {
  country_data <- students %>%
    filter(country_name == country_full)
  
  years <- sort(unique(country_data$year))
  first_year  <- years[1]
  last_year   <- years[length(years)]
  middle_year <- years[round(length(years) / 2)]
  
  avg_computers <- country_data %>%
    filter(year %in% c(first_year, middle_year, last_year)) %>%
    group_by(year) %>%
    summarise(mean_computers = mean(computer_n, na.rm = TRUE))
  
  avg_computers <- avg_computers %>% filter(is.finite(mean_computers))
  
  if (nrow(avg_computers) == 0) next
  
  pie(avg_computers$mean_computers,
      labels = paste0(avg_computers$year, "\n", round(avg_computers$mean_computers, 2)),
      col = c("steelblue", "orange", "lightgreen")[1:nrow(avg_computers)],
      main = paste("Среднее количество компьютеров в", country_full))
}

