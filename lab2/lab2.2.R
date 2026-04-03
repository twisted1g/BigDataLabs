setwd("~/Code/r/BigDataLabs/lab2")
library(dplyr)

# 1. Выполнить учебный импорт любых таблиц данных из csv-файла и xls-таблицы.

data <- read.csv("опрос.csv", row.names = "id")
head(data)
dim(data)

data[is.na(data)] <- 0
data


# 2. Выполнить дескриптивный анализ данных
descriptive_stats <- function(x) {
  x_clean <- x[!is.na(x)]
  c(
    "Среднее" = mean(x_clean),
    "Медиана" = median(x_clean),
    "Мода" = as.numeric(names(sort(table(x_clean), decreasing = TRUE)[1])),
    "Мин" = min(x_clean),
    "Макс" = max(x_clean),
    "Размах" = max(x_clean) - min(x_clean),
    "Ст.отклонение" = sd(x_clean),
    "25% квантиль" = quantile(x_clean, 0.25),
    "75% квантиль" = quantile(x_clean, 0.75),
    "Межкварт.размах" = IQR(x_clean),
    "Кол-во NA" = sum(is.na(x))
  )
}


descriptive_results <- sapply(data, descriptive_stats)
cat("Дескриптивный анализ: ")
print(round(descriptive_results, 2))

# 3. Выполнить сортировку наборов данных по выбранному признаку
cat("Сортировка по признаку")
sorted_by_chrome <- data[order(data[["Google.Chrome"]], decreasing = TRUE, na.last = TRUE), ]

sorted_by_chrome


# 4. Сформировать отдельные наборы данных по одинаковому признаку 
# (например, составить subdataset, из студентов, отдавших предпочтение по шкале > 0.7 определенной книге),
# вывести результат,  выполнить подсчет размерностей новых таблиц,
# снова выполнить их анализ – гистограмма, боксплот, серединные меры

chrome_users <- subset(data, Google.Chrome > 7)
cat("Размерность:", dim(chrome_users), "\n")
cat("Количество:", nrow(chrome_users), "\n")

descriptive_results <- sapply(chrome_users, descriptive_stats)
cat("Дескриптивный анализ: ")
print(round(descriptive_results, 2))


not_chrome_users <- subset(data, Google.Chrome <= 7 | is.na(Google.Chrome))
not_chrome_users

# Ну по приколу сделал Тест Стъдента для несвязных выборок по конкретному браузеру
# Можно исследование сделать по большему числу бразуеров
t.test(chrome_users$Google.Chrome, not_chrome_users$Google.Chrome, var.equal = FALSE)
t.test(chrome_users$Google.Chrome, not_chrome_users$Google.Chrome, var.equal = TRUE)


# par(mfrow = c(1, 3))

browsers <- colnames(chrome_users)

# Гистограммы

par(mfrow = c(1, 5))

lapply(browsers, function(browser) {
  hist(chrome_users[[browser]],
       breaks = seq(0, 10, by = 1),
       main = paste("Распределение оценок для", browser),
       xlab = "Оценка",
       ylab = "Частота",
       col = "lightgreen",
       border = "darkgreen")
})

lapply(browsers, function(browser) {
  hist(not_chrome_users[[browser]],
       breaks = seq(0, 10, by = 1),
       main = paste("Распределение оценок для", browser),
       xlab = "Оценка",
       ylab = "Частота",
       col = "pink",
       border = "darkred")
})

# Боксплоты


dev.new(width = 10, height = 7)
par(mar = c(10, 4, 6, 2))

boxplot(chrome_users,
        las = 2,
        col = rainbow(length(chrome_users)),
        main = "Оценки пользователей Chrome",
        ylim = c(0, 10),
        ylab = "Оценки",
        xlab = browser)


boxplot(not_chrome_users,
        las = 2,
        col = rainbow(length(chrome_users)+3),
        main = "Оценки не пользователей Chrome",
        ylim = c(0, 10),
        xlab = browser)


par(mfrow = c(1, 1))

colMeans(chrome_users) %>% sort(decreasing = TRUE)
colMeans(not_chrome_users) %>% sort(decreasing = TRUE)

# 5. Продемонстрировать: слияние таблиц, 
# добавление строк, исключение переменных,
# формирование части из целого набора данных - подмножество (subset),
# умение загрузить данные их внешнего файла.
data1  <- read.csv("опрос.csv")
data2 <- read.csv("доп_опрос.csv")

cat("Cлияние: ")
merged_data <- merge(data1, data2, by = "id", all = TRUE) # full
# merged_data <- merge(data1, data2, by = "id", all.x = TRUE) # left
# merged_data <- merge(data1, data2, by = "id", all.y = TRUE) # right
dim(merged_data)
head(merged_data)


cat("Добавление новых строк")
new_row <- data.frame(
  Google.Chrome = 8,
  Mozilla.Firefox = 7,
  Opera = 6,
  id = "111"
)

missing_cols <- setdiff(names(data), names(new_row))
new_row[missing_cols] <- NA

data_extended <- rbind(data1, new_row)

dim(data_extended)
tail(data_extended)


cat("Отбрасывание столбца: ")
dim(data)
data_without_opera <- data[, !names(data) %in% c("Opera")]
dim(data_without_opera)



cat("Выбор подмножества: ")
firefox_users <- subset(data, Mozilla.Firefox > 5)

dim(firefox_users)
head(firefox_users)

