setwd("~/Code/r/BigDataLabs/lab2")

library(dplyr)

cat("Чтение данных")
readfile <- read.csv("опрос.csv", row.names = "id")

data = data.frame(readfile)

str(data)
summary(data)


# 1. вычислить max, min, mean по каждому столбцу
base_stat <- sapply(data, function(x){ c(max=max(x, na.rm=TRUE), 
                           min=min(x, na.rm=TRUE), 
                           mean=mean(x, na.rm=TRUE))
})

cat("Вывод базовой информации:")
base_stat

# 2. подсчитать количество людей, отдавших предпочтение выбранному 
# элементу >0.7 и <0.3 (составить вектор) 
cat("Подсчет голосов > 7 за Chrome: ", sum(data[["Google.Chrome"]] > 7, na.rm=TRUE))

cat("Подсчет голосов < 3 за Chrome: ", sum(data[["Google.Chrome"]] < 3, na.rm=TRUE))

# 3. вывести рейтинг фильмов (книг...)  в списке по убыванию 
cat("Сортировка среднего рейтинга: ")
sort(sapply(data, function(x) mean(x, na.rm=TRUE)), decreasing = TRUE )

# 4. поработать с пропущенными данными: 
# отработать оба варианта, продемонстрировать результаты.
cat("Замена na на 0: ")
data[is.na(data)] <- 0
data

#cat("Удаление строк с na: ")
#new_data <- na.omit(data)
#new_data

# 5. Продемонстрировать выбор строк из таблицы по указанному признаку.
cat("Выборка строк с оценкой >  5 Chrome: ")
chrome_users <- data[data[["Google.Chrome"]] > 7,]

sapply(data, function(x) mean=mean(x, na.rm=TRUE)) - 
  sapply(chrome_users, function(x) mean=mean(x, na.rm=TRUE)) 


# 6. Построить столбчатую диаграмму оценок (можно сделать разными способами),
par(mfrow = c(1, 2))

# Столбчатая диаграмма средних значений
means <- colMeans(data, na.rm = TRUE)
means_sorted <- sort(means)

barplot(means_sorted,
        las = 2,
        col = "grey",
        main = "Средние оценки",
        ylab = "Средняя оценка",
        cex.names = 0.7)

# Гистограмма
hist(data[["Google.Chrome"]],
     breaks = seq(0, 10, by = 1),
     main = "Распределение оценок для Google Chrome",
     xlab = "Оценка",
     ylab = "Частота",
     col = "lightblue",
     border = "darkgreen")


par(mfrow = c(1, 1))

