setwd("~/Code/r/stat_analysis")
library(dplyr)
library(ggplot2)

readfile <- read.csv("опрос.csv", row.names = "id")
data <- data.frame(readfile)

#2
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

mean(data, na.rm=TRUE)

descriptive_results <- sapply(data, descriptive_stats)
cat("Дескриптивный анализ: ")
print(round(descriptive_results, 2))

#3
cat("Сортировка по признаку")
sorted_by_chrome <- data[order(data[["Google.Chrome"]], decreasing = TRUE, na.last = TRUE), ]

sorted_by_chrome


#4
chrome_users <- subset(data, Google.Chrome > 7)
cat("Размерность:", dim(chrome_users), "\n")
cat("Количество:", nrow(chrome_users), "\n")

descriptive_results <- sapply(chrome_users, descriptive_stats)
cat("Дескриптивный анализ: ")
print(round(descriptive_results, 2))


not_chrome_users <- subset(data, Google.Chrome <= 7 | is.na(Google.Chrome))
not_chrome_users


par(mfrow = c(1, 3))

# Гистограммы
hist(chrome_users[["Google.Chrome"]],
     breaks = seq(0, 10, by = 1),
     main = "Распределение оценок для Google Chrome",
     xlab = "Оценка",
     ylab = "Частота",
     col = "lightgreen",
     border = "darkgreen")

hist(chrome_users[["Mozilla.Firefox"]],
     breaks = seq(0, 10, by = 1),
     main = "Распределение оценок для Mozilla Firefox",
     xlab = "Оценка",
     ylab = "Частота",
     col = "lightgreen",
     border = "darkgreen")

hist(chrome_users[["Opera"]],
     breaks = seq(0, 10, by = 1),
     main = "Распределение оценок для Opera",
     xlab = "Оценка",
     ylab = "Частота",
     col = "lightgreen",
     border = "darkgreen")


# Боксплоты

boxplot(chrome_users[["Google.Chrome"]],
        las = 2,
        col = "red",
        ylab = "Оценка",
        ylim = c(0, 10),
        xlab = "Google.Chrome")

boxplot(chrome_users[["Mozilla.Firefox"]],
        las = 2,
        col = "red",
        ylab = "Оценка",
        ylim = c(0, 10),
        xlab = "Mozilla.Firefox")


boxplot(chrome_users[["Opera"]],
        las = 2,
        col = "red",
        ylab = "Оценка",
        ylim = c(0, 10),
        xlab = "Mozilla.Firefox")

par(mfrow = c(1, 1))


#5
data1  <- read.csv("опрос.csv")
data2 <- read.csv("доп_опрос.csv")

cat("Cлияние: ")
# merged_data <- merge(data, data2, by = "id", all = TRUE) # full
merged_data <- merge(data, data2, by = "id", all.x = TRUE) # left
# merged_data <- merge(data, data2, by = "id", all.y = TRUE) # right
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

data_extended <- rbind(data, new_row)

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

