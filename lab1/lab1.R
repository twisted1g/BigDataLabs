#1
q <- c(7, 6, 5, 4)
cat("Вектор q: ", q)

g <- 0:3
cat("Вектор g: ", g)

cat("Вектор q - g: ", q - g)

cat("Вектор q + g: ", q + g)

cat("Вектор q * g: ", q * g)

cat("Вектор q / g: ", q / g)

cat("Вектор q ^ g: ", q ^ g)

#2
vec1 <- (1:20)
vec1[seq(1, 20, 2)] <- 0
cat(vec1)

cat(c(rbind(0, seq(2, 20, 2))))

cat("Степени 2: ", 2^(0:10))

cat("Степени 10: ", 10^(0:4))

#3
n <- 1:50
ryd = 1 / (n + (n+1))
sum(ryd)

sum(1/2^(0:20))

n <- 0:9
ryd <- seq(1, 28, 3)/3^n
ryd
sum(ryd)

ryd[ryd > 0.5]
#4

vec <- seq(3, 27, 3)
vec
vec[c(2, 5, 7)]
vec[length(vec) -1]
vec[-(length(vec) -1)]
vec[-6]
vec[1]
vec[100]
vec[-c(1, (length(vec) -1))]

vec[vec > 4 & vec < 10]
vec[vec > 4 | vec < 10]



# 12. В векторе случайных целых чисел в диапазоне [-20; 50] поменять местами минимальный и максимальный элементы.
# Циклы не использовать.
# Подсчитать количество отрицатльных чисел, заменить их нулями.

vec <- sample(-20:50, size = 10, replace = TRUE)

print(vec)

min_index <- which.min(vec)
max_index <- which.max(vec)

vec[c(min_index, max_index)] <- vec[c(max_index, min_index)]

cat("\nВектор после замены минимального и максимального элементов:\n")
print(vec)

cat("\nКоличество отрицательных чисел:", sum(vec < 0), "\n")

vec[vec < 0] <- 0

cat("\nИтоговый вектор:", vec)

print(c("\nКоличество отрицательных чисел после замены:", sum(vec < 0)))


# 27. Создать df, содержащий себестоимость добычи нефти по странам мира в 2021 году.
# Отдельно создайте создай столбец с указанием континента и добавьте его к готовому df.
# Отсортируйте его по возрастанию стоимости.
# Выберите в новый df только страны Европы, в дргую выборку – страны Америки. 

oil_cost_df <- data.frame(
  country = c("США", "Норвегия", "Канада", "Россия", "Нигерия"),
  cost_per_barrel = c(20, 17, 18, 16, 11),
  stringsAsFactors = FALSE
)

oil_cost_df

oil_cost_df["Contenent"] <- c ("Америка", "Европа", "Америка", "Евразия", "Африка")

oil_cost_sorted <- oil_cost_df[order(oil_cost_df[["cost_per_barrel"]]), ]

europe_df <- subset(oil_cost_sorted, Contenent == "Европа")
europe_df

america_df <- subset(oil_cost_sorted, Contenent == "Америка")
america_df
              
