library(xml2)
library(dplyr)
library(ggplot2)
library(stringr)

# Лабораторная работа 4, задание 1
# Сбор исторических данных Numbeo по уровню жизни стран мира за 2014- текущий год

base_url <- "https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=%s"
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- 2014:current_year

# По варианту нужно взять 5 стран.
# Если у вас другой набор из списка, замените этот вектор.
selected_countries <- c("Australia", "Japan", "China", "India", "Indonesia")

indicator_names <- c(
  "quality_of_life",
  "purchasing_power",
  "safety",
  "health_care",
  "cost_of_living",
  "property_price_to_income",
  "traffic_commute_time",
  "pollution",
  "climate"
)

indicator_labels <- c(
  quality_of_life = "Quality of Life Index",
  purchasing_power = "Purchasing Power Index",
  safety = "Safety Index",
  health_care = "Health Care Index",
  cost_of_living = "Cost of Living Index",
  property_price_to_income = "Property Price to Income Ratio",
  traffic_commute_time = "Traffic Commute Time Index",
  pollution = "Pollution Index",
  climate = "Climate Index"
)

parse_numbeo_page <- function(year) {
  page_url <- sprintf(base_url, year)

  doc <- tryCatch(
    read_html(page_url),
    error = function(e) {
      message(sprintf("Не удалось загрузить страницу за %s год: %s", year, e$message))
      return(NULL)
    }
  )

  if (is.null(doc)) {
    return(NULL)
  }

  tables <- xml_find_all(doc, ".//table")
  if (length(tables) == 0) {
    message(sprintf("На странице за %s год не найдена таблица.", year))
    return(NULL)
  }

  # Ищем таблицу, где есть нужные заголовки.
  scores <- vapply(tables, function(tbl) {
    headers <- str_squish(xml_text(xml_find_all(tbl, ".//th")))
    sum(c("Country", "Quality of Life Index", "Purchasing Power Index") %in% headers)
  }, numeric(1))

  main_table <- tables[[which.max(scores)]]
  rows <- xml_find_all(main_table, ".//tr[td]")

  row_values <- lapply(rows, function(row) {
    str_squish(xml_text(xml_find_all(row, ".//td")))
  })

  row_values <- Filter(Negate(is.null), row_values)
  if (length(row_values) == 0) {
    message(sprintf("Для %s года не удалось извлечь строки таблицы.", year))
    return(NULL)
  }

  max_len <- max(lengths(row_values))
  if (max_len < 10 || max_len > 11) {
    message(sprintf(
      "Для %s года найдено неожиданное число столбцов: %s.",
      year,
      max_len
    ))
    return(NULL)
  }

  row_values <- lapply(row_values, function(x) {
    if (length(x) == 10) {
      c(NA_character_, x)
    } else {
      x[seq_len(11)]
    }
  })

  mat <- do.call(rbind, row_values)
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- c("rank_html", "country", indicator_names)

  df <- df %>%
    mutate(
      across(all_of(indicator_names), as.numeric),
      year = year
    ) %>%
    arrange(desc(quality_of_life), country) %>%
    mutate(rank = row_number()) %>%
    select(year, rank, country, all_of(indicator_names))

  df
}

message("Сбор данных Numbeo...")
all_years_data <- bind_rows(lapply(years, parse_numbeo_page))

if (nrow(all_years_data) == 0) {
  stop("Не удалось собрать данные ни за один год.")
}

available_years <- sort(unique(all_years_data$year))
message(sprintf(
  "Загружены данные за годы: %s",
  paste(available_years, collapse = ", ")
))

if (max(available_years) < current_year) {
  message(sprintf(
    "Внимание: данных за %s год нет, последняя доступная версия — %s.",
    current_year,
    max(available_years)
  ))
}

# Сохраняем собранные данные для дальнейшей работы
write.csv(all_years_data, "numbeo_quality_of_life_2014_current.csv", row.names = FALSE)

selected_data <- all_years_data %>%
  filter(country %in% selected_countries) %>%
  arrange(country, year)

if (nrow(selected_data) == 0) {
  stop("Выбранные страны не найдены в загруженных данных.")
}

missing_countries <- setdiff(selected_countries, unique(selected_data$country))
if (length(missing_countries) > 0) {
  message(
    "Некоторые страны не найдены хотя бы в одном году: ",
    paste(missing_countries, collapse = ", ")
  )
}

# Итоговая таблица по 5 странам
selected_data

# Сводка по изменению рейтинга и качества жизни с первого до последнего доступного года
rank_summary <- selected_data %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  summarise(
    first_year = first(year),
    last_year = last(year),
    first_rank = first(rank),
    last_rank = last(rank),
    rank_change = first_rank - last_rank,
    first_qol = first(quality_of_life),
    last_qol = last(quality_of_life),
    qol_change = last_qol - first_qol,
    .groups = "drop"
  ) %>%
  arrange(first_rank)

rank_summary

# 1) График рейтинга всех 5 стран на одной плоскости.
# Чем ниже линия, тем выше место в рейтинге.
plot_rank <- ggplot(selected_data, aes(x = year, y = rank, color = country, group = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_reverse() +
  labs(
    title = "Динамика рейтинга качества жизни",
    subtitle = paste0("Numbeo, 2014 - ", max(available_years)),
    x = "Год",
    y = "Место в рейтинге",
    color = "Страна"
  ) +
  theme_minimal(base_size = 12)

print(plot_rank)

# 2) График общего индекса качества жизни для тех же 5 стран.
plot_qol <- ggplot(selected_data, aes(x = year, y = quality_of_life, color = country, group = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Динамика Quality of Life Index",
    subtitle = paste0("Numbeo, 2014 - ", max(available_years)),
    x = "Год",
    y = "Индекс",
    color = "Страна"
  ) +
  theme_minimal(base_size = 12)

print(plot_qol)

# 3) Наилучшая, на мой взгляд, визуализация для всех показателей:
# фасетный график с отдельной шкалой для каждого индикатора.
indicator_long <- do.call(
  rbind,
  lapply(indicator_names, function(ind_name) {
    data.frame(
      year = selected_data$year,
      country = selected_data$country,
      indicator = indicator_labels[[ind_name]],
      value = selected_data[[ind_name]],
      stringsAsFactors = FALSE
    )
  })
)

indicator_long$indicator <- factor(indicator_long$indicator, levels = unname(indicator_labels))

plot_indicators <- ggplot(
  indicator_long,
  aes(x = year, y = value, color = country, group = country)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
  labs(
    title = "Изменение всех показателей уровня жизни по странам",
    subtitle = "Показатели Numbeo для выбранных стран",
    x = "Год",
    y = "Значение показателя",
    color = "Страна"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(plot_indicators)

# 4) Таблица с изменением каждого показателя между первым и последним годом.
indicator_change <- selected_data %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  summarise(
    across(
      all_of(indicator_names),
      list(first = first, last = last, change = ~ last(.x) - first(.x))
    ),
    .groups = "drop"
  )

indicator_change
