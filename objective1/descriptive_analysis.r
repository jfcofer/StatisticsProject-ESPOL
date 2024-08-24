library(dplyr)
library(ggplot2)
library(ggpubr)


get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Load the CSV file
data <- read.csv("StudentsPerformance.csv")
head(data)

muestra_universidad <- data %>%
  select(parental_level_of_education, promedio) %>%
  filter(parental_level_of_education %in% c("bachelor's degree", "associate's degree", "master's degree"))

size_muestra <- nrow(muestra_universidad)
average <- mean(muestra_universidad$promedio, na.rm = TRUE)
variance <- var(muestra_universidad$promedio, na.rm = TRUE)
range_values <- range(muestra_universidad$promedio, na.rm = TRUE)
range_value <- diff(range_values)
mode_value <- get_mode(muestra_universidad$promedio)
std_dev <- sd(muestra_universidad$promedio, na.rm = TRUE)
median_value <- median(muestra_universidad$promedio, na.rm = TRUE)

# Display the calculated statistics

formatted_size_muestra <- format(size_muestra, nsmall = 0)
formatted_average <- round(average, digits = 2)
formatted_variance <- round(variance, digits = 2)
formatted_range_value <- round(range_value, digits = 2)
formatted_mode_value <- round(mode_value, digits = 2)
formatted_std_dev <- round(std_dev, digits = 2)
formatted_median_value <- round(median_value, digits = 2)


summary_table <- data.frame(
  Statistic = c("Tamaño de Muestra", "Media", "Mediana","Moda","Rango",  "Varianza",  "Desviación Estándar"),
  Value = c(
    formatted_size_muestra, formatted_average, formatted_median_value,formatted_mode_value,
    formatted_range_value, formatted_variance,  formatted_std_dev
  )
)

names(summary_table) <- c("Estadistico", "Valor")


# Create and save the table as an image
table_plot <- ggtexttable(summary_table, rows = NULL, theme = ttheme("mBlue"))
ggsave("objective1/images/muestra_universidad_stats.png", plot = table_plot, width = 6, height = 4)

# Create and save a histogram
histogram_plot <- ggplot(muestra_universidad, aes(x = promedio)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Promedio", x = "Promedio", y = "Frequency")
ggsave("objective1/images/muestra_universidad_histogram_plot.png", plot = histogram_plot, width = 6, height = 4)

# Create and save a boxplot
boxplot_plot <- ggplot(muestra_universidad, aes(y = promedio)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Promedio", y = "Promedio")
ggsave("objective1/images/muestra_universidad_boxplot_plot.png", plot = boxplot_plot, width = 4, height = 6)



muestra_sin_universidad = data %>%
  select(parental_level_of_education, promedio) %>%
  filter(!parental_level_of_education %in% c("bachelor's degree", "associate's degree", "master's degree"))

head(muestra_sin_universidad)

size_muestra <- nrow(muestra_sin_universidad)
average <- mean(muestra_sin_universidad$promedio, na.rm = TRUE)
variance <- var(muestra_sin_universidad$promedio, na.rm = TRUE)
range_values <- range(muestra_sin_universidad$promedio, na.rm = TRUE)
range_value <- diff(range_values)
mode_value <- get_mode(muestra_sin_universidad$promedio)
std_dev <- sd(muestra_sin_universidad$promedio, na.rm = TRUE)
median_value <- median(muestra_sin_universidad$promedio, na.rm = TRUE)

# Display the calculated statistics

formatted_size_muestra <- format(size_muestra, nsmall = 0)
formatted_average <- round(average, digits = 2)
formatted_variance <- round(variance, digits = 2)
formatted_range_value <- round(range_value, digits = 2)
formatted_mode_value <- round(mode_value, digits = 2)
formatted_std_dev <- round(std_dev, digits = 2)
formatted_median_value <- round(median_value, digits = 2)


summary_table <- data.frame(
  Statistic = c("Tamaño de Muestra", "Media", "Mediana","Moda","Rango",  "Varianza",  "Desviación Estándar"),
  Value = c(
    formatted_size_muestra, formatted_average, formatted_median_value,formatted_mode_value,
    formatted_range_value, formatted_variance,  formatted_std_dev
  )
)

names(summary_table) <- c("Estadistico", "Valor")


# Create and save the table as an image
table_plot <- ggtexttable(summary_table, rows = NULL, theme = ttheme("mBlue"))
ggsave("objective1/images/muestra_sin_universidad_stats.png", plot = table_plot, width = 6, height = 4)

# Create and save a histogram
histogram_plot <- ggplot(muestra_sin_universidad, aes(x = promedio)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Promedio", x = "Promedio", y = "Frequency")
ggsave("objective1/images/muestra_sin_universidad_histogram_plot.png", plot = histogram_plot, width = 6, height = 4)

# Create and save a boxplot
boxplot_plot <- ggplot(muestra_sin_universidad, aes(y = promedio)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Promedio", y = "Promedio")
ggsave("objective1/images/muestra_sin_universidad_boxplot_plot.png", plot = boxplot_plot, width = 4, height = 6)
