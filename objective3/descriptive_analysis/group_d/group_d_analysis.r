library(dplyr)
library(ggplot2)
library(ggpubr)


get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

calculate_statistics <- function(score_column) {

  size_muestra <- format(nrow(muestra_grupo_d), nsmall = 0)
  average <- round(mean(score_column, na.rm = TRUE), 2)
  variance <- round(var(score_column, na.rm = TRUE), 2)
  range_values <- range(score_column, na.rm = TRUE)
  range_value <- round(diff(range_values), 2)
  mode_value <- round(get_mode(score_column), 2)
  std_dev <- round(sd(score_column, na.rm = TRUE), 2)
  median_value <- round(median(score_column, na.rm = TRUE), 2)
  
  return(c(size_muestra, average, median_value,  mode_value, range_value,variance, std_dev))
}

make_table <- function(column_names, stats_columns) {
  return (data.frame(
  Statistic = column_names,
  Stats_Column = stats_columns
))
}

change_table_names <- function(table) {
  names(table) <- c("Estadistico", "Valor")
  return(table)
}

make_table_plot<-function(table, table_path){
table_plot <- ggtexttable(table, rows = NULL, theme = ttheme("mBlue"))
ggsave(table_path, plot = table_plot, width = 6, height = 4)  
}


make_histogram_plot <- function(data, score_column, binwidth = 5, fill_color = "blue", 
                                      border_color = "black", title, x_label, y_label = "Frequency", 
                                      file_path, width = 6, height = 4) {
  histogram_plot <- ggplot(data, aes_string(x = score_column)) +
    geom_histogram(binwidth = binwidth, fill = fill_color, color = border_color) +
    labs(title = title, x = x_label, y = y_label)
  
  ggsave(file_path, plot = histogram_plot, width = width, height = height)
}

make_box_plot <- function(data, score_column, fill_color = "blue", 
                          border_color = "black", title = NULL, 
                          y_label = NULL, file_path, 
                          outlier_color = "red", width = 4, height = 6) {
  
  # Check if the score_column exists in the data
  if (!score_column %in% names(data)) {
    stop(paste("Column", score_column, "not found in the data"))
  }
  
  # Set default labels if not provided
  if (is.null(title)) {
    title <- paste("Boxplot of", score_column)
  }
  if (is.null(y_label)) {
    y_label <- score_column
  }
  
  # Create the boxplot using ggplot2
  box_plot <- ggplot(data, aes_string(y = score_column)) +
    geom_boxplot(fill = fill_color, color = border_color, outlier.color = outlier_color) +
    labs(title = title, y = y_label)
  
  # Save the boxplot to the specified file path
  ggsave(file_path, plot = box_plot, width = width, height = height)
}


# Load the CSV file
data <- read.csv("StudentsPerformance.csv")
head(data)

muestra_grupo_d <- data %>%
  select(race_ethnicity, math_score, reading_score, writing_score) %>%
  filter(race_ethnicity == "group D")


math_stats <- calculate_statistics(muestra_grupo_d$math_score)
reading_stats <- calculate_statistics(muestra_grupo_d$reading_score)
writing_stats <- calculate_statistics(muestra_grupo_d$writing_score)

column_names <- c("Tamaño de Muestra", "Media", "Mediana","Moda","Rango",  "Varianza",  "Desviación Estándar")

math_table <- make_table(column_names, math_stats) %>% change_table_names()
writing_table <- make_table(column_names, writing_stats)%>% change_table_names()
reading_table <- make_table(column_names, reading_stats)%>% change_table_names()



make_table_plot(math_table,"objective3/descriptive_analysis/group_d/images/math_table_plot.png")
make_table_plot(writing_table,"objective3/descriptive_analysis/group_d/images/writing_table_plot.png")
make_table_plot(reading_table,"objective3/descriptive_analysis/group_d/images/reading_table_plot.png")



make_histogram_plot(
  data = muestra_grupo_d,
  score_column = "math_score",
  binwidth = 5,
  fill_color = "blue",
  border_color = "black",
  title = "",
  x_label = "Math Score",
  file_path = "objective3/descriptive_analysis/group_d/images/math_hist_plot.png"
)

make_histogram_plot(
  data = muestra_grupo_d,
  score_column = "writing_score",
  binwidth = 5,
  fill_color = "blue",
  border_color = "black",
  title = "",
  x_label = "Writing Score",
  file_path = "objective3/descriptive_analysis/group_d/images/writing_hist_plot.png"
)

make_histogram_plot(
  data = muestra_grupo_d,
  score_column = "reading_score",
  binwidth = 5,
  fill_color = "blue",
  border_color = "black",
  title = "",
  x_label = "Reading Score",
  file_path = "objective3/descriptive_analysis/group_d/images/reading_hist_plot.png"
)


make_box_plot(
  data = muestra_grupo_d,
  score_column = "math_score",
  fill_color = "blue",
  border_color = "black",
  title = "",
  y_label = "Math Score",
  file_path = "objective3/descriptive_analysis/group_d/images/math_box_plot.png",
  outlier_color = "red",
  width = 4,
  height = 6  
)
make_box_plot(
  data = muestra_grupo_d,
  score_column = "writing_score",
  fill_color = "blue",
  border_color = "black",
  title = "",
  y_label = "Writing Score",
  file_path = "objective3/descriptive_analysis/group_d/images/writing_box_plot.png",
  outlier_color = "red",
  width = 4,
  height = 6  
)
make_box_plot(
  data = muestra_grupo_d,
  score_column = "reading_score",
  fill_color = "blue",
  border_color = "black",
  title = "",
  y_label = "Reading Score",
  file_path = "objective3/descriptive_analysis/group_d/images/reading_box_plot.png",
  outlier_color = "red",
  width = 4,
  height = 6  
)

