library(dplyr)
library(ggplot2)
library(ggpubr)


get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

calculate_statistics <- function(score_column) {

  size_muestra <- format(nrow(muestra_universidad), nsmall = 0)
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


make_histogram_plot <- function(data, column, binwidth = 5, fill_color = "blue", 
                                      border_color = "black", title, x_label, y_label = "Frequency", 
                                      file_path, width = 6, height = 4) {
  histogram_plot <- ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = binwidth, fill = fill_color, color = border_color) +
    labs(title = title, x = x_label, y = y_label)
  
  ggsave(file_path, plot = histogram_plot, width = width, height = height)
}

make_box_plot <- function(data, column, fill_color = "blue", 
                          border_color = "black", title = NULL, 
                          y_label = NULL, file_path, 
                          outlier_color = "red", width = 4, height = 6) {
  
  # Check if the column exists in the data
  if (!column %in% names(data)) {
    stop(paste("Column", column, "not found in the data"))
  }
  
  # Set default labels if not provided
  if (is.null(title)) {
    title <- paste("Boxplot of", column)
  }
  if (is.null(y_label)) {
    y_label <- column
  }
  
  # Create the boxplot using ggplot2
  box_plot <- ggplot(data, aes_string(y = column)) +
    geom_boxplot(fill = fill_color, color = border_color, outlier.color = outlier_color) +
    labs(title = title, y = y_label)
  
  # Save the boxplot to the specified file path
  ggsave(file_path, plot = box_plot, width = width, height = height)
}


muestra_universidad <- data %>%
  select(parental_level_of_education, promedio) %>%
  filter(parental_level_of_education %in% c("bachelor's degree", "associate's degree", "master's degree"))


university_stats <- calculate_statistics(muestra_universidad$promedio)

column_names <- c("Tamaño de Muestra", "Media", "Mediana","Moda","Rango",  "Varianza",  "Desviación Estándar")

university_table <- make_table(column_names, university_stats) %>% change_table_names()

make_table_plot(university_table,"objective1/descriptive_analysis/university/images/university_table_plot.png")

make_histogram_plot(
  data = muestra_universidad,
  column = "promedio",
  binwidth = 5,
  fill_color = "blue",
  border_color = "black",
  title = "Histogram of Promedio",
  x_label = "Promedio",
  file_path = "objective1/descriptive_analysis/university/images/university_hist_plot.png"
)

make_box_plot(
  data = muestra_universidad,
  column = "promedio",
  fill_color = "blue",
  border_color = "black",
  title = "Boxplot of Promedios",
  y_label = "Promedio",
  file_path = "objective1/descriptive_analysis/university/images/university_box_plot.png",
  outlier_color = "red",
  width = 4,
  height = 6  
)