library(dplyr)
library(ggplot2)

data <- read.csv("StudentsPerformance.csv")
head(data)

data$promedio <- rowMeans(data[, c("math_score", "reading_score", "writing_score")], na.rm = FALSE)
head(data)

write.csv(data, "StudentsPerformance.csv", row.names = FALSE)
