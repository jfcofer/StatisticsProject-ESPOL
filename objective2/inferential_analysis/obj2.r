# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(readr)
library(nortest)
library(car)

# Fase 1: Cargar y Preparar los Datos
# ------------------------------------
cat("Fase 1: Cargar y Preparar los Datos\n")

# Cargar el archivo CSV con los datos de rendimiento académico de los estudiantes
cat("Cargando el archivo 'StudentsPerformance.csv'...\n")
data <- read_csv("StudentsPerformance.csv")
cat("Datos cargados exitosamente.\n")

# Mostrar las primeras filas del dataset
cat("Mostrando las primeras filas del dataset:\n")
print(head(data))

# Calcular el promedio de rendimiento académico general (average_score)
cat("Calculando el promedio de rendimiento académico general (average_score)...\n")
data <- data %>%
  mutate(average_score = (math_score + reading_score + writing_score) / 3)
cat("Promedio calculado y añadido como nueva columna 'average_score'.\n")

# Mostrar las primeras filas del dataset con la nueva columna
cat("Mostrando las primeras filas del dataset con la nueva columna 'average_score':\n")
print(head(data))

# Dividir los datos en dos grupos según el tipo de almuerzo:
cat("Dividiendo los datos en dos grupos según el tipo de almuerzo...\n")
# - standard_lunch: estudiantes con almuerzo estándar
# - free_reduced_lunch: estudiantes con almuerzo gratuito o reducido
standard_lunch <- data %>%
  filter(lunch == "standard") %>%
  pull(average_score)

free_reduced_lunch <- data %>%
  filter(lunch == "free/reduced") %>%
  pull(average_score)

cat("Datos divididos en dos grupos:\n")
cat("Tamaño del grupo standard_lunch:", length(standard_lunch), "\n")
cat("Tamaño del grupo free_reduced_lunch:", length(free_reduced_lunch), "\n")

# Fase 2: Verificación de Supuestos
# -----------------------------------
cat("Fase 2: Verificación de Supuestos\n")

# Verificar la normalidad de los datos en cada grupo usando el Test de Anderson-Darling
cat("Verificando la normalidad de los datos en cada grupo usando el Test de Anderson-Darling...\n")
ad_test_standard <- ad.test(standard_lunch)
ad_test_free_reduced <- ad.test(free_reduced_lunch)

cat("Resultados del Test de Anderson-Darling:\n")
cat("Grupo standard_lunch:\n")
print(ad_test_standard)
cat("Grupo free_reduced_lunch:\n")
print(ad_test_free_reduced)

# Verificar la homogeneidad de varianzas entre los dos grupos usando el Test de Levene
cat("Verificando la homogeneidad de varianzas entre los dos grupos usando el Test de Levene...\n")
levene_test_result <- leveneTest(average_score ~ lunch, data = data)
cat("Resultado del Test de Levene:\n")
print(levene_test_result)

# Fase 3: Realizar la Prueba t
# -----------------------------
cat("Fase 3: Realizar la Prueba t\n")

# Realizar la prueba t para muestras independientes
cat("Realizando la prueba t para muestras independientes...\n")
t_test_result <- t.test(standard_lunch, free_reduced_lunch, var.equal = TRUE, alternative = "greater")
cat("Resultado de la Prueba t:\n")
print(t_test_result)

# Fase 4: Visualización de los Resultados
# ---------------------------------------
cat("Fase 4: Visualización de los Resultados\n")

# Crear el boxplot para la comparación de los dos grupos
cat("Creando un boxplot para la comparación de los grupos...\n")
boxplot_lunch <- ggplot(data, aes(x = lunch, y = average_score, fill = lunch)) +
  geom_boxplot(color = "darkblue") +
  labs(title = "Distribución del Rendimiento Académico por Tipo de Almuerzo",
       x = "Tipo de Almuerzo",
       y = "Rendimiento Académico Promedio") +
  scale_fill_manual(values = c("standard" = "lightblue", "free/reduced" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar el boxplot
cat("Mostrando el boxplot...\n")
print(boxplot_lunch)

# Guardar el boxplot en un archivo PNG
cat("Guardando el boxplot en un archivo PNG...\n")
ggsave("objective2/inferential_analysis/images/boxplot_lunch.png", boxplot_lunch, width = 10, height = 8)
cat("Boxplot guardado exitosamente.\n")
