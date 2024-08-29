# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(readr)
library(nortest)
library(car)

# Fase 1: Cargar y Preparar los Datos
# ------------------------------------
# Cargar el archivo CSV con los datos de rendimiento académico de los estudiantes
data <- read_csv("StudentsPerformance.csv")

# Calcular el promedio de rendimiento académico general (average_score)
data <- data %>%
  mutate(average_score = (math_score + reading_score + writing_score) / 3)

# Dividir los datos en dos grupos según el tipo de almuerzo:
# - standard_lunch: estudiantes con almuerzo estándar
# - free_reduced_lunch: estudiantes con almuerzo gratuito o reducido
standard_lunch <- data %>%
  filter(lunch == "standard") %>%
  pull(average_score)

free_reduced_lunch <- data %>%
  filter(lunch == "free/reduced") %>%
  pull(average_score)

# Fase 2: Verificación de Supuestos
# -----------------------------------
# Verificar la normalidad de los datos en cada grupo usando el Test de Anderson-Darling
ad_test_standard <- ad.test(standard_lunch)
ad_test_free_reduced <- ad.test(free_reduced_lunch)

# Resultados de la prueba de Anderson-Darling:
# --------------------------------------------
# - Grupo standard_lunch: A = 0.51572, p-value = 0.1902
#   Este resultado sugiere que no se rechaza la hipótesis nula de normalidad para este grupo.
# - Grupo free_reduced_lunch: A = 0.46002, p-value = 0.2597
#   Al igual que el grupo anterior, este p-value indica que no se rechaza la hipótesis nula de normalidad.

# Verificar la homogeneidad de varianzas entre los dos grupos usando el Test de Levene
levene_test_result <- leveneTest(average_score ~ lunch, data = data)

# Resultado del Test de Levene:
# -----------------------------
# - F value = 3.5494, p-value = 0.05986
#   Aunque el p-valor es ligeramente superior a 0.05, lo que sugiere que las varianzas entre los grupos son homogéneas,
#   este valor indica que la homogeneidad de varianzas es aceptable para proceder con la prueba t asumiendo varianzas iguales.

# Fase 3: Realizar la Prueba t
# -----------------------------
# Dado que los grupos son grandes y se asume homogeneidad de varianzas, se realiza la prueba t para muestras independientes.
# Esta prueba determinará si el promedio de rendimiento académico difiere significativamente entre los estudiantes con almuerzo estándar
# y aquellos con almuerzo gratuito o reducido.
t_test_result <- t.test(standard_lunch, free_reduced_lunch, var.equal = TRUE, alternative = "greater")

# Resultados de la Prueba t:
# --------------------------
# - t = 9.5751, df = 998, p-value < 2.2e-16
#   Este p-valor extremadamente bajo indica una diferencia significativa en los promedios de rendimiento académico entre los dos grupos.
# - Intervalo de confianza del 95%: [7.152872, Inf]
#   Esto sugiere que el promedio de los estudiantes con almuerzo estándar es al menos 7.15 puntos mayor que el de aquellos con almuerzo gratuito o reducido.
# - Media de standard_lunch: 70.83721
# - Media de free_reduced_lunch: 62.19906
#   Las medias indican que los estudiantes con almuerzo estándar tienen un rendimiento académico promedio mayor.

# Fase 4: Visualización de los Resultados
# ---------------------------------------
# Crear el boxplot para la comparación de los dos grupos
boxplot_lunch <- ggplot(data, aes(x = lunch, y = average_score, fill = lunch)) +
  geom_boxplot(color = "darkblue") +
  labs(title = "Distribución del Rendimiento Académico por Tipo de Almuerzo",
       x = "Tipo de Almuerzo",
       y = "Rendimiento Académico Promedio") +
  scale_fill_manual(values = c("standard" = "lightblue", "free/reduced" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar solo el boxplot en un archivo PNG
ggsave("objective2/inferential_analysis/images/boxplot_lunch.png", boxplot_lunch, width = 10, height = 8)

# Este boxplot muestra que los estudiantes con almuerzo estándar tienden a tener un rendimiento académico
# superior en comparación con aquellos que reciben almuerzo gratuito o reducido, con menos valores atípicos en el grupo de almuerzo estándar.