library(fitdistrplus)
library(ggplot2)
library(dplyr)
library(car)

# Fase 1: Cargar y Preparar los Datos
# ------------------------------------
cat("Fase 1: Cargar y Preparar los Datos\n")

# Cargar los datos desde un archivo CSV que contiene información sobre el rendimiento académico de los estudiantes
cat("Cargando los datos desde el archivo 'StudentsPerformance.csv'...\n")
data <- read.csv("StudentsPerformance.csv")
cat("Datos cargados exitosamente.\n")

# Mostrar las primeras filas del dataset
cat("Mostrando las primeras filas del dataset:\n")
print(head(data))

# Agrupar el nivel educativo de los padres en dos categorías:
# 1. "Titulo Universitario" para aquellos con título universitario (associate's, bachelor's, master's degree)
# 2. "No tiene Titulo Universitario" para aquellos que no tienen título universitario (high school, some high school, some college)
cat("Agrupando el nivel educativo de los padres en dos categorías...\n")
data$educacion_padres <- ifelse(data$parental_level_of_education %in% c("associate's degree", 
                                                                        "bachelor's degree", 
                                                                        "master's degree"), 
                                "Titulo Universitario", "No tiene Titulo Universitario")
cat("Agrupación completada.\n")

# Mostrar el resumen de la nueva columna
cat("Resumen de la nueva columna 'educacion_padres':\n")
print(table(data$educacion_padres))

# Subconjunto de los datos por nivel educativo de los padres:
# - group1: estudiantes cuyos padres tienen un título universitario
# - group2: estudiantes cuyos padres no tienen un título universitario
cat("Creando subconjuntos de datos según el nivel educativo de los padres...\n")
group1 <- data %>%
  filter(educacion_padres == "Titulo Universitario") %>%
  .$promedio  # Acceso directo a la columna 'promedio'

group2 <- data %>%
  filter(educacion_padres == "No tiene Titulo Universitario") %>%
  .$promedio
cat("Subconjuntos creados:\n")
cat("Tamaño del grupo 1:", length(group1), "\n")
cat("Tamaño del grupo 2:", length(group2), "\n")

# Fase 2: Verificación de Supuestos
# -----------------------------------
cat("Fase 2: Verificación de Supuestos\n")

# Verificar la normalidad de los datos en cada grupo usando el Test de Shapiro-Wilk
cat("Verificando la normalidad de los datos en cada grupo...\n")
shapiro_test_group1 <- shapiro.test(group1)
shapiro_test_group2 <- shapiro.test(group2)
cat("Resultados del Test de Shapiro-Wilk:\n")
cat("Grupo 1:\n")
print(shapiro_test_group1)
cat("Grupo 2:\n")
print(shapiro_test_group2)

# Verificar la homogeneidad de varianzas entre los dos grupos usando el Test de Levene
cat("Verificando la homogeneidad de varianzas entre los grupos...\n")
levene_test_result <- leveneTest(promedio ~ educacion_padres, data = data)
cat("Resultado del Test de Levene:\n")
print(levene_test_result)

# Fase 3: Realizar la Prueba t
# -----------------------------
cat("Fase 3: Realizar la Prueba t\n")

# Realizar una prueba t para muestras independientes
t_test_result <- t.test(group1, group2, var.equal = TRUE, alternative = "greater")
cat("Resultado de la Prueba t:\n")
print(t_test_result)

# Fase 4: Visualización
# -----------------------
cat("Fase 4: Visualización\n")

# Crear el boxplot para la comparación de los dos grupos
cat("Creando un boxplot para la comparación de los grupos...\n")
boxplot_edu <- ggplot(data, aes(x = educacion_padres, y = promedio, fill = educacion_padres)) +
  geom_boxplot(color = "darkblue") +
  labs(title = "Rendimiento Académico por Nivel Educativo de los Padres",
       x = "Nivel Educativo de los Padres",
       y = "Promedio de Rendimiento Académico") +
  scale_fill_manual(values = c("Titulo Universitario" = "lightblue", "No tiene Titulo Universitario" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar el boxplot
cat("Mostrando el boxplot...\n")
print(boxplot_edu)

# Guardar el boxplot en un archivo PNG
cat("Guardando el boxplot en un archivo PNG...\n")
ggsave("objective1/inferential_analysis/images/boxplot_educacion_padres.png", boxplot_edu, width = 10, height = 8)
cat("Boxplot guardado exitosamente.\n")