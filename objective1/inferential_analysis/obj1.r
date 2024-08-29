library(fitdistrplus)
library(ggplot2)
library(dplyr)
library(car)

# Fase 1: Cargar y Preparar los Datos
# ------------------------------------
# Cargar los datos desde un archivo CSV que contiene información sobre el rendimiento académico de los estudiantes
data <- read.csv("StudentsPerformance.csv")

# Agrupar el nivel educativo de los padres en dos categorías:
# 1. "Titulo Universitario" para aquellos con título universitario (associate's, bachelor's, master's degree)
# 2. "No tiene Titulo Universitario" para aquellos que no tienen título universitario (high school, some high school, some college)
data$educacion_padres <- ifelse(data$parental_level_of_education %in% c("associate's degree", 
                                                                        "bachelor's degree", 
                                                                        "master's degree"), 
                                "Titulo Universitario", "No tiene Titulo Universitario")

# Subconjunto de los datos por nivel educativo de los padres:
# - group1: estudiantes cuyos padres tienen un título universitario
# - group2: estudiantes cuyos padres no tienen un título universitario
group1 <- data %>%
  filter(educacion_padres == "Titulo Universitario") %>%
  .$promedio  # Acceso directo a la columna 'promedio'

group2 <- data %>%
  filter(educacion_padres == "No tiene Titulo Universitario") %>%
  .$promedio

# Fase 2: Verificación de Supuestos
# -----------------------------------
# Antes de realizar la prueba t, es importante verificar si los datos cumplen con los supuestos necesarios.

# Verificar la normalidad de los datos en cada grupo usando el Test de Shapiro-Wilk
# Dado el gran tamaño de las muestras (>300), la prueba t es robusta frente a pequeñas desviaciones de la normalidad,
# pero aún así realizamos estas pruebas para confirmar si existen grandes desviaciones.
shapiro_test_group1 <- shapiro.test(group1)
shapiro_test_group2 <- shapiro.test(group2)

# Resultados:
# - Ambos grupos presentan p-valores menores a 0.05, indicando que no siguen una distribución normal perfecta.
# - Sin embargo, debido al tamaño grande de las muestras, estas pequeñas desviaciones no invalidan la prueba t.

# Verificar la homogeneidad de varianzas entre los dos grupos usando el Test de Levene
# La prueba de Levene nos ayuda a verificar si las varianzas de los grupos son similares, 
# lo cual es un supuesto clave para la prueba t con varianzas iguales.
levene_test_result <- leveneTest(promedio ~ educacion_padres, data = data)

# Resultado:
# - El p-valor obtenido es 0.9072, lo que sugiere que no se rechaza la hipótesis nula de homogeneidad de varianzas.
# - Esto significa que las varianzas entre los dos grupos son prácticamente iguales, 
#   permitiéndonos usar la prueba t con la opción `var.equal = TRUE`.

# Fase 3: Realizar la Prueba t
# -----------------------------
# Ahora, realizamos una prueba t para muestras independientes para comparar los promedios de los dos grupos.
# Dado que estamos interesados en probar si el promedio del grupo 1 es mayor que el del grupo 2, 
# especificamos `alternative = "greater"`.

t_test_result <- t.test(group1, group2, var.equal = TRUE, alternative = "greater")

# Mostrar los resultados de la prueba t
print(t_test_result)

# Resultado de la Prueba t:
# - t = 5.672, p-value = 9.242e-09
# - El p-valor es extremadamente bajo, lo que indica una diferencia significativa entre los promedios de los grupos.
# - El intervalo de confianza (3.65, Inf) sugiere que el promedio del grupo con padres de educación universitaria es al menos 3.65 puntos mayor.
# - Conclusión: Los estudiantes cuyos padres tienen un título universitario tienen un rendimiento académico significativamente mayor que aquellos cuyos padres no lo tienen.


# Fase 4: Visualización
# -----------------------
# Crear el boxplot para la comparación de los dos grupos
boxplot_edu <- ggplot(data, aes(x = educacion_padres, y = promedio, fill = educacion_padres)) +
  geom_boxplot(color = "darkblue") +
  labs(title = "Rendimiento Académico por Nivel Educativo de los Padres",
       x = "Nivel Educativo de los Padres",
       y = "Promedio de Rendimiento Académico") +
  scale_fill_manual(values = c("Titulo Universitario" = "lightblue", "No tiene Titulo Universitario" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar solo el boxplot en un archivo PNG
ggsave("objective1/inferential_analysis/images/boxplot_educacion_padres.png", boxplot_edu, width = 10, height = 8)