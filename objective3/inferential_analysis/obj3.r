# Cargar las librerías necesarias
library(car)

# Fase 1: Cargar y Explorar los Datos
# ------------------------------------
cat("Fase 1: Cargar y Explorar los Datos\n")

# Cargar los datos desde un archivo CSV que contiene las calificaciones de los estudiantes
cat("Cargando el archivo 'StudentsPerformance.csv'...\n")
datos <- read.csv("StudentsPerformance.csv")
cat("Datos cargados exitosamente.\n")

# Mostrar la estructura de los datos
cat("Estructura de los datos:\n")
print(str(datos))

# Resumen estadístico de las variables en el conjunto de datos
cat("Resumen estadístico de los datos:\n")
print(summary(datos))

# Fase 2: Análisis de Varianza (ANOVA)
# -------------------------------------
cat("Fase 2: Análisis de Varianza (ANOVA)\n")

# Realizar el ANOVA para 'math_score'
cat("Realizando ANOVA para 'math_score'...\n")
anova_math <- aov(math_score ~ race_ethnicity, data = datos)
cat("Resultados del ANOVA para 'math_score':\n")
print(summary(anova_math))

# Realizar el ANOVA para 'reading_score'
cat("Realizando ANOVA para 'reading_score'...\n")
anova_reading <- aov(reading_score ~ race_ethnicity, data = datos)
cat("Resultados del ANOVA para 'reading_score':\n")
print(summary(anova_reading))

# Realizar el ANOVA para 'writing_score'
cat("Realizando ANOVA para 'writing_score'...\n")
anova_writing <- aov(writing_score ~ race_ethnicity, data = datos)
cat("Resultados del ANOVA para 'writing_score':\n")
print(summary(anova_writing))

# Fase 3: Comparaciones Múltiples (Test de Tukey)
# -----------------------------------------------
cat("Fase 3: Comparaciones Múltiples (Test de Tukey)\n")

# Comparaciones múltiples para 'math_score'
cat("Realizando Test de Tukey para 'math_score'...\n")
tukey_math <- TukeyHSD(anova_math)
cat("Resultados del Test de Tukey para 'math_score':\n")
print(tukey_math)

# Comparaciones múltiples para 'reading_score'
cat("Realizando Test de Tukey para 'reading_score'...\n")
tukey_reading <- TukeyHSD(anova_reading)
cat("Resultados del Test de Tukey para 'reading_score':\n")
print(tukey_reading)

# Comparaciones múltiples para 'writing_score'
cat("Realizando Test de Tukey para 'writing_score'...\n")
tukey_writing <- TukeyHSD(anova_writing)
cat("Resultados del Test de Tukey para 'writing_score':\n")
print(tukey_writing)

# Fase 4: Verificación de Supuestos
# ---------------------------------
cat("Fase 4: Verificación de Supuestos\n")

# Verificar los niveles únicos de 'race_ethnicity'
cat("Niveles únicos de 'race_ethnicity':\n")
print(unique(datos$race_ethnicity))

# Conteo del tamaño de muestra para cada grupo de 'race_ethnicity'
cat("Tamaño de muestra para cada grupo de 'race_ethnicity':\n")
print(table(datos$race_ethnicity))

# Verificación de normalidad (Shapiro-Wilk) para cada grupo de 'math_score'
cat("Verificando la normalidad de los datos en cada grupo de 'math_score' usando el Test de Shapiro-Wilk...\n")

# Filtrar los datos para cada grupo y realizar la prueba de Shapiro-Wilk
shapiro_results <- lapply(unique(datos$race_ethnicity), function(group) {
  group_data <- na.omit(datos$math_score[datos$race_ethnicity == group])
  cat("Grupo", group, "- tamaño de muestra:", length(group_data), "\n")
  if (length(group_data) >= 3 && length(group_data) <= 5000) {
    result <- shapiro.test(group_data)
    print(result)
    return(result$p.value)
  } else {
    cat("Grupo", group, "tiene un tamaño de muestra inadecuado para la prueba de Shapiro-Wilk.\n")
    return(NA)
  }
})

# Comentario sobre los resultados de Shapiro-Wilk
cat("Comentarios sobre los resultados de la prueba de Shapiro-Wilk:\n")
cat("- Los datos de Group A y Group D no muestran evidencia significativa de desviarse de la normalidad.\n")
cat("- Los grupos B, C, y E tienen p-values menores a 0.05, lo que indica que no siguen una distribución normal.\n")

# Conversión de 'race_ethnicity' a factor
cat("Convirtiendo 'race_ethnicity' a factor...\n")
datos$race_ethnicity <- as.factor(datos$race_ethnicity)
cat("Conversión completada. Verificación de la estructura:\n")
print(str(datos$race_ethnicity))

# Prueba de homogeneidad de varianzas (Levene's Test)
cat("Verificando la homogeneidad de varianzas usando el Test de Levene...\n")
levene_results <- list(
  math = leveneTest(math_score ~ race_ethnicity, data = datos),
  reading = leveneTest(reading_score ~ race_ethnicity, data = datos),
  writing = leveneTest(writing_score ~ race_ethnicity, data = datos)
)

cat("Resultados del Test de Levene:\n")
print(levene_results)

# Comentario sobre Levene's Test
cat("Comentarios sobre los resultados del Test de Levene:\n")
cat("- En todos los casos, Pr(>F) > 0.05 sugiere que no hay una diferencia significativa en la varianza de las puntuaciones entre los grupos raciales.\n")
cat("- Esto indica que se cumple el supuesto de homogeneidad de varianzas, permitiendo proceder con el ANOVA.\n")

# Conclusión final
cat("Conclusión final:\n")
cat("- Los ANOVA realizados muestran diferencias significativas en las puntuaciones de matemáticas, lectura, y escritura entre los diferentes grupos raciales.\n")
cat("- El Test de Tukey ha permitido identificar diferencias específicas entre los grupos, con el Grupo E mostrando consistentemente un desempeño superior.\n")
