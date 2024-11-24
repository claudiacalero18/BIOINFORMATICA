# 1. Carga los datos y examínalos en R. Emplea las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay?¿Cuántos tratamientos? (0.5 pto)

# Crear los datos en un dataframe (he tenido que crear un dataframe porque tenia problemas para cargar los datos, creo que es un problema del mac, no habia manera y tenia que avanzar y encontre esta manera, espero que sirva)
datos <- data.frame(
  Tratamiento = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                  4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
                  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 
                  6, 6, 6, 6, 6),
  Wildtype = c(2.2, 2.3, 2.4, 4.5, 5.4, 5.4, 5.4, 4.0, 4.0, 4.4,
               0.0, 0.0, 4.0, 5.0, 6.0, 8.0, 9.0, 8.0, 0.0, 9.0, 
               9.0, 8.0, 7.8, 8.9, 12.0, 2.0, 12.0, 11.0, 14.0, 3.0,
               45.0, 54.0, 46.0, 56.0, 76.0, 54.0, 54.0, 34.0, 45.0, 44.0,
               22.0, 33.0, 33.0, 33.0, 33.0, 33.0, 32.0, 32.0, 43.0, 55.0,
               66.0, 70.0, 40.5, 55.0, 33.0),
  Sequia = c(0.2, 0.3, 0.4, 0.5, 0.2, 0.4, 0.4, 1.0, 0.9, 0.8,
             0.9, 0.8, 0.9, 1.2, 1.3, 1.3, 1.2, 1.4, 2.0, 2.0,
             3.2, 3.4, 4.5, 5.6, 6.5, 5.4, 4.5, 6.4, 6.7, 6.9,
             9.0, 9.8, 9.8, 9.8, 9.9, 9.0, 7.0, 8.0, 9.0, 6.0,
             6.8, 9.0, 9.0, 9.0, 9.89, 9.90, 6.90, 9.90, 9.99, 9.80,
             70.0, 85.0, 30.0, 70.0, 45.0),
  ExcesoRiego = c(6.0, 6.2, 6.4, 6.5, 5.8, 6.1, 6.4, 5.6, 5.4, 5.3,
                  2.1, 2.3, 2.4, 2.5, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5,
                  4.0, 5.5, 3.8, 4.2, 5.9, 8.4, 8.6, 8.7, 8.8, 8.9,
                  16.0, 12.3, 12.4, 15.9, 15.7, 19.0, 20.5, 18.5, 17.3, 20.3,
                  12.1, 12.0, 10.5, 11.3, 12.4, 13.3, 11.1, 15.5, 16.6, 17.7,
                  21.4, 28.9, 31.3, 35.8, 38.8)
)

# Muestra las primeras filas de los datos
head(datos)

# Resumen estadístico de los datos
summary(datos)

# Dimensiones del dataframe (filas y columnas)
dim(datos) #Aqui observamos que hay 4 variables y 55 tratamientos

# Estructura del dataframe
str(datos)

#2. Haz un boxplot para nuestros datos. Uno para cada condición. Elige un color para cada condición y guárdalo para lassiguientes gráficas. 
colores <- c("Wildtype" = "skyblue", "Sequia" = "orange", "ExcesoRiego" = "green")

boxplot(datos$Wildtype,
        main = "Boxplot para Wildtype",
        ylab = "Valores",
        col = colores["Wildtype"])

boxplot(datos$Sequia,
        main = "Boxplot para Sequia",
        ylab = "Valores",
        col = colores["Sequia"])

boxplot(datos$ExcesoRiego,
        main = "Boxplot para ExcesoRiego",
        ylab = "Valores",
        col = colores["ExcesoRiego"])

#3.Haz dos gráficos de dispersión. El primero debe comparar Sequía con Wildtype, y el segundo ExcesoRiego con Wildtype. Cada tratamiento debe de ir de un color distinto. Pista: usa col=datos$Tratamiento. 
#4. Ponle leyenda al gráfico del apartado anterior. En el margen inferior derecho. Pista: investiga sobre legend(). 
                                                                                                                     
# Convertir Tratamiento a factor 
datos$Tratamiento <- as.factor(datos$Tratamiento)

# Sequía vs Wildtype
plot(datos$Wildtype, datos$Sequia, 
     col = datos$Tratamiento,  
     pch = 16,                 
     xlab = "Wildtype", 
     ylab = "Sequía", 
     main = "Dispersión: Sequía vs Wildtype")

# Leyenda
legend("bottomright", 
       legend = levels(datos$Tratamiento),  
       col = 1:length(levels(datos$Tratamiento)),  
       pch = 16,                             
       title = "Tratamiento")                

# ExcesoRiego vs Wildtype
plot(datos$Wildtype, datos$ExcesoRiego, 
     col = datos$Tratamiento,  
     pch = 16,                 
     xlab = "Wildtype", 
     ylab = "ExcesoRiego", 
     main = "Dispersión: ExcesoRiego vs Wildtype")

# Leyenda
legend("bottomright", 
       legend = levels(datos$Tratamiento),  
       col = 1:length(levels(datos$Tratamiento)),  
       pch = 16,                             
       title = "Tratamiento")   

# 5. Haz un histograma para cada variable. Recuerda mantener los colores. 

datos$Tratamiento <- as.factor(datos$Tratamiento)

colores <- c("skyblue", "orange", "green")

# Wildtype
hist(datos$Wildtype, 
     col = colores[1],  
     main = "Histograma: Wildtype", 
     xlab = "Wildtype", 
     ylab = "Frecuencia", 
     breaks = 10)

# Sequía
hist(datos$Sequia, 
     col = colores[2],  
     main = "Histograma: Sequía", 
     xlab = "Sequía", 
     ylab = "Frecuencia", 
     breaks = 10)

# ExcesoRiego
hist(datos$ExcesoRiego, 
     col = colores[3],  
     main = "Histograma: ExcesoRiego", 
     xlab = "ExcesoRiego", 
     ylab = "Frecuencia", 
     breaks = 10)

#6. Haz un factor en la columna tratamiento y guárdalo en una variable. 

tratamiento_factor <- factor(datos$Tratamiento)
tratamiento_factor

#7.Media y DV
# Media para cada tratamiento en Wildtype
tapply(datos$Wildtype, datos$Tratamiento, mean)

# DV para cada tratamiento en Wildtype
tapply(datos$Wildtype, datos$Tratamiento, sd)

# Media para cada tratamiento en Sequía
tapply(datos$Sequia, datos$Tratamiento, mean)

# DV para cada tratamiento en Sequía
tapply(datos$Sequia, datos$Tratamiento, sd)

# Media para cada tratamiento en ExcesoRiego
tapply(datos$ExcesoRiego, datos$Tratamiento, mean)

# DV para cada tratamiento en ExcesoRiego
tapply(datos$ExcesoRiego, datos$Tratamiento, sd)

#8.Averigua cuántos elementos tiene cada tratamiento. Recomendación: es más fácil si usas table() con el factor.

datos$Tratamiento <- as.factor(datos$Tratamiento)
table(datos$Tratamiento)

#9.Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una variable diferente. 

tratamiento_1 <- subset(datos, Tratamiento == 1)
tratamiento_4 <- subset(datos, Tratamiento == 4)

#10.Queremos comprobar que hay diferencias significativas para el tratamiento 1 y el tratamiento 5 entre Wildtype ySequia, y entre Wildtype y ExcesoRiego. Primero, necesitaríamos comprobar si los datos se distribuyen de formanormal. En función de los resultados de la prueba de normalidad, ¿qué test usarías para cada comparativa? ¿Puedescomparar también Sequia con ExcesoRiego en ambos tratamientos? ** En general, asumimos que las muestras son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo con tus resultados
#Prueba de Normalidad
tratamiento_5 <- subset(datos, Tratamiento == 5)
# Wildtype en Tratamiento 1
shapiro.test(tratamiento_1$Wildtype)

# Sequia en Tratamiento 1
shapiro.test(tratamiento_1$Sequia)

# ExcesoRiego en Tratamiento 1
shapiro.test(tratamiento_1$ExcesoRiego)

# Wildtype en Tratamiento 5
shapiro.test(tratamiento_5$Wildtype)

# Sequia en Tratamiento 5
shapiro.test(tratamiento_5$Sequia)

# ExcesoRiego en Tratamiento 5
shapiro.test(tratamiento_5$ExcesoRiego)

#Comparaciones
#Wildtype vs Sequia Tratamiento 1  
t.test(tratamiento_1$Wildtype,tratamiento_1$Sequia)

#Wildtype vs Sequia Tratamiento 5 
t.test(tratamiento_5$Wildtype,tratamiento_5$Sequia)

#Sequia vs ExcesoRiego Tratamiento 1
t.test(tratamiento_1$Sequia,tratamiento_1$ExcesoRiego)

#Sequia vs ExcesoRiego Tratamiento 5
t.test(tratamiento_5$Sequia,tratamiento_5$ExcesoRiego)

# 11. Realiza un ANOVA para comparar el tratamiento 1 en las tres condiciones. Pista: primero separa los valores de tratamiento1 en Wildtype, Sequia y ExcesoRiego en variables separadas. Luego fíjate en el archivo “datos-anova.txt” y trata de colocar los datos de esa forma en una tabla. Por último, ejecuta el test
#Primero separamos las condiciones del Tratamiento 1
wildtype1 <- tratamiento_1$Wildtype
sequia1 <- tratamiento_1$Sequia
excesoriego1 <- tratamiento_1$ExcesoRiego

#Creamos el dataframe
datos_anova <- data.frame(
  valor = c(wildtype1, sequia1, excesoriego1),
  condicion = factor(rep(c("Wildtype", "Sequia", "ExcesoRiego"), each = length(wildtype1)))
)

#Ejecutamos
anova_resultado <- aov(valor ~ condicion, data = datos_anova)
summary(anova_resultado)


