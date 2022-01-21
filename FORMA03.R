


# FORMA 03

# se importan librerias
if (!require(ggplot2)){
  install.packages("ggplot2", dependencies = TRUE )
  require (ggplot2)
}
if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(tidyr)){
  install.packages("tidyr", dependencies = TRUE )
  require (tidyr)
}

if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(emmeans)){
  install.packages("emmeans", dependencies = TRUE )
  require (emmeans)
}


#   ____                                          _               _ 
#  |  _ \   _ __    ___    __ _   _   _   _ __   | |_    __ _    / |
#  | |_) | | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |   | |
#  |  __/  | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |   | |
#  |_|     |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_|
#                         |___/                                 
#(23 puntos) Lord Vader desea saber si los niveles de exigencia con que los comandantes de las 
#diferentes divisiones evalúan a los nuevos soldados son similares, por lo que le ha solicitado 
#estudiar si existen diferencias significativas en el promedio de la evaluación realizada por el
#comandante entre las distintas divisiones. El Lord Sith ha sido muy claro al solicitar un reporte
#de aquellas divisiones en las que se observen diferencias.

# Se leen los datos
datos <- read.csv2(file.choose(), encoding = "UTF-8", sep = ";")

alfa <- 0.01

# Hipótesis nula:
# H0: La evaluacion promedio de los comandantes es igual para todas las divisiones

# Hipótesis alternativa:
# Ha: La evaluacion promedio de los comandantes es diferente para al menos una divisiones


g <- ggqqplot(
  datos,
  x = "division",
  color = "eval_comandante"
)

g <- g + facet_wrap(~ division)
print(g)

prueba <- aov (eval_comandante~division,
               data = datos)
cat ("Resultado de la prueba ANOVA")
print (summary (prueba))


# GrÃ¡fico del tamaÃ±o del efecto
g2 <- ezPlot (
  data = datos,
  dv = eval_comandante,
  wid = id,#instancia
  between = division,
  y_lab = "Evaluacion promedio de los soldados [g]",
  x = division
)

print (g2)

#Warning: muestra un warning en consola, esto es debido a que la cantidad de datos por grupo es diferente, 
#de todos modos no afecta el grÃ¡fico.


#GrÃ¡fico de cajas que compara los pesos de los pollitos por tipo de suplemento
g3 <- boxplot(eval_comandante ~ division,
              data = datos,
              border = "red",
              col = "pink",
              ylab = "Evaluacions de los comandantes [g]",
              xlab = "Division")

print (g3)


cat ("\n\ nProcedimiento post - hoc de Holm \n\n")

holm<-pairwise.t.test ( datos[["eval_comandante"]],
                        datos[["division"]],
                        p.adj = "holm",
                        pool.sd = TRUE,
                        paired = FALSE,
                        conf.level = 1 - alfa )
print(holm)


#Pairwise comparisons using t tests with pooled SD 

#data:  datos[["eval_comandante"]] and datos[["division"]] 

#Cavetrooper Flametrooper Lavatrooper Recontrooper Sandtrooper Shoretrooper Snowtrooper
#Flametrooper <2e-16      -            -           -            -           -            -          
#  Lavatrooper  <2e-16      <2e-16       -           -            -           -            -          
#  Recontrooper 1           <2e-16       <2e-16      -            -           -            -          
#  Sandtrooper  1           <2e-16       <2e-16      1            -           -            -          
#  Shoretrooper 1           <2e-16       <2e-16      1            1           -            -          
#  Snowtrooper  1           <2e-16       <2e-16      1            1           1            -          
#  Spacetrooper 1           <2e-16       <2e-16      1            1           1            1          

#P value adjustment method: holm 





#                                                _               ____  
#  _ __    _ __    ___    __ _   _   _   _ __   | |_    __ _    |___ \ 
# | '_ \  | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |     __) |
# | |_) | | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |    / __/ 
# | .__/  |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_____|
# |_|                    |___/                                         

#(24 puntos) A fin de determinar si es necesario establecer programas de entrenamiento 
#diferenciados para clones y reclutas, Lord Vader quiere saber si es posible distinguir 
#entre ambas clases de soldados con los datos actuales. Para ello, ha solicitado evaluar 
#un modelo clasificador que contemple entre 2 y 5 variables predictoras. Considere que, 
#para ser aceptable, el modelo:
#• Debe lograr una exactitud (accuracy) de al menos 0,8 en datos de prueba
#• No puede considerar casos con demasiada influencia (considerando la distancia de Cook)
#• No debe presentar autocorrelación (usando la prueba de Durbin-Watson para un retardo y un nivel de significación α = .01)
#• No debe presentar multicolinealidad severa (considerando el factor de inflación de la varianza, con un VIF promedio inferior a 1,03).
#Considere la semilla 407 para obtener una muestra de 400 datos, 80% de los cuales serán 
#empleados para ajustar el modelo y el 20% restante, para evaluarlo.

#Se lee el archivo y con la semilla 407 se escogen 400 muestas al azar.
datos <- read.csv2(file.choose(), encoding = "UTF-8", sep = ";")
set.seed(407)
muestra <- sample_n(datos, size= 400)
head(muestra)

#Se separan el conjunto de entrenamiento y el conjunto de prueba.
sample <- sample.int(n = nrow(muestra), size = floor(.80*nrow(muestra)), replace = F)
entrenamiento <- muestra[sample, ]
prueba  <- muestra[-sample, ]


#                                                _               _____ 
#  _ __    _ __    ___    __ _   _   _   _ __   | |_    __ _    |___ / 
# | '_ \  | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |     |_ \ 
# | |_) | | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |    ___) |
# | .__/  |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |____/ 
# |_|                    |___/                                         

#Pregunta 3
#(9 puntos) Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) 
#en donde un estudio o experimento, relacionado con las expectativas de los chilenos para el nuevo 
#gobierno, necesite utilizar una prueba de Friedman debido a problemas con la escala de la variable 
#dependiente en estudio. Indiqué cuáles serían las variables involucradas en su ejemplo (con sus 
#respectivos niveles) y las hipótesis nula y alternativa a contrastar.

#El futuro gobierno desea evaluar la apreciación púlblica sobre la construcción de plantas de energía
#electrica de distintos tipos en el sur del país, para ello se seleccionan al azar una muestra de 5000
#personas a las cuales se las informará con detalle las diferencias entre plantas de energía, Eólica,
#Solar, Hidroeléctira y Termoeléctrica. Diferencias tales como, uso del espacio público, costo de contrucción
#costo final al usuario, etc. Luego se les pide que clasifiquen en una escala numérica de 1 a 5 con 1 siendo 
#"absolutamente en desacuerdo" y 5 "completamente de acuerdo" a calificar y clasificar aspectos tales como:
#1.-Que se construya una planta de energía de ESE tipo, en un sector cercano a su hogar.
#2.-Que esta dispuesto a pagar los costos
#3.-El nivel de contaminación que crea
#ETC.
#El análisis tiene como fin, encontrar cúal o cuales plantas son de
#preferencia pública en cuanto a su constucción pública. 

#H0 - No hay diferencia en preferencia de construcción tipos de plantas de energía
#HA - Hay alguna diferencia en prefrencia de contrucción entre almenos dos tipos plantas de energía

#Las condiciones iniciales se cumplen ya que:

#1. La variable independiente debe ser categórica y tener a lo menos tres niveles.
#Se clasifican más de 3 distintos aspectos de las plantas además de 
#ser 4 tipos de plantas distintas.

#2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
#Es una escala de 1 a 5 con 5 siendo lo mejor y 1 lo peor. Es de caracter original

#3. Los sujetos son una muestra aleatoria e independiente de la población.
#Se escogen azar aquellos que van a clasificar las plantas de energía.












