###########ENUNCIADO PREGUNTA 1#####################
# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los comandantes de las diferentes divisiones
# evalúan a los nuevos soldados son similares, por lo que le ha solicitado estudiar si existen diferencias significativas en el
# promedio de la evaluación realizada por el comandante entre las distintas divisiones. El Lord Sith ha sido muy claro al
# solicitar un reporte de aquellas divisiones en las que se observen diferencias.


# Responder porque se aplica la prueba que aplicaremos.
# Se aplicara la prueba de ANOVA de una via para muestras independientes ya que queremos ver las diferencias que existen
# entre las medias de la evalucion del comandante a los stormtrooper

# Formulación de hipótesis:
# H0: La media de la evaluación de los comandante es igual para todas las divisiones.
# HA: La media de la evaluación de los comandante es distinta para al menos una de las divisiones.


# Verifiación de condiciones para usar ANOVA para muestras independientes:

# 1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
# iguales.
# Esta condicion se verifica viendo el enunciado donde se muestra que la varible dependiente (eval_comandante)
# sigue un intervalo de valores iguales.

# 2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
# Se puede asumir una aleatoriedad e indepencia, debido a que las divisiones estan compuestas por diferentes soldados
# provenientes de diferentes planetas obtenidos aleatoriamente.

# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
# se comprueba que las pruebas siguen una distribución normal, donde podemos ver el grafico Q-Q realizado para
# cada muestra y no se observan grandes cantidades de valores atipicos, salvo en Recontrooper donde dos se salen
# de lo normal, pero para estar seguros se procede con cautela y se usa un nivel de significación alfa = 0,025

# 4. Las k muestras tienen varianzas aproximadamente iguales.
# Se hace una prueba de homocedasticidad donde se obtienen las varianzas de cada grupo, luego se calcula la homocedasticidad
# donde la razon entre la maxima y minima varianza muestral es de 1.482657, que se cumple que es menor a 1.5 por lo que
# queda verificada la homogeneidad de las varianzas.


# Importación de librerias
library(dplyr)
library(ggpubr)
library(ez)
library(MASS)


# Cambiar dirección donde tenga su archivo.
datos<-read.csv2("C:\\Users\\Lenovo\\Desktop\\Datos PEP 2.csv")

# Comprobación de normalidad
datos[["division"]] <- factor(datos[["division"]])
datos[["instancia"]] <- factor(1:nrow(datos))

Cavetrooper <- datos %>% filter(division=="Cavetrooper")
Snowtrooper <- datos %>% filter(division=="Snowtrooper")
Lavatrooper <- datos %>% filter(division=="Lavatrooper")
Shoretrooper <- datos %>% filter(division=="Shoretrooper")
Spacetrooper <- datos %>% filter(division=="Spacetrooper")
Sandtrooper <- datos %>% filter(division=="Sandtrooper")
Flametrooper <- datos %>% filter(division=="Flametrooper")
Recontrooper <- datos %>% filter(division=="Recontrooper")


muestra_Cave <- Cavetrooper[["eval_comandante"]]
muestra_Snow <- Snowtrooper[["eval_comandante"]]
muestra_Lava <- Lavatrooper[["eval_comandante"]]
muestra_Shore <- Shoretrooper[["eval_comandante"]]
muestra_Space <- Spacetrooper[["eval_comandante"]]
muestra_Sand <- Sandtrooper[["eval_comandante"]]
muestra_Flame <- Flametrooper[["eval_comandante"]]
muestra_Recon <- Recontrooper[["eval_comandante"]]

# grafico Q-Q para comprobar normalidad
g <- ggqqplot(datos, x = "eval_comandante", y ="division", color="division")

g <- g + facet_wrap(~ division)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)


# Homogeneidad de las varianzas
var_Cave <- sd(muestra_Cave)^2
var_Snow <- sd(muestra_Snow)^2
var_Lava <- sd(muestra_Lava)^2
var_Shore <- sd(muestra_Shore)^2
var_Space <- sd(muestra_Space)^2
var_Sand <- sd(muestra_Sand)^2
var_Flame <- sd(muestra_Flame)^2
var_Recon <- sd(muestra_Recon)^2

varianzas <- c(var_Cave, var_Snow, var_Lava, var_Shore, var_Space, var_Sand, var_Flame, var_Recon)
homogeneidad <- max(varianzas) / min(varianzas)
cat("Homogeneidad de las varianzas", homogeneidad)

# PRUEBA ANOVA 
prueba <- ezANOVA(data = datos, dv = eval_comandante, wid = instancia, between = division, return_aov = TRUE, type = 2)
print(prueba)

# Gráfico del tamaño del efecto
g2 <- ezPlot(data =datos, dv = eval_comandante, wid = instancia, between = division, y_lab = "Evaluación promedio de los comandantes",
             x = division)
print(g2)

####### CONCLUSIONES ####### 
# Dado que nuestro valor P es mucho menor a nuestro nivel de significacion alfa, con un 99% de seguridad
# se rechaza la hipotesis nula en favor a la hipotesis alternativa.


###########ENUNCIADO PREGUNTA 2#####################



###########ENUNCIADO PREGUNTA 3#####################
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en donde un
# estudio o experimento, relacionado con las expectativas de los chilenos para el nuevo gobierno, necesite utilizar una
# prueba de Friedman debido a problemas con la escala de la variable dependiente en estudio. Indiqué cuáles serían las
# variables involucradas en su ejemplo (con sus respectivos niveles) y las hipótesis nula y alternativa a contrastar.

# Luego de que Boric saliera electo en las ultimas elecciones, se ha hablado 
# mucho de que pasara con el país, por lo que, se decidió 
# realizar un estudio respecto a las expectativas de los chilenos para este 
# nuevo gobierno. Por lo que se ha seleccionado una muestra aleatoria representativa 
# de las 3 clases sociales (clase baja, clase media y clase alta) y se les ha solicitado 
# evaluar 2 aspectos de las expectativas del gobierno de Boric con una escala Likert de 5 puntos, 
# donde el valor 1 corresponde a una valoración muy negativa y 5, a una muy positiva. Entonces las 
# hipótesis a contrastar son:
# H0: las diferentes clases sociales tienen expectativas similares. 
# HA: al menos una clase social obtiene una expectativa distinta a las demás.


