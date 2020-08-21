############################################################################
###################  Ingresos por decil Aguascalientes ####################
##########################################################################

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/2018/Aguascalientes")
Conc<-read.dbf("ConcAguascalientes.dbf",as.is = T)

######################################################################################################
########################## Vamos con los totales y la creación de cuadros #############################
#######################################################################################################

#recordemos que Nhog son solo unos.
#esta x es un "array" que resulta de la suma de todos los factor. Es decir, el número de hogares. 34.7 millones.
#aparentemente este "array" es una especie de matriz. En este caso, parece que de 1*1.
#lo que hace tapply es "buscar una variable categoricas y aplicar una función por grupo."
#lo que hace es que te regresa una matriz "array" con los resultados de la función por cada categoría de la otra variable.
#En este caso, suma lo que hay en factor por las categorías que teníamos en Nhog. Pero Nhog son solo 1´s.
#no le veo el caso.
x<-tapply(Conc$factor,Conc$Nhog,sum) 

#ahora vamos a crear y  bajo la misma figura
#Aquí si tenemos que la variable decil es categorica. Aquí sí tiene sentido.
#creo que lo que me está diciendo es cuántos hogares quedaron en cada decil.
y<-tapply(Conc$factor,Conc$DECIL,sum)

#ahor vamos a calcular el ingreso promedio para todos los Hogares
#lo que hicimos aquí fue multiplicar el número de hogares por el ingreso respectivo. 
#y sumarlos por la clasificación de nhog (que es un uno! osea que solo hay una clasificación)
#luego dividimos eso sobre el número de total de hogares que ya teníamos en x. 
#el resultado es un ingreso trimestral promedio de $49,610
ing_cormed_t<-tapply(Conc$factor*Conc$ing_cor,Conc$Nhog,sum)/x 

#ahora vamos a ahcer lo mismo, pero por decil
ing_cormed_d<-tapply(Conc$factor*Conc$ing_cor,Conc$DECIL,sum)/y

#aquí están los ingresos promedio por decil...Booooom, bitch!!!!
ing_cormed_d

##################################################################################################
############################## Cuadros ##########################################################
################################################################################################

#creamos una data frame 
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))

#Let´s crate the deciles
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

#le ponemos nombre a las filas
row.names(prom_rub)<-Numdec

prom_rub

write.dbf(prom_rub,file="Aguascalientes ingresos por decil y GINI .dbf")

################################################################################################
############################ Indice de GINI ###################################################
#############################################################################################

#vamos a crear un data frame
#lo que queda son dos culumnas. Una con el número de hogares ede cada decil que es igual. y la otra con el ingreso pormedio.
deciles_hog_ingcor <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x), #recordemos que x es el número de hogaeres por decil. AQuí está 10 veces
                                 ingreso=c(ing_cormed_d[1],ing_cormed_d[2],ing_cormed_d[3], #y aquí estamos pniendo en cada observación del data frame el ingreso promedio de ese decil
                                           ing_cormed_d[4],ing_cormed_d[5],ing_cormed_d[6],
                                           ing_cormed_d[7],ing_cormed_d[8],ing_cormed_d[9],
                                           ing_cormed_d[10]))
#ahora creamos "a" que deriva de la función GINI
#la función gini requiere dos argumento. Uno de ingresos y otro de pesos
#En los pesos, van el número de hogares.
a<-gini(deciles_hog_ingcor$ingreso,weights=deciles_hog_ingcor$hogares)

#ahora renombramos las columnas de nuestro cuado prom_rub
names(prom_rub)=c("INGRESO CORRIENTE") #cosa que pudimos y debimos hacer arriba cuando lo creamos

#le cmabiamos el numbre de a GINI
names(a)="GINI"

a

