############################################################################
###########  Aguascalientes Ingresos por decil Hogares Ind?genas ##########
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

Conc<-Conc%>%
  filter(HogarIndig==1)

######################################################################################################
########################## Vamos con los totales y la creaci?n de cuadros #############################
#######################################################################################################

#recordemos que Nhog son solo unos.
#esta x es un "array" que resulta de la suma de todos los factor. Es decir, el n?mero de hogares. 34.7 millones.
#aparentemente este "array" es una especie de matriz. En este caso, parece que de 1*1.
#lo que hace tapply es "buscar una variable categoricas y aplicar una funci?n por grupo."
#lo que hace es que te regresa una matriz "array" con los resultados de la funci?n por cada categor?a de la otra variable.
#En este caso, suma lo que hay en factor por las categor?as que ten?amos en Nhog. Pero Nhog son solo 1?s.
#no le veo el caso.
x<-tapply(Conc$factor,Conc$Nhog,sum) 

#ahora vamos a crear y  bajo la misma figura
#Aqu? si tenemos que la variable decil es categorica. Aqu? s? tiene sentido.
#creo que lo que me est? diciendo es cu?ntos hogares quedaron en cada decil.
y<-tapply(Conc$factor,Conc$DECIL,sum)

#ahor vamos a calcular el ingreso promedio para todos los Hogares
#lo que hicimos aqu? fue multiplicar el n?mero de hogares por el ingreso respectivo. 
#y sumarlos por la clasificaci?n de nhog (que es un uno! osea que solo hay una clasificaci?n)
#luego dividimos eso sobre el n?mero de total de hogares que ya ten?amos en x. 
#el resultado es un ingreso trimestral promedio de $49,610
ing_cormed_t<-tapply(Conc$factor*Conc$ing_cor,Conc$Nhog,sum)/x 

#ahora vamos a ahcer lo mismo, pero por decil
ing_cormed_d<-tapply(Conc$factor*Conc$ing_cor,Conc$DECIL,sum)/y

#aqu? est?n los ingresos promedio por decil...Booooom, bitch!!!!
ing_cormed_d

##################################################################################################
############################## Cuadros ##########################################################
################################################################################################

#creamos una data frame 
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))

#Let?s crate the deciles
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

#le ponemos nombre a las filas
row.names(prom_rub)<-Numdec

prom_rub

write.dbf(prom_rub,file="Aguascalientes Indigena ingresos por decil y GINI .dbf")


################################################################################################
############################ Indice de GINI ###################################################
#############################################################################################

#vamos a crear un data frame
#lo que queda son dos culumnas. Una con el n?mero de hogares ede cada decil que es igual. y la otra con el ingreso pormedio.
deciles_hog_ingcor <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x), #recordemos que x es el n?mero de hogaeres por decil. AQu? est? 10 veces
                                 ingreso=c(ing_cormed_d[1],ing_cormed_d[2],ing_cormed_d[3], #y aqu? estamos pniendo en cada observaci?n del data frame el ingreso promedio de ese decil
                                           ing_cormed_d[4],ing_cormed_d[5],ing_cormed_d[6],
                                           ing_cormed_d[7],ing_cormed_d[8],ing_cormed_d[9],
                                           ing_cormed_d[10]))
#ahora creamos "a" que deriva de la funci?n GINI
#la funci?n gini requiere dos argumento. Uno de ingresos y otro de pesos
#En los pesos, van el n?mero de hogares.
a<-gini(deciles_hog_ingcor$ingreso,weights=deciles_hog_ingcor$hogares)

#ahora renombramos las columnas de nuestro cuado prom_rub
names(prom_rub)=c("INGRESO CORRIENTE") #cosa que pudimos y debimos hacer arriba cuando lo creamos

#le cmabiamos el numbre de a GINI
names(a)="GINI"

a

