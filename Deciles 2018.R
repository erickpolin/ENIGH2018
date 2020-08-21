#ENIGH 2018
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/2018")
Conc<-read.dbf("concentradohogar.dbf",as.is = T)

#Keeping Variables of interest
Conc <- Conc [ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis")]

################ DEfinir hogares inígenas#################
Poblacion<-read.dbf("poblacion.dbf",as.is = T)

Poblacion <- Poblacion [ c("folioviv", "foliohog", "numren", "parentesco","hablaind","comprenind","etnia")]

#El concepto de hogar indígena se ha definido como aquel donde el jefe(a), 
#su cónyuge o alguno de los ascendientes (madre o padre, madrastra o padrastro, abuelo(a),
#bisabuelo(a), tatarabuelo(a), suegro(a)) declararon hablar alguna lengua indígena.
parentescos<-c(101,102,201,202,203,204,205,601,602,606,607,608,615,616)

Poblacion<- Poblacion%>%
  mutate(HogarIndigena=ifelse(parentesco%in%parentescos&hablaind==1|parentesco%in%parentescos&comprenind==1|parentesco%in%parentescos&etnia==1,1,0))

HogaresIndigenas<-Poblacion %>%
  group_by(folioviv,foliohog)%>%
  summarize(HogarIndigena=mean(HogarIndigena))

HogaresIndigenas<-data.frame(HogaresIndigenas)

HogaresIndigenas<-HogaresIndigenas%>%
  mutate(HogarIndigena=ifelse(HogarIndigena>0,1,0))

prop.table(table(HogaresIndigenas$HogarIndigena))

Conc<-merge(Conc,HogaresIndigenas,by=c("folioviv","foliohog"))

########ya están los hogares indígenas############

#the fist two digits of "folioviv" makes reference to the state
#Let´s create a variable called entidad that contains thos two first digits of the "folioviv" variable
Conc$entidad<-substr(Conc$folioviv,1,2)

######################################################

write.dbf(Conc,file="Conc.dbf")







#Let´s crate the deciles
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

#apparently this is a "flag", IDK what is this shit yet
Conc$Nhog <- 1

########################################## DECILES ##########################################

#Attaching the data frame
attach(Conc) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to ing_cor, folioviv, foliohog
Conc<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc) #this give us the households sorted by total income

#Adding the values of the expansion factor. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(Conc$factor,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/10) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
Conc$tam_dec<-tam_dec

############################# Creating Deciles of Income ###########################
BD1 <- Conc #por algún motivo quiere que se renombre la base

BD1$MAXT<-BD1$ing_cor #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

BD1<-BD1[with(BD1, order(rank(MAXT))),]  #lo que hicimos aquí fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

BD1$ACUMULA<-cumsum(BD1$factor) #aquí creamos una variable de suma acumulada del factor de viviendas.


######################################################################################################
############################Ahora viene la creación de los deciles###################################
######################################################################################################

#no se que es esto de a1 y b1. Pero sí e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],
             BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aquí estamos creando otra variable de suma acumulada del número de hogares
BD1$ACUMULA2<-cumsum(BD1$factor)

#aquí estamos creando una variable que se llama decil que solo tiene ceros
BD1$DECIL<-0

#recordemos que el tamaño de cada decil es de 3,474,481. 
#loq ue hicimos aquí es pedirle que ponga un uno a los primeros hogares menores al tamaño de decil. 
#es decir, que el primer decil tiene ya UNOS
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesión del 1 al 9, cuando la variable acumulado2 sea mayor que el tamaño de decil multiplicado por
#1, 2, 3... pone en la variable decil el número i+1
for(i in 1:9)
{
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le quedó cero (que es la última observación), ponle el decil 10
BD1[BD1$DECIL%in%"0",]$DECIL<-10


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
x<-tapply(BD1$factor,BD1$Nhog,sum) 

#ahora vamos a crear y  bajo la misma figura
#Aquí si tenemos que la variable decil es categorica. Aquí sí tiene sentido.
#creo que lo que me está diciendo es cuántos hogares quedaron en cada decil.
y<-tapply(BD1$factor,BD1$DECIL,sum)

#ahor vamos a calcular el ingreso promedio para todos los Hogares
#lo que hicimos aquí fue multiplicar el número de hogares por el ingreso respectivo. 
#y sumarlos por la clasificación de nhog (que es un uno! osea que solo hay una clasificación)
#luego dividimos eso sobre el número de total de hogares que ya teníamos en x. 
#el resultado es un ingreso trimestral promedio de $49,610
ing_cormed_t<-tapply(BD1$factor*BD1$ing_cor,BD1$Nhog,sum)/x 

#ahora vamos a ahcer lo mismo, pero por decil
ing_cormed_d<-tapply(BD1$factor*BD1$ing_cor,BD1$DECIL,sum)/y

#aquí están los ingresos promedio por decil...Booooom, bitch!!!!
ing_cormed_d

##################################################################################################
############################## Cuadros ##########################################################
################################################################################################

#creamos una data frame 
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))

#le ponemos nombre a las filas
row.names(prom_rub)<-Numdec

prom_rub

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

#####################################################################################################
#########################  Fuentes de ingreso por entidad federativa   #############################
###################################################################################################

library(foreign) # librería que nos ayuda a leer las tablas en diferentes formatos
library(survey) # librería para calcular el diseño muestral

# limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())

#la carpeta...
setwd("C:/Users/Erick/Dropbox/GIC/2018")

#abrimos los datos
Conc<-read.dbf("concentradohogar.dbf",as.is = T)

#nos quedamos con lo que se necesita
Conc <- Conc [ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis")]

#creamos la entidad federativa
Conc$entidad <- substr(Conc$folioviv,1,2)

#Definimos un vector con la entidad federativa
Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur",
             "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "México", "Durango",
             "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo",
             "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
             "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán",
             "Zacatecas")
#mismo proceso que los deciles
#creamos una "bandera"para enumerar los hogares. Le pondremos un 1 a todos.
Conc$Nhog <- 1

#aquí vamos a utilizar el paquete Survey
#Vamos a cargar el diseño muestral

mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc,weights=~factor)

#vamos por el ingreso corriente total del país
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notesé que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 


#ahora, vamos a hacer lo mismo por entidad
#aquí cmabia la función a svyby, en by va la entidad que creamos.
#y al final va la función que queremos
Ming_corEnt <- svyby(~ing_cor,denominator=~Nhog,by=~entidad ,mydesign,svyratio)

############################################################################################################
###################################        Trabajo       #######################################
############################################################################################################
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~ingtrab,denominator=~Nhog,mydesign) # Total promedio
MingtrabEnt <- svyby(~ingtrab,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # por entidad
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~trabajo,denominator=~Nhog,mydesign) # Total promedio
MtrabajoEnt <- svyby(~trabajo,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # por entidad
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~negocio,denominator=~Nhog,mydesign) # Total promedio
MnegocioEnt <- svyby(~negocio,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # por entidad
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~otros_trab,denominator=~Nhog,mydesign) # Total promedio
Motros_trabEnt<- svyby(~otros_trab,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # por entidad

############################################################################################################
###################################        Rentas de la propiedad      #######################################
############################################################################################################

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~rentas,denominator=~Nhog,mydesign) # Total promedio
MrentasEnt <- svyby(~rentas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio
###### ingresos de sociedades
MutilidadTot <- svyratio(~utilidad,denominator=~Nhog,mydesign) # Total promedio
MutilidadEnt <- svyby(~utilidad,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio
###### arrendamiento
MarrendaTot <- svyratio(~arrenda,denominator=~Nhog,mydesign) # Total promedio
MarrendaEnt <- svyby(~arrenda,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

############################################################################################################
###################################        Transferencias   #######################################
#####################################################################################################

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~transfer,denominator=~Nhog,mydesign) # Total promedio
MtransferEnt <- svyby(~transfer,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibió jubilaciones. Así que puede ser públicas o privadas.

MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionEnt <- svyby(~jubilacion,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### becas que pueden ser, de nuevo, públicas privadas. 
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasEnt <- svyby(~becas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### donativos que también pueden ser públicos o privados.
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosEnt <- svyby(~donativos,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### remesas se definen como ingresos provenientes d eotros paises. Así de manera genérica.
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasEnt <- svyby(~remesas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### bene_gob:  aquí estna los programas públicos. Prospera, procampo, 65 y más, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobEnt <- svyby(~bene_gob,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogEnt <- svyby(~transf_hog,denominator=~Nhog,by=~entidad ,mydesign,svyratio)

###### trans_inst: puede venir de institucione spúblicas o privadas.
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instEnt <- svyby(~trans_inst,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

### estim_alqu ### Aparentemente se le pregunta al entrevistado cuánto constaría la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquEnt <- svyby(~estim_alqu,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

### otros_ing ### es literalmente ¿algo más?
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingEnt <- svyby(~otros_ing,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio


#####################################################################################################
######################################### Estimaciones #############################################
##################################################################################################

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aquí es extraer el valor de la primera columa que corresponde al cálculo.
ES_Ming_corEnt <- Ming_corEnt[[2]] #En el caso de las entidades, los cálculos quedaron en la segunda columna
ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabEnt <- MingtrabEnt[[2]]
ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoEnt <- MtrabajoEnt[[2]]
ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioEnt <- MnegocioEnt[[2]]
ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabEnt <- Motros_trabEnt [[2]]
ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasEnt <- MrentasEnt [[2]]
ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadEnt <- MutilidadEnt [[2]]
ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaEnt <- MarrendaEnt [[2]]
ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferEnt <- MtransferEnt[[2]]
ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionEnt <- MjubilacionEnt [[2]]
ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasEnt <- MbecasEnt [[2]]
ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosEnt <- MdonativosEnt[[2]]
ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasEnt <- MremesasEnt[[2]]
ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobEnt <- Mbene_gobEnt [[2]]
ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogEnt <- Mtransf_hogEnt [[2]]
ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instEnt <- Mtrans_instEnt[[2]]
ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquEnt <- Mestim_alquEnt [[2]]
ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingEnt <- Motros_ingEnt [[2]]

########## Error Estándar ##########
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corEnt <- SE (Ming_corEnt)
SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabEnt <- SE (MingtrabEnt)
SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoEnt <- SE (MtrabajoEnt)
SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioEnt <- SE (MnegocioEnt)
SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabEnt <- SE (Motros_trabEnt)
SE_MrentasTot <- SE (MrentasTot)
SE_MrentasEnt <- SE (MrentasEnt)
SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadEnt <- SE (MutilidadEnt)
SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaEnt <- SE (MarrendaEnt)
SE_MtransferTot <- SE (MtransferTot)
SE_MtransferEnt <- SE (MtransferEnt)
SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionEnt <- SE (MjubilacionEnt)
SE_MbecasTot <- SE (MbecasTot)
SE_MbecasEnt <- SE (MbecasEnt)
SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosEnt <- SE (MdonativosEnt)
SE_MremesasTot <- SE (MremesasTot)
SE_MremesasEnt <- SE (MremesasEnt)
SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobEnt <- SE (Mbene_gobEnt)
SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogEnt <- SE (Mtransf_hogEnt)
SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instEnt <- SE (Mtrans_instEnt)
SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquEnt <- SE (Mestim_alquEnt)
SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingEnt <- SE (Motros_ingEnt)

########## Coeficiente de variación ##########
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corEnt <- cv(Ming_corEnt)
CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabEnt <- cv(MingtrabEnt)
CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoEnt <- cv(MtrabajoEnt)
CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioEnt <- cv(MnegocioEnt)
CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabEnt <- cv(Motros_trabEnt)
CV_MrentasTot <- cv(MrentasTot)
CV_MrentasEnt <- cv(MrentasEnt)
CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadEnt <- cv(MutilidadEnt)
CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaEnt <- cv(MarrendaEnt)
CV_MtransferTot <- cv(MtransferTot)
CV_MtransferEnt <- cv(MtransferEnt)
CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionEnt <- cv(MjubilacionEnt)
CV_MbecasTot <- cv(MbecasTot)
CV_MbecasEnt <- cv(MbecasEnt)
CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosEnt <- cv(MdonativosEnt)
CV_MremesasTot <- cv(MremesasTot)
CV_MremesasEnt <- cv(MremesasEnt)
CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobEnt <- cv(Mbene_gobEnt)
CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogEnt <- cv(Mtransf_hogEnt)
CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instEnt <- cv(Mtrans_instEnt)
CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquEnt <- cv(Mestim_alquEnt)
CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingEnt <- cv(Motros_ingEnt)
########## Limite inferior ##########
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corEnt <- confint(Ming_corEnt,level=0.90)[,1]
LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabEnt <- confint(MingtrabEnt,level=0.90)[,1]
LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoEnt <- confint(MtrabajoEnt,level=0.90)[,1]
LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioEnt <- confint(MnegocioEnt,level=0.90)[,1]
LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabEnt <- confint(Motros_trabEnt,level=0.90)[,1]
LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasEnt <- confint(MrentasEnt,level=0.90)[,1]
LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadEnt <- confint(MutilidadEnt,level=0.90)[,1]
LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaEnt <- confint(MarrendaEnt,level=0.90)[,1]
LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferEnt <- confint(MtransferEnt,level=0.90)[,1]
LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionEnt <- confint(MjubilacionEnt,level=0.90)[,1]
LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasEnt <- confint(MbecasEnt,level=0.90)[,1]
LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosEnt <- confint(MdonativosEnt,level=0.90)[,1]
LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasEnt <- confint(MremesasEnt,level=0.90)[,1]
LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobEnt <- confint(Mbene_gobEnt,level=0.90)[,1]
LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogEnt <- confint(Mtransf_hogEnt,level=0.90)[,1]
LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instEnt <- confint(Mtrans_instEnt,level=0.90)[,1]
LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquEnt <- confint(Mestim_alquEnt,level=0.90)[,1]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingEnt <- confint(Motros_ingEnt,level=0.90)[,1]

########## Limite superior ##########
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corEnt <- confint(Ming_corEnt,level=0.90)[,2]
LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabEnt <- confint(MingtrabEnt,level=0.90)[,2]
LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoEnt <- confint(MtrabajoEnt,level=0.90)[,2]
LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioEnt <- confint(MnegocioEnt,level=0.90)[,2]
LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabEnt <- confint(Motros_trabEnt,level=0.90)[,2]
LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasEnt <- confint(MrentasEnt,level=0.90)[,2]
LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadEnt <- confint(MutilidadEnt,level=0.90)[,2]
LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaEnt <- confint(MarrendaEnt,level=0.90)[,2]
LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferEnt <- confint(MtransferEnt,level=0.90)[,2]
LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionEnt <- confint(MjubilacionEnt,level=0.90)[,2]
LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasEnt <- confint(MbecasEnt,level=0.90)[,2]
LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosEnt <- confint(MdonativosEnt,level=0.90)[,2]
LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasEnt <- confint(MremesasEnt,level=0.90)[,2]
LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobEnt <- confint(Mbene_gobEnt,level=0.90)[,2]
LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogEnt <- confint(Mtransf_hogEnt,level=0.90)[,2]
LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instEnt <- confint(Mtrans_instEnt,level=0.90)[,2]
LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquEnt <- confint(Mestim_alquEnt,level=0.90)[,2]
LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingEnt <- confint(Motros_ingEnt,level=0.90)[,2]


####################################################################################################
#############################      Cuadros       ##################################################
##################################################################################################

#este cuadro, lo único que tiene son todas la estimaciones.
#son 33 filas y 18 columnas.
c_ent_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corEnt),c(ES_MingtrabTot,ES_MingtrabEnt),c(ES_MtrabajoTot,ES_MtrabajoEnt),c(ES_MnegocioTot,ES_MnegocioEnt)
             ,c(ES_Motros_trabTot,ES_Motros_trabEnt),c(ES_MrentasTot,ES_MrentasEnt),c(ES_MutilidadTot,ES_MutilidadEnt)
             ,c(ES_MarrendaTot,ES_MarrendaEnt),c(ES_MtransferTot,ES_MtransferEnt),c(ES_MjubilacionTot,ES_MjubilacionEnt),c(ES_MbecasTot,ES_MbecasEnt),
             c(ES_MdonativosTot,ES_MdonativosEnt),c(ES_MremesasTot,ES_MremesasEnt),c(ES_Mbene_gobTot,ES_Mbene_gobEnt),c(ES_Mtransf_hogTot,ES_Mtransf_hogEnt)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instEnt),c(ES_Mestim_alquTot,ES_Mestim_alquEnt),c(ES_Motros_ingTot,ES_Motros_ingEnt))
##### ERROR ESTANDAR
c_ent_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corEnt),c(SE_MingtrabTot,SE_MingtrabEnt),c(SE_MtrabajoTot,SE_MtrabajoEnt),c(SE_MnegocioTot,SE_MnegocioEnt)
             ,c(SE_Motros_trabTot,SE_Motros_trabEnt),c(SE_MrentasTot,SE_MrentasEnt),c(SE_MutilidadTot,SE_MutilidadEnt)
             ,c(SE_MarrendaTot,SE_MarrendaEnt),c(SE_MtransferTot,SE_MtransferEnt),c(SE_MjubilacionTot,SE_MjubilacionEnt),c(SE_MbecasTot,SE_MbecasEnt),
             c(SE_MdonativosTot,SE_MdonativosEnt),c(SE_MremesasTot,SE_MremesasEnt),c(SE_Mbene_gobTot,SE_Mbene_gobEnt),c(SE_Mtransf_hogTot,SE_Mtransf_hogEnt),c(SE_Mtrans_instTot,SE_Mtrans_instEnt)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquEnt),c(SE_Motros_ingTot,SE_Motros_ingEnt))

##### COEFICIENTE DE VARIACION
c_ent_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corEnt),c(CV_MingtrabTot,CV_MingtrabEnt),c(CV_MtrabajoTot,CV_MtrabajoEnt),c(CV_MnegocioTot,CV_MnegocioEnt)
             ,c(CV_Motros_trabTot,CV_Motros_trabEnt),c(CV_MrentasTot,CV_MrentasEnt),c(CV_MutilidadTot,CV_MutilidadEnt
             ),c(CV_MarrendaTot,CV_MarrendaEnt),c(CV_MtransferTot,CV_MtransferEnt),c(CV_MjubilacionTot,CV_MjubilacionEnt),c(CV_MbecasTot,CV_MbecasEnt)
             ,c(CV_MdonativosTot,CV_MdonativosEnt),c(CV_MremesasTot,CV_MremesasEnt),c(CV_Mbene_gobTot,CV_Mbene_gobEnt),c(CV_Mtransf_hogTot,CV_Mtransf_hogEnt),c(CV_Mtrans_instTot,CV_Mtrans_instEnt)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquEnt),c(CV_Motros_ingTot,CV_Motros_ingEnt))

##### LIMITE INFERIOR AL 90%
c_ent_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corEnt),c(LI_MingtrabTot,LI_MingtrabEnt),c(LI_MtrabajoTot,LI_MtrabajoEnt),
             c(LI_MnegocioTot,LI_MnegocioEnt),c(LI_Motros_trabTot,LI_Motros_trabEnt),c(LI_MrentasTot,LI_MrentasEnt),c(LI_MutilidadTot,LI_MutilidadEnt),c(LI_MarrendaTot,LI_MarrendaEnt)
             ,c(LI_MtransferTot,LI_MtransferEnt),c(LI_MjubilacionTot,LI_MjubilacionEnt),c(LI_MbecasTot,LI_MbecasEnt),c(LI_MdonativosTot,LI_MdonativosEnt)
             ,c(LI_MremesasTot,LI_MremesasEnt),c(LI_Mbene_gobTot,LI_Mbene_gobEnt),c(LI_Mtransf_hogTot,LI_Mtransf_hogEnt),c(LI_Mtrans_instTot,LI_Mtrans_instEnt)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquEnt),c(LI_Motros_ingTot,LI_Motros_ingEnt))

### LIMITE SUPERIOR AL 90%
c_ent_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corEnt),c(LS_MingtrabTot,LS_MingtrabEnt),c(LS_MtrabajoTot,LS_MtrabajoEnt),c(LS_MnegocioTot,LS_MnegocioEnt)
             ,c(LS_Motros_trabTot,LS_Motros_trabEnt),c(LS_MrentasTot,LS_MrentasEnt),c(LS_MutilidadTot,LS_MutilidadEnt),c
             (LS_MarrendaTot,LS_MarrendaEnt),c(LS_MtransferTot,LS_MtransferEnt),c(LS_MjubilacionTot,LS_MjubilacionEnt),c(LS_MbecasTot,LS_MbecasEnt),c(
               LS_MdonativosTot,LS_MdonativosEnt),c(LS_MremesasTot,LS_MremesasEnt),c(LS_Mbene_gobTot,LS_Mbene_gobEnt),c(LS_Mtransf_hogTot,LS_Mtransf_hogEnt),c(LS_Mtrans_instTot,LS_Mtrans_instEnt)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquEnt),c(LS_Motros_ingTot,LS_Motros_ingEnt))

# se agregan los nombres de las entidades a las filas
#esta cadena está bien loca, no?
row.names(c_ent_ES)<-row.names(c_ent_SE)<-row.names(c_ent_CV)<-row.names(c_ent_LI)<-row.names(c_ent_LS)<-Entidades

#ahora vamos a ponerle nombre a las columnas
names(c_ent_ES)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_SE)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_CV)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_LI)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variación redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla #####
round(c_ent_ES)
round(c_ent_SE)
round(c_ent_CV,4)*100
round(c_ent_LI)
round(c_ent_LS)

#####################################################################################################
####################################################################################################
##################################################################################################

#####################################################################################################
########################  Fuentes de ingreso por decil para todo el país ###########################
###################################################################################################

mydesign <- svydesign(id=~upm,strata=~est_dis,data=BD1,weights=~factor)

#vamos por el ingreso corriente total del país
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notesé que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aquí cmabia la función a svyby, en by va el decil que creamos.
#y al final va la función que queremos
Ming_corDECIL <- svyby(~ing_cor,denominator=~Nhog,by=~DECIL,mydesign,svyratio)

############################################################################################################
###################################        Trabajo       #######################################
############################################################################################################
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~ingtrab,denominator=~Nhog,mydesign) # Total promedio
MingtrabDECIL <- svyby(~ingtrab,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~trabajo,denominator=~Nhog,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~trabajo,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~negocio,denominator=~Nhog,mydesign) # Total promedio
MnegocioDECIL <- svyby(~negocio,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~otros_trab,denominator=~Nhog,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~otros_trab,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil

############################################################################################################
###################################        Rentas de la propiedad      #######################################
############################################################################################################

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~rentas,denominator=~Nhog,mydesign) # Total promedio
MrentasDECIL <- svyby(~rentas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~utilidad,denominator=~Nhog,mydesign) # Total promedio
MutilidadDECIL <- svyby(~utilidad,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~arrenda,denominator=~Nhog,mydesign) # Total promedio
MarrendaDECIL <- svyby(~arrenda,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil

############################################################################################################
###################################        Transferencias   #######################################
#####################################################################################################

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~transfer,denominator=~Nhog,mydesign) # Total promedio
MtransferDECIL <- svyby(~transfer,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibió jubilaciones. Así que puede ser públicas o privadas.

MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~jubilacion,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, públicas privadas. 
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~becas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que también pueden ser públicos o privados.
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~donativos,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. Así de manera genérica.
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~remesas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aquí estna los programas públicos. Prospera, procampo, 65 y más, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~bene_gob,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~transf_hog,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione spúblicas o privadas.
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~trans_inst,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

### estim_alqu ### Aparentemente se le pregunta al entrevistado cuánto constaría la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~estim_alqu,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

### otros_ing ### es literalmente ¿algo más?
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~otros_ing,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil


#####################################################################################################
######################################### Estimaciones #############################################
##################################################################################################

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aquí es extraer el valor de la primera columa que corresponde al cálculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los cálculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Estándar ##########
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variación ##########
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)
########## Limite inferior ##########
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
                                                            ]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior ##########
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]


####################################################################################################
#############################      Cuadros       ##################################################
##################################################################################################

#este cuadro, lo único que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena está bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_DECIL_SE)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_DECIL_CV)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_DECIL_LI)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_DECIL_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variación redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla #####
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

#####################################################################################################
####################################################################################################
##################################################################################################
#####################################################################################################
#########################  Fuentes de ingreso por decil por entidad #################################
###################################################################################################

#####################################################################################################
#####################################  Aguascalientes #############################################
###################################################################################################

library(tidyverse)

#vamos a crear una base de datos solo para aguascalientes

BD1<-BD1%>%
  mutate(entidad=as.numeric(entidad))


Aguascalientes<-BD1%>%
  filter(entidad==1)


mydesign <- svydesign(id=~upm,strata=~est_dis,data=Aguascalientes,weights=~factor)

#vamos por el ingreso corriente total del estado
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 59,346.
#notesé que esto no es otra cosa que el ing_cor*factor/total de los hogares que hay en aguascalientes
Ming_corTotAguascalientes <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aquí cmabia la función a svyby, en by va el decil que creamos.
#y al final va la función que queremos
Ming_corDecil <- svyby(~ing_cor,denominator=~Nhog,by=~DECIL,mydesign,svyratio)

