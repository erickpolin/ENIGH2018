###################  Creación de la base 2018 #####################
##################     Urbano    #########################


#ENIGH 2018
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018")
Conc<-read.dbf("concentradohogar.dbf",as.is = T)

#Keeping Variables of interest
Conc <- Conc [ c("folioviv", "foliohog","tot_integ", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis","tam_loc")]

################ DEfinir hogares in?genas 
Poblacion<-read.dbf("poblacion.dbf",as.is = T)

Poblacion <- Poblacion [ c("folioviv", "foliohog", "numren", "parentesco","hablaind","comprenind","etnia")]

#El concepto de hogar ind?gena se ha definido como aquel donde el jefe(a), 
#su c?nyuge o alguno de los ascendientes (madre o padre, madrastra o padrastro, abuelo(a),
#bisabuelo(a), tatarabuelo(a), suegro(a)) declararon hablar alguna lengua ind?gena.
parentescos<-c(101,102,201,202,203,204,205,601,602,606,607,608,615,616)

Poblacion<- Poblacion%>%
  mutate(HogarIndigena=ifelse(parentesco%in%parentescos&hablaind==1,1,0|parentesco%in%parentescos&comprenind==1|parentesco%in%parentescos&etnia==1))

HogaresIndigenas<-Poblacion %>%
  group_by(folioviv,foliohog)%>%
  summarize(HogarIndigena=mean(HogarIndigena))

HogaresIndigenas<-data.frame(HogaresIndigenas)

HogaresIndigenas<-HogaresIndigenas%>%
  mutate(HogarIndigena=ifelse(HogarIndigena>0,1,0))

prop.table(table(HogaresIndigenas$HogarIndigena))

Conc<-merge(Conc,HogaresIndigenas,by=c("folioviv","foliohog"))

########ya est?n los hogares ind?genas

#the fist two digits of "folioviv" makes reference to the state
#Let?s create a variable called entidad that contains thos two first digits of the "folioviv" variable
Conc$entidad<-substr(Conc$folioviv,1,2)

############vamos a deflactar
entidad<-c("01","02","03","04","05","06","07","08","09",
           "10","11","12","13","14","15","16","17","18","19","20",
           "21","22","23","24","25","26","27","28","29","30","31","32")

Deflactores<-c(99.72681114, 100.7448332, 100.577354, 100.0405632, 100.3536931, 99.47579091,
               99.97322672,100.1204269,99.6271963,99.47590234,99.55847446,99.59978154,
               99.64694471,99.75771694,99.55737165,99.60461493,99.6391153,99.25764758,99.97057884,
               99.78754729,99.9375229,99.32058978,100.4795674,99.81711761,102.0798977,101.0968882,
               100.0880301,100.3167204,99.76430073,99.83092717,99.7534199,99.6364563)

entidades<-c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
             "Colima","Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo",
             "Jalisco","México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
             "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")

Deflactores2008<-data.frame(entidad,entidades,Deflactores)

Conc<-merge(Conc,Deflactores2008,by=c("entidad"))

Conc <- Conc%>%
  mutate(ing_cor=(ing_cor/Deflactores)*100, ingtrab=(ingtrab/Deflactores)*100, trabajo=(trabajo/Deflactores)*100, 
         negocio=(negocio/Deflactores)*100, otros_trab=(otros_trab/Deflactores)*100, rentas=(rentas/Deflactores)*100,
         utilidad=(utilidad/Deflactores)*100,arrenda=(arrenda/Deflactores)*100, transfer=(transfer/Deflactores)*100,
         jubilacion=(jubilacion/Deflactores)*100, becas=(becas/Deflactores)*100, donativos=(donativos/Deflactores)*100,
         remesas=(remesas/Deflactores)*100, bene_gob=(bene_gob/Deflactores)*100, transf_hog=(transf_hog/Deflactores)*100, 
         trans_inst=(trans_inst/Deflactores)*100,estim_alqu=(estim_alqu/Deflactores)*100, otros_ing=(otros_ing/Deflactores)*100)

Conc<-Conc%>%
  mutate(Small=ifelse(tam_loc==4,1,0))


Conc<-Conc %>%
  filter(Small==0)





#apparently this is a "flag", IDK what is this shit yet
Conc$Nhog <- 1

########################################## DECILES 

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

############################# Creating Deciles of Income 

Conc$MAXT<-Conc$ing_cor #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

Conc<-Conc[with(Conc, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

Conc$ACUMULA<-cumsum(Conc$factor) #aqu? creamos una variable de suma acumulada del factor de viviendas.



############################Ahora viene la creaci?n de los deciles

#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1,]$factor
  Conc<-rbind(Conc[1:(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),],
              Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1):dim(Conc[1])[1],])
  b1<-tam_dec*i-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
Conc$ACUMULA2<-cumsum(Conc$factor)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
Conc$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
Conc[(Conc$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  Conc[((Conc$ACUMULA2>tam_dec*i)&(Conc$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
Conc[Conc$DECIL%in%"0",]$DECIL<-10




write.dbf(Conc,file="Conc2018_urban.dbf")

rm(list=ls())


#################     Rural      ##################


#ENIGH 2018
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018")
Conc<-read.dbf("concentradohogar.dbf",as.is = T)

#Keeping Variables of interest
Conc <- Conc [ c("folioviv", "foliohog","tot_integ", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis","tam_loc")]

################ DEfinir hogares in?genas 
Poblacion<-read.dbf("poblacion.dbf",as.is = T)

Poblacion <- Poblacion [ c("folioviv", "foliohog", "numren", "parentesco","hablaind","comprenind","etnia")]

#El concepto de hogar ind?gena se ha definido como aquel donde el jefe(a), 
#su c?nyuge o alguno de los ascendientes (madre o padre, madrastra o padrastro, abuelo(a),
#bisabuelo(a), tatarabuelo(a), suegro(a)) declararon hablar alguna lengua ind?gena.
parentescos<-c(101,102,201,202,203,204,205,601,602,606,607,608,615,616)

Poblacion<- Poblacion%>%
  mutate(HogarIndigena=ifelse(parentesco%in%parentescos&hablaind==1,1,0|parentesco%in%parentescos&comprenind==1|parentesco%in%parentescos&etnia==1))

HogaresIndigenas<-Poblacion %>%
  group_by(folioviv,foliohog)%>%
  summarize(HogarIndigena=mean(HogarIndigena))

HogaresIndigenas<-data.frame(HogaresIndigenas)

HogaresIndigenas<-HogaresIndigenas%>%
  mutate(HogarIndigena=ifelse(HogarIndigena>0,1,0))

prop.table(table(HogaresIndigenas$HogarIndigena))

Conc<-merge(Conc,HogaresIndigenas,by=c("folioviv","foliohog"))

########ya est?n los hogares ind?genas

#the fist two digits of "folioviv" makes reference to the state
#Let?s create a variable called entidad that contains thos two first digits of the "folioviv" variable
Conc$entidad<-substr(Conc$folioviv,1,2)

############vamos a deflactar
entidad<-c("01","02","03","04","05","06","07","08","09",
           "10","11","12","13","14","15","16","17","18","19","20",
           "21","22","23","24","25","26","27","28","29","30","31","32")

Deflactores<-c(99.72681114, 100.7448332, 100.577354, 100.0405632, 100.3536931, 99.47579091,
               99.97322672,100.1204269,99.6271963,99.47590234,99.55847446,99.59978154,
               99.64694471,99.75771694,99.55737165,99.60461493,99.6391153,99.25764758,99.97057884,
               99.78754729,99.9375229,99.32058978,100.4795674,99.81711761,102.0798977,101.0968882,
               100.0880301,100.3167204,99.76430073,99.83092717,99.7534199,99.6364563)

entidades<-c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
             "Colima","Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo",
             "Jalisco","México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
             "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")

Deflactores2008<-data.frame(entidad,entidades,Deflactores)

Conc<-merge(Conc,Deflactores2008,by=c("entidad"))

Conc <- Conc%>%
  mutate(ing_cor=(ing_cor/Deflactores)*100, ingtrab=(ingtrab/Deflactores)*100, trabajo=(trabajo/Deflactores)*100, 
         negocio=(negocio/Deflactores)*100, otros_trab=(otros_trab/Deflactores)*100, rentas=(rentas/Deflactores)*100,
         utilidad=(utilidad/Deflactores)*100,arrenda=(arrenda/Deflactores)*100, transfer=(transfer/Deflactores)*100,
         jubilacion=(jubilacion/Deflactores)*100, becas=(becas/Deflactores)*100, donativos=(donativos/Deflactores)*100,
         remesas=(remesas/Deflactores)*100, bene_gob=(bene_gob/Deflactores)*100, transf_hog=(transf_hog/Deflactores)*100, 
         trans_inst=(trans_inst/Deflactores)*100,estim_alqu=(estim_alqu/Deflactores)*100, otros_ing=(otros_ing/Deflactores)*100)

Conc<-Conc%>%
  mutate(Small=ifelse(tam_loc==4,1,0))


Conc<-Conc %>%
  filter(Small==1)





#apparently this is a "flag", IDK what is this shit yet
Conc$Nhog <- 1

########################################## DECILES 

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

############################# Creating Deciles of Income 

Conc$MAXT<-Conc$ing_cor #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

Conc<-Conc[with(Conc, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

Conc$ACUMULA<-cumsum(Conc$factor) #aqu? creamos una variable de suma acumulada del factor de viviendas.



############################Ahora viene la creaci?n de los deciles

#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1,]$factor
  Conc<-rbind(Conc[1:(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),],
              Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1):dim(Conc[1])[1],])
  b1<-tam_dec*i-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
Conc$ACUMULA2<-cumsum(Conc$factor)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
Conc$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
Conc[(Conc$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  Conc[((Conc$ACUMULA2>tam_dec*i)&(Conc$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
Conc[Conc$DECIL%in%"0",]$DECIL<-10




write.dbf(Conc,file="Conc2018_rural.dbf")

