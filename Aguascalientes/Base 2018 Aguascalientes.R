####################################################################
###################  Creación de la base 2018 #####################
######################  Aguascalientes  #########################
################################################################

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
Conc <- Conc [ c("folioviv", "foliohog","tot_integ", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis")]

################ DEfinir hogares in?genas#################
Poblacion<-read.dbf("poblacion.dbf",as.is = T)

Poblacion <- Poblacion [ c("folioviv", "foliohog", "numren", "parentesco","hablaind","comprenind","etnia")]

#El concepto de hogar ind?gena se ha definido como aquel donde el jefe(a), 
#su c?nyuge o alguno de los ascendientes (madre o padre, madrastra o padrastro, abuelo(a),
#bisabuelo(a), tatarabuelo(a), suegro(a)) declararon hablar alguna lengua ind?gena.
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

########ya est?n los hogares ind?genas############

#the fist two digits of "folioviv" makes reference to the state
#Let?s create a variable called entidad that contains thos two first digits of the "folioviv" variable
Conc$entidad<-substr(Conc$folioviv,1,2)

ConcAguascalientes<-Conc%>%
  filter(entidad=="01")

remove(Conc)
remove(Poblacion)


######################################################

#apparently this is a "flag", IDK what is this shit yet
ConcAguascalientes$Nhog <- 1

########################################## DECILES ##########################################

#Attaching the data frame
attach(ConcAguascalientes) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to ing_cor, folioviv, foliohog
ConcAguascalientes<- orderBy (~+ing_cor+folioviv+foliohog, data=ConcAguascalientes) #this give us the households sorted by total income

#Adding the values of the expansion factor. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(ConcAguascalientes$factor,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/10) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
ConcAguascalientes$tam_dec<-tam_dec

############################# Creating Deciles of Income ###########################

ConcAguascalientes$MAXT<-ConcAguascalientes$ing_cor #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

ConcAguascalientes<-ConcAguascalientes[with(ConcAguascalientes, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

ConcAguascalientes$ACUMULA<-cumsum(ConcAguascalientes$factor) #aqu? creamos una variable de suma acumulada del factor de viviendas.


######################################################################################################
############################Ahora viene la creaci?n de los deciles###################################
######################################################################################################

#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-ConcAguascalientes[dim(ConcAguascalientes[ConcAguascalientes$ACUMULA<tam_dec*i,])[1]+1,]$factor
  ConcAguascalientes<-rbind(ConcAguascalientes[1:(dim(ConcAguascalientes[ConcAguascalientes$ACUMULA<tam_dec*i,])[1]+1),],
                            ConcAguascalientes[(dim(ConcAguascalientes[ConcAguascalientes$ACUMULA<tam_dec*i,])[1]+1):dim(ConcAguascalientes[1])[1],])
  b1<-tam_dec*i-ConcAguascalientes[dim(ConcAguascalientes[ConcAguascalientes$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  ConcAguascalientes[(dim(ConcAguascalientes[ConcAguascalientes$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  ConcAguascalientes[(dim(ConcAguascalientes[ConcAguascalientes$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
ConcAguascalientes$ACUMULA2<-cumsum(ConcAguascalientes$factor)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
ConcAguascalientes$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
ConcAguascalientes[(ConcAguascalientes$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  ConcAguascalientes[((ConcAguascalientes$ACUMULA2>tam_dec*i)&(ConcAguascalientes$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
ConcAguascalientes[ConcAguascalientes$DECIL%in%"0",]$DECIL<-10

setwd("C:/Users/Erick/Dropbox/GIC/2018/Aguascalientes")
write.dbf(ConcAguascalientes,file="ConcAguascalientes.dbf")

