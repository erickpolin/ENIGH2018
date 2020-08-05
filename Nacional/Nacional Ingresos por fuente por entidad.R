############################################################################
###########  Nacional Ingresos por fuente por entidad #####################
#########################################################################

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/2018")
Conc<-read.dbf("Conc.dbf",as.is = T)

#Definimos un vector con la entidad federativa
Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur",
             "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "M?xico", "Durango",
             "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de M?xico", "Michoac?n de Ocampo",
             "Morelos", "Nayarit", "Nuevo Le?n", "Oaxaca", "Puebla", "Quer?taro", "Quintana Roo", "San Luis Potos?",
             "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucat?n",
             "Zacatecas")


mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc,weights=~factor)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 


#ahora, vamos a hacer lo mismo por entidad
#aqu? cmabia la funci?n a svyby, en by va la entidad que creamos.
#y al final va la funci?n que queremos
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
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionEnt <- svyby(~jubilacion,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasEnt <- svyby(~becas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosEnt <- svyby(~donativos,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasEnt <- svyby(~remesas,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobEnt <- svyby(~bene_gob,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogEnt <- svyby(~transf_hog,denominator=~Nhog,by=~entidad ,mydesign,svyratio)

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instEnt <- svyby(~trans_inst,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional

### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquEnt <- svyby(~estim_alqu,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio

### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingEnt <- svyby(~otros_ing,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional promedio


#####################################################################################################
######################################### Estimaciones #############################################
##################################################################################################

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corEnt <- Ming_corEnt[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna
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

########## Error Est?ndar ##########
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

########## Coeficiente de variaci?n ##########
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

#este cuadro, lo ?nico que tiene son todas la estimaciones.
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
#esta cadena est? bien loca, no?
row.names(c_ent_ES)<-row.names(c_ent_SE)<-row.names(c_ent_CV)<-row.names(c_ent_LI)<-row.names(c_ent_LS)<-Entidades

#ahora vamos a ponerle nombre a las columnas
names(c_ent_ES)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_SE)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_CV)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_LI)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_ent_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla #####
round(c_ent_ES)
round(c_ent_SE)
round(c_ent_CV,4)*100
round(c_ent_LI)
round(c_ent_LS)

#ahora vamos a guardar los cuadros
write.dbf(c_ent_ES,file = "Nacional Ingresos por fuente por entidad estimaciones.dbf")
write.dbf(c_ent_SE,file = "Nacional Ingresos por fuente por entidad errores standard.dbf")
write.dbf(c_ent_CV,file = "Nacional Ingresos por fuente por entidad CV.dbf")
write.dbf(c_ent_LI,file = "Nacional Ingresos por fuente por entidad LI.dbf")
write.dbf(c_ent_ES,file = "Nacional Ingresos por fuente por entidad LS.dbf")
