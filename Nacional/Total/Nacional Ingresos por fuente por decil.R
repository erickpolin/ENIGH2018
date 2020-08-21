############################################################################
###########  Nacional Ingresos por fuente por decil #####################
#########################################################################

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018")
Conc<-read.dbf("Conc.dbf",as.is = T)

mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc,weights=~factor)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
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
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~jubilacion,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~becas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~donativos,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~remesas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~bene_gob,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~transf_hog,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~trans_inst,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~estim_alqu,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~otros_ing,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil


#####################################################################################################
######################################### Estimaciones #############################################
##################################################################################################

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

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

########## Error Est?ndar ##########
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

########## Coeficiente de variaci?n ##########
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

#este cuadro, lo ?nico que tiene son todas la estimaciones.
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
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_SE)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_CV)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LI)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LS)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla #####
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/nacional")

write.dbf(c_DECIL_ES,file = "Nacional Ingresos por fuente por DECIL estimaciones.dbf")
write.dbf(c_DECIL_SE,file = "Nacional Ingresos por fuente por DECIL errores standard.dbf")
write.dbf(c_DECIL_CV,file = "Nacional Ingresos por fuente por DECIL CV.dbf")
write.dbf(c_DECIL_LI,file = "Nacional Ingresos por fuente por DECIL LI.dbf")
write.dbf(c_DECIL_ES,file = "Nacional Ingresos por fuente por DECIL LS.dbf")

