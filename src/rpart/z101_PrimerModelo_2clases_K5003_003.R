#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Administrator\\Desktop\\Maestria\\EyF\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")


# Armamos diferentes clases binarias:
# Sólo es evento las clase BAJA+2
#dataset[, clase_binaria1 := ifelse(
#  clase_ternaria == "BAJA+2",
#  "BAJA+2",
#  "noevento"
#)]


# Clases BAJAS+1 y BAJA+2 combinadas
dataset[, clase_binaria2 := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento"
)]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


#------------------------FE---------------




## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

#"ctrx_quarter",
# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("mprestamos_personales",
                   "ctrx_quarter",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mpasivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente",
                   "mrentabilidad",
                   "mautoservicio",
                   "mtarjeta_visa_consumo",
                   "mtarjeta_master_consumo",
                   "mprestamos_personales",
                   "mprestamos_hipotecarios",
                   "mpayroll",
                   "mttarjeta_visa_debitos_automaticos",
                   "mpagomiscuentas",
                   "mtransferencias_recibidas",
                   "mtransferencias_emitidas",
                   "Master_mfinanciacion_limite",
                   "Master_msaldototal",
                   "Master_msaldopesos",
                   "Master_mconsumospesos",
                   "Master_mlimitecompra",
                   "Master_mpagospesos",
                   "Master_mconsumototal",
                   "Master_mpagominimo",
                   "Visa_mfinanciacion_limite",
                   "Visa_msaldototal",
                   "Visa_msaldopesos",
                   "Visa_mconsumospesos",
                   "Visa_mlimitecompra",
                   "Visa_mpagospesos",
                   "Visa_mconsumototal",
                   "Visa_mpagominimo",
                   "mrentabilidad_annual")

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
  dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
}

dtrain$r_ctrx_quarter

summary(dtrain)

#----------------------------



# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria2 ~ .-clase_ternaria -mcomisiones_mantenimiento -Visa_mpagado -numero_de_cliente -mprestamos_personales -ctrx_quarter -mcuentas_saldo -mactivos_margen -mpasivos_margen -mcaja_ahorro -mcuenta_corriente -mrentabilidad -mautoservicio -mtarjeta_visa_consumo -mtarjeta_master_consumo -mprestamos_personales               -mprestamos_hipotecarios -mpayroll -mttarjeta_visa_debitos_automaticos -mpagomiscuentas -mtransferencias_recibidas -mtransferencias_emitidas -Master_mfinanciacion_limite -Master_msaldototal -Master_msaldopesos -Master_mconsumospesos -Master_mlimitecompra -Master_mpagospesos            -Master_mconsumototal -Master_mpagominimo -Visa_mfinanciacion_limite -Visa_msaldototal -Visa_msaldopesos -Visa_mconsumospesos -Visa_mlimitecompra -Visa_mpagospesos -Visa_mconsumototal -Visa_mpagominimo -mrentabilidad_annual",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.54,#  -0.89
                 minsplit=  1073,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     9 )  #  12


print(modelo$variable.importance)


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "evento"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 0.0419) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA503_FE" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA5003_FE/K503_003.csv",
        sep=  "," )

