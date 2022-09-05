## Sobre Campos
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Genius is one percent inspiration and 99 percent perspiration
## --- ~~Thomas Edison~~ Kate Sanborn

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

require("ROCR")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Administrator\\Desktop\\Maestria\\EyF\\")  

# Poner sus semillas
semillas <- c(238001, 257687, 564227, 785501, 956341)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                                          p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]



calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                                 78000, -2000) / 0.3
  )
}



# Antes de empezar vamos a ver la importancia de variables
modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)


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
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
}

dtrain$r_ctrx_quarter

summary(dtrain)

#----------------------------



# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -numero_de_cliente -mprestamos_personales -ctrx_quarter -mcuentas_saldo -mactivos_margen -mpasivos_margen -mcaja_ahorro -mcuenta_corriente -mrentabilidad -mautoservicio -mtarjeta_visa_consumo -mtarjeta_master_consumo -mprestamos_personales               -mprestamos_hipotecarios -mpayroll -mttarjeta_visa_debitos_automaticos -mpagomiscuentas -mtransferencias_recibidas -mtransferencias_emitidas -Master_mfinanciacion_limite -Master_msaldototal -Master_msaldopesos -Master_mconsumospesos -Master_mlimitecompra -Master_mpagospesos            -Master_mconsumototal -Master_mpagominimo -Visa_mfinanciacion_limite -Visa_msaldototal -Visa_msaldopesos -Visa_mconsumospesos -Visa_mlimitecompra -Visa_mpagospesos -Visa_mconsumototal -Visa_mpagominimo -mrentabilidad_annual",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.54,#  -0.89
                 minsplit=  1073,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     9 )  #  12


print(modelo$variable.importance)


calcular_ganancia(modelo, dtest)


## ---------------------------
## Step 12: Optimización 
## ---------------------------

# AGREGADO - Una funcion auxiliar para los experimentos

experimento_rpart_completo <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    #train_sample <- tomar_muestra(train)
    r <- modelo_rpart_ganancia(train, test, 
                               cp = cp, ms = ms, mb = mb, md = md)
    gan <- c(gan, r)
  }
  mean(gan)
}

# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart_ganancia <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
}

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}


set.seed(semillas[1])
obj_fun_md_ms_mb <- function(x) {
  experimento_rpart_completo(dataset, semillas
                             , md = x$maxdepth
                             , ms = x$minsplit
                             , mb = floor(x$minsplit/2))
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb,
  par.set = makeParamSet(
    makeNumericParam("cp", lower=-1, upper= 0),
    makeIntegerParam("maxdepth",  lower = 4L, upper = 10L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 500L),
    makeNumericParam("minbucket",  lower = 50L, upper = 200L)
    # makeNumericParam <- para parámetros continuos
  ),
  noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 50L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)

run_md_ms

#Vuelvo a correr el modelo con los resultados de la Op.B. a ver si la ganancia da mejor:

modelo2  <- rpart(formula=   "clase_binaria ~ . -numero_de_cliente -mprestamos_personales -ctrx_quarter -mcuentas_saldo -mactivos_margen -mpasivos_margen -mcaja_ahorro -mcuenta_corriente -mrentabilidad -mautoservicio -mtarjeta_visa_consumo -mtarjeta_master_consumo -mprestamos_personales               -mprestamos_hipotecarios -mpayroll -mttarjeta_visa_debitos_automaticos -mpagomiscuentas -mtransferencias_recibidas -mtransferencias_emitidas -Master_mfinanciacion_limite -Master_msaldototal -Master_msaldopesos -Master_mconsumospesos -Master_mlimitecompra -Master_mpagospesos            -Master_mconsumototal -Master_mpagominimo -Visa_mfinanciacion_limite -Visa_msaldototal -Visa_msaldopesos -Visa_mconsumospesos -Visa_mlimitecompra -Visa_mpagospesos -Visa_mconsumototal -Visa_mpagominimo -mrentabilidad_annual",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.316,#  -0.89
                 minsplit=  430,   # 621
                 minbucket=  197,   # 309
                 maxdepth=    5 )  #  12


print(modelo2$variable.importance)


calcular_ganancia(modelo2, dtest)

#no arroja mejores resultados con los HP que propone, asi que me quedo con los anteriores.
#Esta es la salida:
#Recommended parameters:
#cp=-0.316; maxdepth=5; minsplit=430; minbucket=197
#Objective: y = 20314666.667

#> calcular_ganancia(modelo2, dtest)
#[1] 21573333
#
#
#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
# dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
# dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
# dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
# dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
# dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
# dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
# dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
# dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
# dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
# dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
# dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
# dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]

#---------------------------------------------------
#Hago de nuevo el FE sobre train y apply

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Administrator\\Desktop\\Maestria\\EyF\\")  

# Poner sus semillas
semillas <- c(238001, 257687, 564227, 785501, 956341)
#cargo de nuevo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv" )


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

# Borramos el target viejo
dataset[, clase_ternaria := NULL]

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# Hago la transformacion en TRAIN y en APPLY

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

#dtrain$r_ctrx_quarter
#summary(dtrain)

#----------------------------



# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo3  <- rpart(formula=   "clase_binaria ~ . -numero_de_cliente -mprestamos_personales -ctrx_quarter -mcuentas_saldo -mactivos_margen -mpasivos_margen -mcaja_ahorro -mcuenta_corriente -mrentabilidad -mautoservicio -mtarjeta_visa_consumo -mtarjeta_master_consumo -mprestamos_personales               -mprestamos_hipotecarios -mpayroll -mttarjeta_visa_debitos_automaticos -mpagomiscuentas -mtransferencias_recibidas -mtransferencias_emitidas -Master_mfinanciacion_limite -Master_msaldototal -Master_msaldopesos -Master_mconsumospesos -Master_mlimitecompra -Master_mpagospesos            -Master_mconsumototal -Master_mpagominimo -Visa_mfinanciacion_limite -Visa_msaldototal -Visa_msaldopesos -Visa_mconsumospesos -Visa_mlimitecompra -Visa_mpagospesos -Visa_mconsumototal -Visa_mpagominimo -mrentabilidad_annual",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.54,#  -0.89
                 minsplit=  1073,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     9 )  #  12


print(modelo3$variable.importance)




#---------------------------------------------------
#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo3,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(257687)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


dir.create( "./exp/" )
dir.create( "./exp/KA4120_10" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]
  
  
  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
          file= paste0( "./exp/KA4120_10/KA4120_0010_",  corte, ".csv"),
          sep=  "," )
}


