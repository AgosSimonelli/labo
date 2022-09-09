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

# Poner sus semillas
semillas <- c(238001, 257687, 564227, 785501, 956341)

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


#----------------------------




#------------------------FE---------------

dtrain[, porcentaje_limite_visa := Visa_mlimitecompra/mpayroll]
dapply[, porcentaje_limite_visa := Visa_mlimitecompra/mpayroll]


dtrain[, cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
dapply[, cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]


dtrain[, mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
dapply[, mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]

dtrain[, limite_total  := master_mlimitecompra+visa_mlimitecompra]
dapply[, limite_total  := master_mlimitecompra+visa_mlimitecompra]



dtrain[, ccompras_total_tj  := master_cconsumos+visa_cconsumos]
dapply[, ccompras_total_tj  := master_cconsumos+visa_cconsumos]

dtrain[, mcompras_total_tj  := master_mconsumospesos+visa_mconsumospesos]
dapply[, mcompras_total_tj  := master_mconsumospesos+visa_mconsumospesos]


dtrain[, cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
dapply[, cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]


dtrain[, cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
dapply[, cpagos_servicios  := cpagodeservicios+cpagomiscuentas]


dtrain[, mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
dapply[, mpagos_servicios  := mpagodeservicios+mpagomiscuentas]



## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

#"ctrx_quarter",
# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mpasivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente",
                   "mrentabilidad",
                   "mautoservicio",
                   "mtarjeta_visa_consumo",
                   "mtarjeta_master_consumo",
                   #"mprestamos_personales",
                   #"mprestamos_hipotecarios",
                   "mpayroll",
                   "mttarjeta_visa_debitos_automaticos",
                   "mpagomiscuentas",
                   "mtransferencias_recibidas",
                   "mtransferencias_emitidas",
                   "Master_mfinanciacion_limite",
                   "Master_msaldototal",
                   "Master_msaldopesos",
                   #"Master_mconsumospesos",
                   #"Master_mlimitecompra",
                   "Master_mpagospesos",
                   "Master_mconsumototal",
                   "Master_mpagominimo",
                   "Visa_mfinanciacion_limite",
                   "Visa_msaldototal",
                   "Visa_msaldopesos",
                   "Visa_mconsumospesos",
                   #"Visa_mlimitecompra",
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


#----------------------------------------------------


# Vamos a probar que tal anda un modelo con las clases combinadas 
set.seed(semillas[1])
in_training <- caret::createDataPartition(dtrain$clase_binaria2,
                                          p = 0.70, list = FALSE)
dtrain2  <-  dtrain[in_training, ]
dtest   <-  dtrain[-in_training, ]


# Usamos parámetros ~~robados~~ sacados de los scripts de Gustavo.
parametros <- list(cp = -1, minsplit = 1073, minbucket = 278, maxdepth = 9)
# MUY IMPORTANTE: No estamos sacando los otros targets del dataset, hay
# que sacarlos en la fórmula como esta debajo.

modelo_bin2 <- rpart(clase_binaria2 ~ . - clase_ternaria  -mcomisiones_mantenimiento -Visa_mpagado -numero_de_cliente -mprestamos_personales -ctrx_quarter -mcuentas_saldo -mactivos_margen -mpasivos_margen -mcaja_ahorro -mcuenta_corriente -mrentabilidad -mautoservicio -mtarjeta_visa_consumo -mtarjeta_master_consumo -mpayroll -mttarjeta_visa_debitos_automaticos -mpagomiscuentas -mtransferencias_recibidas -mtransferencias_emitidas -Master_mfinanciacion_limite -Master_msaldototal -Master_msaldopesos -Master_mconsumospesos -Master_mlimitecompra -Master_mpagospesos -Master_mconsumototal -Master_mpagominimo -Visa_mfinanciacion_limite -Visa_msaldototal -Visa_msaldopesos -Visa_mconsumospesos -Visa_mlimitecompra -Visa_mpagospesos -Visa_mconsumototal -Visa_mpagominimo -mrentabilidad_annual -cprestamos_personales -cprestamos_prendarios -cprestamos_hipotecarios -mprestamos_prendarios -mprestamos_personales -mprestamos_hipotecarios,
                     data = dtrain2,
                     xval = 0,
                     control = parametros)

# Y calculamos la ganancia
pred_testing <- predict(modelo_bin2, dtest, type = "prob")
print(sum(
  (pred_testing[, "evento"] >= 0.0419) * ifelse(
    # Usamos la clase ternaria para calcular la gan
    dtest$clase_ternaria == "BAJA+2",
    78000,
    -2000) / 0.3
)
)


print(modelo_bin2$variable.importance)


#--------------------------------



## ---------------------------
## Step 12: Optimización 
## ---------------------------
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.0419) * ifelse(clase == "evento", 78000, -2000))
  )
}


# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart_ganancia <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria2 ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "evento"], test$clase_binaria2) / 0.3
}


# AGREGADO - Una funcion auxiliar para los experimentos

experimento_rpart_completo <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dtrain2$clase_binaria2, p = 0.70,
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



set.seed(semillas[1])
obj_fun_md_ms_mb <- function(x) {
  experimento_rpart_completo(dtrain, semillas
                             , md = x$maxdepth
                             , ms = x$minsplit
                             , mb = floor(x$minsplit/2))
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb,
  par.set = makeParamSet(
    #makeNumericParam("cp", lower=-1, upper= 0),
    makeIntegerParam("maxdepth",  lower = 4L, upper = 15L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 2000L),
    makeNumericParam("minbucket",  lower = 50L, upper = 1000L)
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


#Recommended parameters:
#  maxdepth=15; minsplit=907; minbucket=962
#Objective: y = 221884000.000
#...............................................

# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria2 ~ . -clase_ternaria -mpagodeservicios-mpagomiscuentas  -cpagodeservicios -cpagomiscuentas -cseguro_vida -cseguro_auto -cseguro_vivienda -cseguro_accidentes_personales -ctarjeta_debito -master_cconsumos -visa_cconsumos -mcomisiones_mantenimiento -Visa_mpagado -numero_de_cliente -mprestamos_personales -ctrx_quarter -mcuentas_saldo -mactivos_margen -mpasivos_margen -mcaja_ahorro -mcuenta_corriente -mrentabilidad -mautoservicio -mtarjeta_visa_consumo -mtarjeta_master_consumo -mpayroll -mttarjeta_visa_debitos_automaticos -mpagomiscuentas -mtransferencias_recibidas -mtransferencias_emitidas -Master_mfinanciacion_limite -Master_msaldototal -Master_msaldopesos -Master_mconsumospesos -Master_mlimitecompra -Master_mpagospesos -Master_mconsumototal -Master_mpagominimo -Visa_mfinanciacion_limite -Visa_msaldototal -Visa_msaldopesos -Visa_mconsumospesos -Visa_mlimitecompra -Visa_mpagospesos -Visa_mconsumototal -Visa_mpagominimo -mrentabilidad_annual -cprestamos_personales -cprestamos_prendarios -cprestamos_hipotecarios -mprestamos_prendarios -mprestamos_personales -mprestamos_hipotecarios -master_mlimitecompra -visa_mlimitecompra",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.54, #  -0.89
                 minsplit=  1073,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     9 )  #  12



print(modelo$variable.importance)

#vector_importantes <- names( modelo$variable.importance )
#vector_importantes

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
dir.create( "./exp/KA5004_FE" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA5004_FE/K5004_003.csv",
        sep=  "," )





