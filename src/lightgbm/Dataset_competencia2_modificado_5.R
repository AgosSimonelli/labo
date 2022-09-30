#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")
require("dplyr")

#En este script hago las mismas variables que para la C1 y a diferencia del modificado_4
#(en ese hice rankeo y borre las variables con DF y me dio muy mal en kaggle, OJO AHI EL RANKEO
#LO HABIA HECHO SOBRE TODO EL DS PERO DEBERIA SER SOLO SOBRE MARZO Y  MAYO!!), en este 
#voy a hacer el rankeo solo en marzo y mayo y borro las variables que tienen DD
#script .modificado_3 --> ahi no hice rankeo y solo borre las que tenian DataDrifting)

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Administrator\\Desktop\\Maestria\\EyF\\")

# Poner sus semillas
semillas <- c(238001, 257687, 564227, 785501, 956341)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")



dtrain  <- dataset[foto_mes == 202103]
dapply  <- dataset[foto_mes == 202105]

## ---------------------------

dtrain[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]
dapply[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]


dtrain[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
dapply[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]


dtrain[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
dapply[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]

dtrain[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]
dapply[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]



dtrain[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
dapply[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]

dtrain[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]
dapply[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]


dtrain[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
dapply[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]


dtrain[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
dapply[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]


dtrain[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
dapply[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]


dtrain[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]
dapply[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]


dtrain[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]
dapply[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]


dtrain[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
dapply[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]


#variable extraida de una tesis de maestria de Irlanda
dtrain[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]
dapply[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

#--------------------


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
                   "mpayroll",
                   "mttarjeta_visa_debitos_automaticos",
                   "mpagomiscuentas",
                   "mtransferencias_recibidas",
                   "mtransferencias_emitidas",
                   "Master_mfinanciacion_limite",
                   "Master_msaldototal",
                   "Master_msaldopesos",
                   "Master_mpagospesos",
                   "Master_mconsumototal",
                   "Master_mpagominimo",
                   "Visa_mfinanciacion_limite",
                   "Visa_msaldototal",
                   "Visa_msaldopesos",
                   "Visa_mconsumospesos",
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




#---------------------
#Borro estas variables porque las uni

dtrain [ , ':=' ( ccomisiones_mantenimiento=NULL, ccomisiones_otras=NULL, cprestamos_personales=NULL , cprestamos_prendarios=NULL, cprestamos_hipotecarios=NULL , mprestamos_personales=NULL, mprestamos_prendarios=NULL, mprestamos_hipotecarios=NULL , Master_mlimitecompra=NULL, Visa_mlimitecompra=NULL , Master_cconsumos=NULL, Visa_cconsumos=NULL , Master_mconsumospesos=NULL , Visa_mconsumospesos=NULL, cseguro_vida=NULL , cseguro_auto=NULL, cseguro_vivienda=NULL, cseguro_accidentes_personales=NULL, cpagodeservicios=NULL, cpagomiscuentas=NULL, mpagodeservicios=NULL,  mpagomiscuentas=NULL, ctarjeta_visa_descuentos=NULL, ctarjeta_master_descuentos=NULL,  mtarjeta_visa_descuentos=NULL, mtarjeta_master_descuentos=NULL)]
dapply [ , ':=' ( ccomisiones_mantenimiento=NULL, ccomisiones_otras=NULL, cprestamos_personales=NULL , cprestamos_prendarios=NULL, cprestamos_hipotecarios=NULL , mprestamos_personales=NULL, mprestamos_prendarios=NULL, mprestamos_hipotecarios=NULL , Master_mlimitecompra=NULL, Visa_mlimitecompra=NULL , Master_cconsumos=NULL, Visa_cconsumos=NULL , Master_mconsumospesos=NULL , Visa_mconsumospesos=NULL, cseguro_vida=NULL , cseguro_auto=NULL, cseguro_vivienda=NULL, cseguro_accidentes_personales=NULL, cpagodeservicios=NULL, cpagomiscuentas=NULL, mpagodeservicios=NULL,  mpagomiscuentas=NULL, ctarjeta_visa_descuentos=NULL, ctarjeta_master_descuentos=NULL,  mtarjeta_visa_descuentos=NULL, mtarjeta_master_descuentos=NULL)]


#Borro estas variables porque le hice rankeo
dtrain [ , ':=' (ctrx_quarter=NULL , mcuentas_saldo=NULL , mactivos_margen=NULL , mpasivos_margen=NULL , mcaja_ahorro=NULL , mcuenta_corriente=NULL , mrentabilidad=NULL , mautoservicio=NULL , mtarjeta_visa_consumo=NULL , mtarjeta_master_consumo=NULL , mpayroll=NULL , mttarjeta_visa_debitos_automaticos=NULL , mpagomiscuentas=NULL , mtransferencias_recibidas=NULL  , mtransferencias_emitidas=NULL , Master_mfinanciacion_limite=NULL , Master_msaldototal=NULL , Master_msaldopesos=NULL , Master_mpagospesos =NULL , Master_mconsumototal=NULL , Master_mpagominimo=NULL ,Visa_mfinanciacion_limite=NULL ,  Visa_msaldototal=NULL  , Visa_msaldopesos=NULL , Visa_mconsumospesos=NULL , Visa_mpagospesos=NULL , Visa_mconsumototal=NULL , Visa_mpagominimo=NULL , mrentabilidad_annual=NULL )]  
dapply [ , ':=' (ctrx_quarter=NULL , mcuentas_saldo=NULL , mactivos_margen=NULL , mpasivos_margen=NULL , mcaja_ahorro=NULL , mcuenta_corriente=NULL , mrentabilidad=NULL , mautoservicio=NULL , mtarjeta_visa_consumo=NULL , mtarjeta_master_consumo=NULL , mpayroll=NULL , mttarjeta_visa_debitos_automaticos=NULL , mpagomiscuentas=NULL , mtransferencias_recibidas=NULL  , mtransferencias_emitidas=NULL , Master_mfinanciacion_limite=NULL , Master_msaldototal=NULL , Master_msaldopesos=NULL , Master_mpagospesos =NULL , Master_mconsumototal=NULL , Master_mpagominimo=NULL ,Visa_mfinanciacion_limite=NULL ,  Visa_msaldototal=NULL  , Visa_msaldopesos=NULL , Visa_mconsumospesos=NULL , Visa_mpagospesos=NULL , Visa_mconsumototal=NULL , Visa_mpagominimo=NULL , mrentabilidad_annual=NULL )]  

 
#Borro estas variables por Data Drifting

dtrain [ , ':=' (mcuenta_corriente=NULL , mcuentas_saldo=NULL , mcuenta_debitos_automaticos=NULL , ccajas_otras=NULL , Master_mfinanciacion_limite=NULL , Master_Finiciomora=NULL ,Visa_mfinanciacion_limite=NULL , Visa_madelantodolares=NULL , Visa_fultimo_cierre=NULL , Visa_mpagado=NULL , Visa_mpagosdolares=NULL, mforex_sell=NULL )]
dapply [ , ':=' (mcuenta_corriente=NULL , mcuentas_saldo=NULL , mcuenta_debitos_automaticos=NULL , ccajas_otras=NULL , Master_mfinanciacion_limite=NULL , Master_Finiciomora=NULL ,Visa_mfinanciacion_limite=NULL , Visa_madelantodolares=NULL , Visa_fultimo_cierre=NULL , Visa_mpagado=NULL , Visa_mpagosdolares=NULL, mforex_sell=NULL )]



dim(dtrain)
dim(dapply)

nuevodf <- merge(dtrain,dapply, all=TRUE)

dim(nuevodf)

#grabo el dataset
fwrite( nuevodf,
        "dataset_competencia2_FE5.csv.gz",
        logical01= TRUE,
        sep= "," )


