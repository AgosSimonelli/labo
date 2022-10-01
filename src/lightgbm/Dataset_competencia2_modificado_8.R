#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")
require("dplyr")


# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Administrator\\Desktop\\Maestria\\EyF\\")

# Poner sus semillas
semillas <- c(238001, 257687, 564227, 785501, 956341)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")



dtrain  <- dataset[foto_mes == 202103]
dapply  <- dataset[foto_mes == 202105]


#summary(dataset$active_quarter)


## ---------------------------

dtrain[, f_mcomisiones_antiguedad := mcomisiones/cliente_antiguedad]
dapply[, f_mcomisiones_antiguedad := mcomisiones/cliente_antiguedad]

dtrain[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]
dapply[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]

dtrain[, f_edad_limite_visa := Visa_mlimitecompra/cliente_edad]
dapply[, f_edad_limite_visa := Visa_mlimitecompra/cliente_edad]

dtrain[, f_edad_limite_master := Master_mlimitecompra/cliente_edad]
dapply[, f_edad_limite_master := Master_mlimitecompra/cliente_edad]

dtrain[, f_total_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
dapply[, f_total_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]


dtrain[, f_total_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
dapply[, f_total_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]


dtrain[, f_total_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
dapply[, f_total_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]


dtrain[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
dapply[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]


dtrain[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
dapply[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]


dtrain[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra] #las dejo separas tambien
dapply[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra] #las dejo separadas tambien


dtrain[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
dapply[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]

dtrain[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos] #las dejo separas tambien
dapply[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos] #las dejo separas tambien

dtrain[, f_mcompras_usd_total_tj  := Master_mconsumosdolares+Visa_mconsumosdolares]
dapply[, f_mcompras_usd_total_tj  := Master_mconsumosdolares+Visa_mconsumosdolares]


dtrain[, f_total_adelanto_pesos  := Master_madelantopesos+Visa_madelantopesos]
dapply[, f_total_adelanto_pesos  := Master_madelantopesos+Visa_madelantopesos]


dtrain[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]
dapply[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]


dtrain[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]
dapply[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]


dtrain[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
dapply[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]


#variable extraida de una tesis de maestria de Irlanda
dtrain[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]
dapply[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]


dtrain[, f_mcompras_total_tj_edad  := f_mcompras_total_tj/cliente_edad]
dapply[, f_ccompras_total_tj_edad  := f_mcompras_total_tj/cliente_edad]

#--------------------
#dataset$ctrx_quarter

#"ctrx_quarter",
# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c(
                   "mcuentas_saldo", 
                   "mcuenta_corriente",
                   "mcuenta_debitos_automaticos",
                    "Master_mfinanciacion_limite",
                   "Master_Finiciomora",
                   "Master_fultimo_cierre",
                   "Master_mfinanciacion_limite",
                   "Visa_Finiciomora",
                   "Visa_madelantodolares",
                   "Visa_fultimo_cierre",
                   "Visa_mfinanciacion_limite",
                   "Visa_mpagado",
                   "Visa_mpagosdolares",
                   "ccajas_otras",
                   "mactivos_margen",
                   "mactivos_margen",
                   "mpasivos_margen",
                   "mcaja_ahorro",
                   "mrentabilidad",
                   "mautoservicio",
                   "mtarjeta_visa_consumo",
                   "mtarjeta_master_consumo",
                   #"mpayroll",
                   "mtransferencias_recibidas",
                   "mtransferencias_emitidas",
                   "Master_msaldototal",
                   "Master_msaldopesos",
                   "Master_mpagospesos",
                   "Master_mconsumototal",
                   "Visa_msaldototal",
                   "Visa_msaldopesos",
                   "Visa_mconsumospesos",
                   "Visa_mpagospesos",
                   "Visa_mconsumototal",
                   "mrentabilidad_annual")

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
  dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
}



#---------------------
#Borro estas variables porque las uni
#No borro las de prestamos individuales

dtrain [ , ':=' ( ccomisiones_mantenimiento=NULL, ccomisiones_otras=NULL,  Master_cconsumos=NULL, Visa_cconsumos=NULL , Master_madelantopesos=NULL, Visa_madelantopesos =NULL,  cseguro_vida=NULL , cseguro_auto=NULL, cseguro_vivienda=NULL, cseguro_accidentes_personales=NULL, cpagodeservicios=NULL, cpagomiscuentas=NULL, mpagodeservicios=NULL,  mpagomiscuentas=NULL, ctarjeta_visa_descuentos=NULL, ctarjeta_master_descuentos=NULL,  mtarjeta_visa_descuentos=NULL, mtarjeta_master_descuentos=NULL)]
dapply [ , ':=' ( ccomisiones_mantenimiento=NULL, ccomisiones_otras=NULL,  Master_cconsumos=NULL, Visa_cconsumos=NULL , Master_madelantopesos=NULL, Visa_madelantopesos =NULL, cseguro_vida=NULL , cseguro_auto=NULL, cseguro_vivienda=NULL, cseguro_accidentes_personales=NULL, cpagodeservicios=NULL, cpagomiscuentas=NULL, mpagodeservicios=NULL,  mpagomiscuentas=NULL, ctarjeta_visa_descuentos=NULL, ctarjeta_master_descuentos=NULL,  mtarjeta_visa_descuentos=NULL, mtarjeta_master_descuentos=NULL)]


#Borro estas variables porque le hice rankeo
dtrain [ , ':=' (mactivos_margen=NULL , mpasivos_margen=NULL , mcaja_ahorro=NULL , mrentabilidad=NULL , mautoservicio=NULL , mtarjeta_visa_consumo=NULL , mtarjeta_master_consumo=NULL , mtransferencias_recibidas=NULL  , mtransferencias_emitidas=NULL , Master_mfinanciacion_limite=NULL , Master_msaldototal=NULL , Master_msaldopesos=NULL , Master_mpagospesos =NULL , Master_mconsumototal=NULL , Visa_mfinanciacion_limite=NULL ,  Visa_msaldototal=NULL  , Visa_msaldopesos=NULL , Visa_mconsumospesos=NULL , Visa_mpagospesos=NULL , Visa_mconsumototal=NULL , mrentabilidad_annual=NULL )]  
dapply [ , ':=' (mactivos_margen=NULL , mpasivos_margen=NULL , mcaja_ahorro=NULL , mrentabilidad=NULL , mautoservicio=NULL , mtarjeta_visa_consumo=NULL , mtarjeta_master_consumo=NULL , mtransferencias_recibidas=NULL  , mtransferencias_emitidas=NULL , Master_mfinanciacion_limite=NULL , Master_msaldototal=NULL , Master_msaldopesos=NULL , Master_mpagospesos =NULL , Master_mconsumototal=NULL , Visa_mfinanciacion_limite=NULL ,  Visa_msaldototal=NULL  , Visa_msaldopesos=NULL , Visa_mconsumospesos=NULL , Visa_mpagospesos=NULL , Visa_mconsumototal=NULL , mrentabilidad_annual=NULL )]  

 
#Borro estas variables por Data Drifting - Me quedo con las rankeadas
#Visa_mfinanciacion_limite=NULL con esta tengo dudas, pruebo dejandola 

dtrain [ , ':=' (mcuenta_corriente=NULL , mcuentas_saldo=NULL , mcuenta_debitos_automaticos=NULL , ccajas_otras=NULL , Master_mfinanciacion_limite=NULL , Master_Finiciomora=NULL, Master_fultimo_cierre=NULL , Visa_Finiciomora=NULL , Visa_madelantodolares=NULL , Visa_fultimo_cierre=NULL , Visa_mpagado=NULL , Visa_mpagosdolares=NULL)]
dapply [ , ':=' (mcuenta_corriente=NULL , mcuentas_saldo=NULL , mcuenta_debitos_automaticos=NULL , ccajas_otras=NULL , Master_mfinanciacion_limite=NULL , Master_Finiciomora=NULL, Master_fultimo_cierre=NULL , Visa_Finiciomora=NULL , Visa_madelantodolares=NULL , Visa_fultimo_cierre=NULL , Visa_mpagado=NULL , Visa_mpagosdolares=NULL)]



dim(dtrain)
dim(dapply)

nuevodf <- merge(dtrain,dapply, all=TRUE)

dim(nuevodf)

#grabo el dataset
fwrite( nuevodf,
        "dataset_competencia2_FE8.csv.gz",
        logical01= TRUE,
        sep= "," )


