#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")
require("BurStMisc")

#install.packages("BurStMisc")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Administrator\\Desktop\\Maestria\\EyF\\")

# Poner sus semillas
semillas <- c(238001, 257687, 564227, 785501, 956341)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")

#dtrain  <- dataset[foto_mes == 202103]
#dapply  <- dataset[foto_mes == 202105]

## ---------------------------

dataset[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]
#dapply[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]


dataset[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
#dapply[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]


dataset[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
#dapply[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]

dataset[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]
#dapply[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]



dataset[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
#dapply[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]

dataset[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]
#dapply[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]


dataset[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
#dapply[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]


dataset[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
#dapply[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]


dataset[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
#dapply[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]


dataset[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]
#dapply[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]


dataset[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]
#dapply[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]


dataset[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
#dapply[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]

dataset [ , ':=' (cforex_buy=NULL , cforex_sell=NULL, ccomisiones_mantenimiento=NULL, ccomisiones_otras=NULL, cprestamos_personales=NULL , cprestamos_prendarios=NULL, cprestamos_hipotecarios=NULL , mprestamos_personales=NULL, mprestamos_prendarios=NULL, mprestamos_hipotecarios=NULL , Master_mlimitecompra=NULL, Visa_mlimitecompra=NULL , Master_cconsumos=NULL, Visa_cconsumos=NULL , Master_mconsumospesos=NULL , Visa_mconsumospesos=NULL, cseguro_vida=NULL , cseguro_auto=NULL, cseguro_vivienda=NULL, cseguro_accidentes_personales=NULL, cpagodeservicios=NULL, cpagomiscuentas=NULL, mpagodeservicios=NULL,  mpagomiscuentas=NULL, ctarjeta_visa_descuentos=NULL, ctarjeta_master_descuentos=NULL,  mtarjeta_visa_descuentos=NULL, mtarjeta_master_descuentos=NULL)]

#borrar <- c("ccomisiones_mantenimiento", "ccomisiones_otras", "cprestamos_personales" , "cprestamos_prendarios", "cprestamos_hipotecarios" , "mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios" , "Master_mlimitecompra", "Visa_mlimitecompra" ,"Master_cconsumos", "Visa_cconsumos" ,"Master_mconsumospesos" ,"Visa_mconsumospesos", "cseguro_vida" , "cseguro_auto", "cseguro_vivienda", "cseguro_accidentes_personales", "cpagodeservicios", "cpagomiscuentas", "mpagodeservicios",  "mpagomiscuentas","ctarjeta_visa_descuentos", "ctarjeta_master_descuentos",  "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos")
#dataset <- dataset[ , !(names(dataset) %in% borrar)]

dataset


write.csv(dataset,"dataset_competencia2_modificado.csv")

