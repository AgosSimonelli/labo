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

head(dataset)


dtrain  <- dataset[foto_mes == 202103]
dapply  <- dataset[foto_mes == 202105]



nuevo_train <- select(dtrain,clase_ternaria, foto_mes, mcuenta_corriente, mcuentas_saldo, mcuenta_debitos_automaticos , ccajas_otras , Master_mfinanciacion_limite , Master_Finiciomora, Master_fultimo_cierre , Visa_Finiciomora , Visa_madelantodolares , Visa_fultimo_cierre , Visa_mpagado , Visa_mpagosdolares)

nuevo_dapply <- select(dapply,clase_ternaria, foto_mes, mcuenta_corriente, mcuentas_saldo, mcuenta_debitos_automaticos , ccajas_otras , Master_mfinanciacion_limite , Master_Finiciomora, Master_fultimo_cierre , Visa_Finiciomora , Visa_madelantodolares , Visa_fultimo_cierre , Visa_mpagado , Visa_mpagosdolares)


#dim(nuevods)
#nuevods$mcuenta_corriente
#nuevods$mcuentas_saldo

#----------- RANKEO ---------------

mis_variables <- c("mcuenta_corriente", "mcuentas_saldo", "mcuenta_debitos_automaticos" , "ccajas_otras" , "Master_mfinanciacion_limite" , "Master_Finiciomora", "Master_fultimo_cierre" , "Visa_Finiciomora" , "Visa_madelantodolares" , "Visa_fultimo_cierre" , "Visa_mpagado" , "Visa_mpagosdolares")

prefix <- "r_"
for (var in mis_variables) {
  nuevo_train[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
  nuevo_dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
}


dim(nuevo_train)
dim(nuevo_dapply)


#----Borro las Variables originales

nuevo_train [ , ':=' (mcuenta_corriente=NULL , mcuentas_saldo=NULL , mcuenta_debitos_automaticos=NULL , ccajas_otras=NULL , Master_mfinanciacion_limite=NULL , Master_Finiciomora=NULL, Master_fultimo_cierre=NULL , Visa_Finiciomora=NULL , Visa_madelantodolares=NULL , Visa_fultimo_cierre=NULL , Visa_mpagado=NULL , Visa_mpagosdolares=NULL)]
nuevo_dapply [ , ':=' (mcuenta_corriente=NULL , mcuentas_saldo=NULL , mcuenta_debitos_automaticos=NULL , ccajas_otras=NULL , Master_mfinanciacion_limite=NULL , Master_Finiciomora=NULL, Master_fultimo_cierre=NULL , Visa_Finiciomora=NULL , Visa_madelantodolares=NULL , Visa_fultimo_cierre=NULL , Visa_mpagado=NULL , Visa_mpagosdolares=NULL)]

dim(nuevo_train)
dim(nuevo_dapply)


nuevodf <- merge(nuevo_train,nuevo_dapply, all=TRUE)

dim(nuevodf)

#grabo el dataset
fwrite( nuevodf,
        "dataset_variables_DD.csv.gz",
        logical01= TRUE,
        sep= "," )

