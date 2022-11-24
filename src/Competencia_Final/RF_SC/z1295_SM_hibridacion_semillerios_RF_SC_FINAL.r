# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "001_ZZ1292_hibridacion_semillerio_kaggle_m1_y_m2"

# Se listan INDIVIDUALMENTE los semillerios que se desean incluir
# Estos archivos los genera el z1292 cuando tiene PARAM$generar_salida_hibridador <- TRUE
PARAM$archivos <- c(
  "../001_ZZ1292_hibridacion_semillerio_kaggle_m1_RF_SC/001_ZZ1292_hibridacion_semillerio_kaggle_m1_RF_SC_rank_predicciones.csv",
  "../001_ZZ1292_hibridacion_semillerio_kaggle_m2_C4_SC/001_ZZ1292_hibridacion_semillerio_kaggle_m2_C4_SC_rank_predicciones.csv"
)

PARAM$corte <- 10000 # cantidad de envios
# FIN Parametros del script

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/Competencia_Final/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/Competencia_Final/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

cat("Semillerios involucrados en la hibridacion: ", length(PARAM$archivos), "\n")
cat("Directorio de salida: ", getwd(), "\n")

for (indice_semillerio in seq_along(PARAM$archivos)) {
  
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion_semillerio <- fread(PARAM$archivos[indice_semillerio])
  #setorder(tb_prediccion, numero_de_cliente)
  
  if(!exists("tb_hibridador")) {
    tb_hibridador <- data.table(numero_de_cliente = tb_prediccion_semillerio[, numero_de_cliente])
  }
  
  tb_hibridador[, paste0("prediccion_", indice_semillerio) := tb_prediccion_semillerio$prediccion]
  tb_hibridador[, paste0("prediccion_", indice_semillerio) := frank(get(paste0("prediccion_", indice_semillerio)), ties.method = "random")]
  
}

tb_prediccion_hibridador <- data.table(
  tb_hibridador[, list(numero_de_cliente)],
  prediccion = rowMeans(tb_hibridador[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
)

setorder(tb_prediccion_hibridador, prediccion) # Esto es una media de rankings, entonces de menor a mayor
tb_prediccion_hibridador[, Predicted := 0]
tb_prediccion_hibridador[1:PARAM$corte, Predicted := 1L]

nombre_arch_ensamble <- paste0(
  PARAM$experimento,
  "_",
  "hibridacion",
  "_",
  sprintf("C%d", PARAM$corte),
  ".csv"
)
fwrite(
  tb_prediccion_hibridador[, list(numero_de_cliente, Predicted)],
  file = nombre_arch_ensamble,
  sep = ","
)