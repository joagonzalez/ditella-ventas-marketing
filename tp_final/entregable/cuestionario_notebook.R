#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Trabajo practico final Ventas y Marketing - Universidad Torcuato Di Tella - MiM + Analytics
# Alumno: Joaquin Gonzalez
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())
working_dir <- '/home/jgonzalez/dev/ventas-marketing/TP_FINAL'
source_dir <- '/home/jgonzalez/dev/ventas-marketing/TP_FINAL'
setwd(working_dir)

ANSWERS_FILENAME = 'answers.csv'
DATASET_FILENAME = 'dataset.csv'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Librerias
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(Matrix)
library(data.table)
library(dplyr)
library(fastDummies)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Funciones Custom
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

load_csv_data <- function(csv_file, sample_ratio = 1, drop_cols = NULL,
                          sel_cols = NULL) {
  
  dt <- fread(csv_file, header = TRUE, sep = ",", stringsAsFactors = TRUE,
              na.strings = "", drop = drop_cols, select = sel_cols,
              showProgress = TRUE)
  
  if (sample_ratio < 1) {
    sample_size <- as.integer(sample_ratio * nrow(dt))
    dt <- dt[sample(.N, sample_size)]
  }
  
  return(dt)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Programa principal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


c <- expand.grid(
  Pantalla <- c("14", "15.6"),
  Procesador <- c("i3", "i5", "i7"),
  Memoria <- c("8GB", "16GB","32GB"),
  Precio <- c("450USD", "700USD", "1300USD"),
  Almacenamiento <- c("250GB", "500GB", "1TB"))

#change the column names to these
names(c) <- c("Pantalla", "Procesador", "Memoria", "Precio", "Almacenamiento")

design <- caFactorialDesign(data=c, type="fractional")
code <- caEncodedDesign(design)
encodedorthodesign <- data.frame(design, code)
print(encodedorthodesign)
data = encodedorthodesign[]

# Se generan dummy vars y se acomoda el dataset
data = data[, ! names(data) %in% c("Pantalla", "Memoria", "Precio", "Almacenamiento", "Procesador")]
data <- dummy_cols(data, select_columns = 'Pantalla.1')
data <- dummy_cols(data, select_columns = 'Almacenamiento.1')
data <- dummy_cols(data, select_columns = 'Procesador.1')
data <- dummy_cols(data, select_columns = 'Memoria.1')
data <- dummy_cols(data, select_columns = 'Precio.1')
data = data[, ! names(data) %in% c("Pantalla.1", "Memoria.1", "Precio.1", "Almacenamiento.1", "Procesador.1")]


answers = load_csv_data(ANSWERS_FILENAME) # cargamos respuestas de la encuesta

dataset_out = as.data.frame(lapply(data, rep, nrow(answers))) # repetimos las 15 combinaciones encuestadas por la cantidad de usuarios que contestaron la encuesta
dataset_out$Encuestado <- ceiling(as.numeric(rownames(dataset_out))/nrow(data)) # agregamos columna con id de usuario para respuestas

answers = select(answers, -1) # removemos columna timestamp
rankings = matrix(t(as.matrix(answers)), nrow = 1) # aplanamos en una fila las respuestas de ranking de todos los usuarios

dataset_out$rankings = t(rankings) # Agregamos columna rankings al dataset final para su posterior analisis

write.csv(dataset_out, DATASET_FILENAME) # exportamos el dataset

