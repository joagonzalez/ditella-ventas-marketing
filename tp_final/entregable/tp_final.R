#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Trabajo practico final Ventas y Marketing - Universidad Torcuato Di Tella - MiM + Analytics
# Alumno: Joaquin Gonzalez
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())
working_dir <- '/home/jgonzalez/dev/ventas-marketing/TP_FINAL'
image_dir <- '/home/jgonzalez/dev/ventas-marketing/TP_FINAL/images'
setwd(working_dir)

DATASET_FILENAME = 'dataset.csv' # El input de este script es el output de cuestionario_notebook.r

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Librerias
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(Matrix)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(fastDummies)
library(conjoint)

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

n_encuestados = 17
data = load_csv_data(DATASET_FILENAME) # cargamos dataset preprocesado por script cuestionario_notebook.R


# separo en n_encuestados datasets el dataset original para poder hacer las el analisis mas facil
for (i in 1:n_encuestados){
  assign(paste("user", i, sep = ""), data[data[, data$Encuestado == i]])
}

levels <- c("Intercept", "Pantalla14", "Pantalla156", "Almacentamiento250", "Almacentamiento500", "Almacentamiento1000", "Procesadori3", "Procesadori5", "Procesadori7", "Memoria8", "Memoria16", "Memoria32", "Precio450", "Precio700", "Precio1300")
attributes <- c("Intercept", rep(c("Pantalla"), times = 2), rep(c("Almacenamiento"), times = 3) , 
                rep(c("Procesador"), times = 3), rep(c("Memoria"), times = 3), rep(c("Precio"), times = 3))

# Por cada usuario calculamos regresion lineal contemplando valores de referencia para intercepto los siguientes datos:
# pantalla 14, almacenamiento 250gb, procesador i3, memoria 8gb, precio 450usd

setwd(image_dir)
relative_price = c()
wtp_per_user = c()
elijo_a = c()

for (i in 1:n_encuestados){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 1 - Regresion lineal por usuario
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Producto de referencia: Pantalla14, Almacenamiento250, Procesadori3, Memoria8GB, Precio450
  tmp = lm(rankings ~ Pantalla.1_2 + Almacenamiento.1_2 + Almacenamiento.1_3 + Procesador.1_2 + Procesador.1_3 + Memoria.1_2 + Memoria.1_3 + Precio.1_2 + Precio.1_3, 
     data[data[, data$Encuestado == i]])
  
  assign(paste("regresion", i , sep=""), tmp) # Guardo regresion en variable

  print("-------------------------------------")
  print(sprintf("Regresion para encuestado %d", i))
  print("-------------------------------------")
  print(summary(tmp))
  
  regression_tmp = tmp
  coeffs_tmp =  as.vector(summary(regression_tmp)$coefficients[,1])
  pw_ut_tmp = c(coeffs_tmp[1], 0, coeffs_tmp[2], 0, coeffs_tmp[3:4], 0, coeffs_tmp[5:6], 0, coeffs_tmp[7:8], 0, coeffs_tmp[9:10])
  pw_ut_df_tmp <- data.frame(Variable = attributes, Levels = levels, pw_ut_tmp)
  
  
  cellPantalla <- subset(pw_ut_df_tmp, pw_ut_df_tmp$Variable == "Pantalla")
  cellAlmacenamiento <- subset(pw_ut_df_tmp, pw_ut_df_tmp$Variable == "Almacenamiento")
  cellProcesador <- subset(pw_ut_df_tmp, pw_ut_df_tmp$Variable == "Procesador")
  cellMemoria <- subset(pw_ut_df_tmp, pw_ut_df_tmp$Variable == "Memoria")
  cellPrecio <- subset(pw_ut_df_tmp, pw_ut_df_tmp$Variable == "Precio")

  # Definimos grafico por cada atributo
  gg1 <- ggplot(data = cellPantalla, aes(x = Levels, y = pw_ut_tmp, group = 1)) + geom_line() + geom_point() + ggtitle("Pantalla") + ylab("Part-Worth Utilities")
  gg2 <- ggplot(data = cellAlmacenamiento, aes(x = Levels, y = pw_ut_tmp, group = 1)) + geom_line() + geom_point() + ggtitle("Almacenamiento") + ylab("Part-Worth Utilities")
  gg3 <- ggplot(data = cellProcesador, aes(x = Levels, y = pw_ut_tmp, group = 1)) + geom_line() + geom_point() + ggtitle("Procesador") + ylab("Part-Worth Utilities")
  gg4 <- ggplot(data = cellMemoria, aes(x = Levels, y = pw_ut_tmp, group = 1)) + geom_line() + geom_point() + ggtitle("Memoria") + ylab("Part-Worth Utilities")
  gg5 <- ggplot(data = cellPrecio, aes(x = Levels, y = pw_ut_tmp, group = 1)) + geom_line() + geom_point() + ggtitle(paste("Precio usuario ", i, sep="")) + ylab("Part-Worth Utilities") + theme(plot.title = element_text(size = 18, face = "bold"))
  
  # Graficamos utilidades por usuario
  png(file=paste("pw_ut_user_", i, sep= ""), width=1024, height=768)
  grid.arrange(gg1, gg2, gg3, gg4, gg5)
  dev.off()
  
  # Graficamos utilidades por usuario solo para precio
  png(file=paste("pw_ut_user_price_", i, sep= ""), width=1024, height=768)
  grid.arrange(gg5)
  dev.off()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 2 - Importancia relativa de cada atributo por cada encuestado
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # rangos
  cellPantalla_range <- max(cellPantalla$pw_ut_tmp) - min(cellPantalla$pw_ut_tmp)
  cellAlmacenamiento_range <- max(cellAlmacenamiento$pw_ut_tmp) - min(cellAlmacenamiento$pw_ut_tmp)
  cellProcesador_range <- max(cellProcesador$pw_ut_tmp) - min(cellProcesador$pw_ut_tmp)
  cellMemoria_range <- max(cellMemoria$pw_ut_tmp) - min(cellMemoria$pw_ut_tmp)
  cellPrecio_range <- max(cellPrecio$pw_ut_tmp) - min(cellPrecio$pw_ut_tmp)
  total_range <- sum(cellPantalla_range + cellAlmacenamiento_range + cellProcesador_range + cellMemoria_range + cellPrecio_range)
  
  # importancia relativa
  cellPantalla_importance <- cellPantalla_range/total_range
  cellAlmacenamiento_importance <- cellAlmacenamiento_range/total_range
  cellProcesador_importance <- cellProcesador_range/total_range
  cellMemoria_importance <- cellMemoria_range/total_range
  cellPrecio_importance <- cellPrecio_range/total_range
  relative_importance <- data.frame(Attribute = c("Pantalla", "Almacenamiento", "Procesador", "Memoria", "Precio"),
                                    Importance = c(cellPantalla_importance, cellAlmacenamiento_importance, cellProcesador_importance, cellMemoria_importance, cellPrecio_importance))

  relative_importance_price <- data.frame(Attribute = c("Precio"),
                                    Importance = c(cellPrecio_importance))
  

  # Graficamos importancia relativa por usuario para todas las variables
  ggplot(relative_importance, aes(x = Attribute, y = Importance)) + geom_bar(stat = "identity") + ggtitle(paste("Importancia relativa de atributos para usuario ", i, sep="")) + theme(plot.title = element_text(size = 16, face = "bold"))
  ggsave(paste("importance", i, setp=""), device = "png")
  
  
  relative_price <- c(relative_price, cellPrecio_importance)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 4 - Cuanto estan dispuestos a pagar los consumidores por cambios en el precio?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  lowPrice <- 450
  mediumPrice <- 700
  highPrice <- 1300

  
  # Se quita el precio de relative_importance_user ya que no debemos realizar el analisis con ese atributo
  relative_importance_user = c(cellPantalla_importance, cellAlmacenamiento_importance, cellProcesador_importance, cellMemoria_importance)
  # Capturamos el atributo mas importante del current user del loop y el valor
  most_important_attribute_value = relative_importance_user[which.max(relative_importance_user)]
  most_important_attribute = attributes[which.max(relative_importance_user)]
  
  cell <- subset(pw_ut_df_tmp, pw_ut_df_tmp$Variable == most_important_attribute)
  cell_max_ut <- max(cell$pw_ut_tmp)
  cell_min_ut <- min(cell$pw_ut_tmp)
  
  # Para comparar, siempre tomamos la diferencia entre el maximo y el minimo como rango para la categoria mas importante para este usuario
  ut_range <- cell_max_ut - cell_min_ut
  # Vemos el rango de precio entre alto y bajo y la diferencia de utilidad entre esos precios para este usuario
  price_range <- highPrice - lowPrice
  price_ut_range <- cellPrecio$pw_ut_tmp[cellPrecio$Levels == "Precio450"] - cellPrecio$pw_ut_tmp[cellPrecio$Levels == "Precio1300"]
  
  # The monetary value of one unit of utility is
  mv <- price_range / price_ut_range
  # Por lo tanto, el WTP para modificar el atributo favorito en base al rango de precio es:
  mv*ut_range 
  
  # Guardamos en un vector todas las willingess to pay para cada usuario encuestado
  wtp_per_user <- c(wtp_per_user, mv*ut_range) # continua la finalizacion del ejercicio 4 fuera del for en linea 234
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 6 - Generar dos perfiles de productos. Calcular estimacion sobre participacion de mercado para cada uno de estos productos
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # La utilidad de un producto es la suma de las utilidades de sus atributos para el usuario de esta iteracion
  # Producto A: i7, 32GB RAM, 1TB, 14", 1300USD
  # Producto B: i5, 16GB,RAM, 1TB, 14", 700USD
  
  utility_a <- sum(pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Intercept"] + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Procesadori7"] 
                   + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Memoria32"] + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Almacentamiento1000"]
                   + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Pantalla14"] + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Precio1300"])
  
  utility_b <-  sum(pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Intercept"] + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Procesadori5"] 
                    + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Memoria16"] + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Almacentamiento1000"]
                    + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Pantalla14"] + pw_ut_df_tmp$pw_ut_tmp[pw_ut_df_tmp$Levels == "Precio700"])

  
  elijo_a = c(elijo_a, utility_a>utility_b) # continua la finalizacion del ejercicio 6 fuera del for en linea 252
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Imprimimos resultados para el i-usuario
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  print(sprintf("Usuario: %d", i))
  print(sprintf("Pantalla %f", cellPantalla_importance))
  print(sprintf("Almacenamiento %f", cellAlmacenamiento_importance))
  print(sprintf("Procesador %f", cellProcesador_importance))
  print(sprintf("Memoria %f", cellMemoria_importance))
  print(sprintf("Precio %f", cellPrecio_importance))
  print(sprintf("Atributo mas importante para usuario: %s", most_important_attribute))
  print(sprintf("Importancia relativa del atributo mas importante: %f", most_important_attribute_value))
  print(sprintf("Rango de utilidad para el atributo: %f", ut_range))
  print(sprintf("Rango de utilidad para el precio: %f", price_ut_range))
  print(sprintf("Voluntad a pagar por cambio en el atributo: %f", mv*ut_range ))
  print(sprintf("Utilidad nuevo producto A: %f", utility_a ))
  print(sprintf("Utilidad nuevo producto B: %f", utility_b ))
  
} 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3 - Valores parciales asociados a precio de cada encuestado
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd(working_dir)

x_price = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
hist(relative_price,
     main = "Importancia relativa precio por usuario", 
     ylab = "Cantidad de usuarios",
     xlab = "Importancia respecto a otros atributos",
     col = "darkmagenta",
     breaks = 20)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 4 - Voluntad a pagar por usuario
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

hist(wtp_per_user,
     main = "Voluntad a pagar por cambio en atributo mas importante por usuario", 
     ylab = "Cantidad de usuarios",
     xlab = "Voluntad a pagar en USD",
     col = "darkmagenta",
     breaks = 20)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5 - Realizar segmentacion de encuestados basandose en preferencias obtenidas mediante la regresion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# se realiza en informe

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 6 - Generar dos perfiles de productos. Calcular estimacion sobre participacion de mercado para cada uno de estos productos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

usuarios_a = length(elijo_a[elijo_a == TRUE])
usuarios_b = length(elijo_a[elijo_a == FALSE])
usuarios = 17
market_share_a = usuarios_a / usuarios
market_share_b = usuarios_b / usuarios

print(sprintf("Market Share A: %f", market_share_a ))
print(sprintf("Market Share B: %f", market_share_b ))

