#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Trabajo practico Ventas y Marketing - Universidad Torcuato Di Tella - MiM + Analytics
# Alumno: Joaquin Gonzalez
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())
working_dir <- 'C:/Users/joaac/Desktop/Ditella/ditella-ventas-y-marketing'
source_dir <- 'C:/Users/joaac/Desktop/Ditella/ditella-ventas-y-marketing'
setwd(working_dir)

FILENAME = 'rocketfuel_data.csv'


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Librerias
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(Matrix)
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


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

data <- load_csv_data(FILENAME)

# separamos usuarios de grupo de control de usuarios expuestos
exposed_users <- data[!(data$test == 0), ]
control_group_users <- data[!(data$test == 1), ]

TOTAL_USERS = nrow(data)
EXPOSED_USERS = nrow(exposed_users)
CONTROL_GROUP_USERS = nrow(control_group_users)
COST_PER_1000_ADS = 9
CONVERTED_USER_VALUE = 40

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREGUNTA 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nrow(exposed_users[(exposed_users$converted == 1), ]) # cuantos compraron producto de los expuestos
nrow(control_group_users[(control_group_users$converted == 1), ]) # cuantos compraron producto de los no expuestos


CAMPAIGN_ACC_EXPOSED = (nrow(exposed_users[(exposed_users$converted == 1), ]) / EXPOSED_USERS) * 100
CAMPAIGN_ACC_NOT_EXPOSED = (nrow(control_group_users[(control_group_users$converted == 1), ]) / CONTROL_GROUP_USERS) * 100
CAMPAIGN_ACC = CAMPAIGN_ACC_EXPOSED - CAMPAIGN_ACC_NOT_EXPOSED
cat(sprintf("CAMPAIGN ACCURACY: %f", CAMPAIGN_ACC)) # en %
cat(sprintf("Usuarios que compran producto a raiz de la campana: %f", EXPOSED_USERS * (CAMPAIGN_ACC/100)))  # cuantos compraron producto de los expuestos producto de la campaña



color <- brewer.pal(length(count), "Set2") 
pie(table(exposed_users$converted), 
    main= "Exposed Users" ,
    labels = paste(prop.table(table(exposed_users$converted))*100, "%"), 
    col = color, 
    cex = 1)

legend("topleft", legend = c("Converted", "Not Converted"),
       fill = color)


color <- brewer.pal(length(count), "Set2") 
pie(table(control_group_users$converted), 
    main= "Control Group Users" ,
    labels = paste(prop.table(table(control_group_users$converted))*100, "%"), 
    col = color, 
    cex = 1)

legend("topleft", legend = c("Converted", "Not Converted"),
       fill = color)

# Analisis de proporciones
prop.table(table(exposed_users$converted))
prop.table(table(control_group_users$converted))
prop.table(table(exposed_users$mode_impr_day))
prop.table(table(exposed_users$mode_impr_hour))
prop.table(table(exposed_users$tot_impr))
prop.table(table(data$test))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREGUNTA 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# a y b
TOTAL_ADEVERTISEMENTS = sum(data$tot_impr) # cantidad de ads generados para todo el dataset
COST_ADVERTISEMENT = COST_PER_1000_ADS * (TOTAL_ADEVERTISEMENTS/1000) # costo total por cantidad de ads generados

# se resta a las ganancias dadas por usuarios que compran debido a la campaña los costos de pagar los ads para todo el dataset
cat(sprintf("Ganancia de la campana por todas las conversiones: %f", (nrow(data[(data$converted == 1), ])*CONVERTED_USER_VALUE)-COST_ADVERTISEMENT))
cat(sprintf("Ganancia generada por conversiones debidas a la campana: %f", (EXPOSED_USERS * (CAMPAIGN_ACC/100) * CONVERTED_USER_VALUE)-COST_ADVERTISEMENT))



#c 

NET_RETURN_CAMPAIGN = ((EXPOSED_USERS * (CAMPAIGN_ACC/100) * CONVERTED_USER_VALUE) - COST_ADVERTISEMENT)
ROI = ( NET_RETURN_CAMPAIGN / COST_ADVERTISEMENT) * 100



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREGUNTA 3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# conversion_rate = sum(converted == 1) / sum(dataset) y esto calcularlo para grupos tot_impr


# EXPOSED USERS
# Asi segmento en grupos para calcular el conversion rate
exposed_users_1 <- exposed_users[exposed_users$tot_impr < 25 & exposed_users$tot_impr > 0]
exposed_users_2 <- exposed_users[exposed_users$tot_impr < 50 & exposed_users$tot_impr > 25]
exposed_users_3 <- exposed_users[exposed_users$tot_impr < 75 & exposed_users$tot_impr > 50]
exposed_users_4 <- exposed_users[exposed_users$tot_impr < 100 & exposed_users$tot_impr > 75]
exposed_users_5 <- exposed_users[exposed_users$tot_impr < 125 & exposed_users$tot_impr > 100]
exposed_users_6 <- exposed_users[exposed_users$tot_impr < 150 & exposed_users$tot_impr > 125]
exposed_users_7 <- exposed_users[exposed_users$tot_impr < 175 & exposed_users$tot_impr > 150]
exposed_users_8 <- exposed_users[exposed_users$tot_impr < 200 & exposed_users$tot_impr > 175]
exposed_users_9 <- exposed_users[exposed_users$tot_impr < 225 & exposed_users$tot_impr > 200]
exposed_users_10 <- exposed_users[exposed_users$tot_impr < 250 & exposed_users$tot_impr > 225]

conversion_rate_1 = sum(exposed_users_1$converted)/nrow(exposed_users_1)
conversion_rate_2 = sum(exposed_users_2$converted)/nrow(exposed_users_2)
conversion_rate_3 = sum(exposed_users_3$converted)/nrow(exposed_users_3)
conversion_rate_4 = sum(exposed_users_4$converted)/nrow(exposed_users_4)
conversion_rate_5 = sum(exposed_users_5$converted)/nrow(exposed_users_5)
conversion_rate_6 = sum(exposed_users_6$converted)/nrow(exposed_users_6)
conversion_rate_7 = sum(exposed_users_7$converted)/nrow(exposed_users_7)
conversion_rate_8 = sum(exposed_users_8$converted)/nrow(exposed_users_8)
conversion_rate_9 = sum(exposed_users_9$converted)/nrow(exposed_users_9)
conversion_rate_10 = sum(exposed_users_10$converted)/nrow(exposed_users_10)

conversion_rates = c(conversion_rate_1, conversion_rate_2, conversion_rate_3,
                     conversion_rate_4, conversion_rate_5, conversion_rate_6,
                     conversion_rate_7, conversion_rate_8, conversion_rate_9,
                     conversion_rate_10)

plot(conversion_rates, ylab = "Conversion Rate", xlab = "Group",
     main = "Conversion Rates per Group (Exposed Users)", col = 2,
     type = "b", pch = 19)


# CONTROL GROUP USERS
# Asi segmento en grupos para clacular el conversion rate
control_group_users_1 <- control_group_users[control_group_users$tot_impr < 25 & control_group_users$tot_impr > 0]
control_group_users_2 <- control_group_users[control_group_users$tot_impr < 50 & control_group_users$tot_impr > 25]
control_group_users_3 <- control_group_users[control_group_users$tot_impr < 75 & control_group_users$tot_impr > 50]
control_group_users_4 <- control_group_users[control_group_users$tot_impr < 100 & control_group_users$tot_impr > 75]
control_group_users_5 <- control_group_users[control_group_users$tot_impr < 125 & control_group_users$tot_impr > 100]
control_group_users_6 <- control_group_users[control_group_users$tot_impr < 150 & control_group_users$tot_impr > 125]
control_group_users_7 <- control_group_users[control_group_users$tot_impr < 175 & control_group_users$tot_impr > 150]
control_group_users_8 <- control_group_users[control_group_users$tot_impr < 200 & control_group_users$tot_impr > 175]
control_group_users_9 <- control_group_users[control_group_users$tot_impr < 225 & control_group_users$tot_impr > 200]
control_group_users_10 <- control_group_users[control_group_users$tot_impr < 250 & control_group_users$tot_impr > 225]

conversion_rate_1 = sum(control_group_users_1$converted)/nrow(control_group_users_1)
conversion_rate_2 = sum(control_group_users_2$converted)/nrow(control_group_users_2)
conversion_rate_3 = sum(control_group_users_3$converted)/nrow(control_group_users_3)
conversion_rate_4 = sum(control_group_users_4$converted)/nrow(control_group_users_4)
conversion_rate_5 = sum(control_group_users_5$converted)/nrow(control_group_users_5)
conversion_rate_6 = sum(control_group_users_6$converted)/nrow(control_group_users_6)
conversion_rate_7 = sum(control_group_users_7$converted)/nrow(control_group_users_7)
conversion_rate_8 = sum(control_group_users_8$converted)/nrow(control_group_users_8)
conversion_rate_9 = sum(control_group_users_9$converted)/nrow(control_group_users_9)
conversion_rate_10 = sum(control_group_users_10$converted)/nrow(control_group_users_10)

conversion_rates = c(conversion_rate_1, conversion_rate_2, conversion_rate_3,
                     conversion_rate_4, conversion_rate_5, conversion_rate_6,
                     conversion_rate_7, conversion_rate_8, conversion_rate_9,
                     conversion_rate_10)

plot(conversion_rates, ylab = "Conversion Rate", xlab = "Group",
     main = "Conversion Rates per Group (Control Group Users)", col = 3,
     type = "b", pch = 19)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREGUNTA 4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Analisis Exploratorio
hist(exposed_users$tot_impr, 
     main = "Total Impresions Exposed Users", 
     ylab = "# of users",
     xlab = "# of impressions",
     xlim = c(0,250),
     breaks = 2000,
     col = "darkmagenta")


hist(control_group_users$tot_impr, 
     main = "Total Impresions Control Group Users", 
     ylab = "# of users",
     xlab = "# of impressions",
     xlim = c(0,250),
     breaks = 2000,
     col = "darkmagenta")


hist(exposed_users$mode_impr_hour, 
     main = "Hour of day most impressions", 
     ylab = "# of impressions",
     xlab = "hour of day",
     col = "darkmagenta",
     breaks = 500)


hist(control_group_users$mode_impr_hour, 
     main = "Hour of day most impressions", 
     ylab = "# of impressions",
     xlab = "hour of day",
     col = "darkmagenta",
     breaks = 500)


hist(exposed_users$mode_impr_day, 
     main = "Day of week most impressions Exposed Users", 
     ylab = "# of impressions", 
     xlab = "day of week",
     col = "orange",
     breaks = 7)

hist(control_group_users$mode_impr_day, 
     main = "Day of week most impressions Control Group", 
     ylab = "# of impressions",
     xlab = "day of week",
     col = "darkmagenta",
     breaks = 7)

# a

# EXPOSED USERS DAY OF WEEK
# Asi segmento en grupos para calcular el conversion rate
exposed_users_1 <- exposed_users[exposed_users$mode_impr_day == 1]
exposed_users_2 <- exposed_users[exposed_users$mode_impr_day == 2]
exposed_users_3 <- exposed_users[exposed_users$mode_impr_day == 3]
exposed_users_4 <- exposed_users[exposed_users$mode_impr_day == 4]
exposed_users_5 <- exposed_users[exposed_users$mode_impr_day == 5]
exposed_users_6 <- exposed_users[exposed_users$mode_impr_day == 6]
exposed_users_7 <- exposed_users[exposed_users$mode_impr_day == 7]


conversion_rate_1 = sum(exposed_users_1$converted)/nrow(exposed_users_1)
conversion_rate_2 = sum(exposed_users_2$converted)/nrow(exposed_users_2)
conversion_rate_3 = sum(exposed_users_3$converted)/nrow(exposed_users_3)
conversion_rate_4 = sum(exposed_users_4$converted)/nrow(exposed_users_4)
conversion_rate_5 = sum(exposed_users_5$converted)/nrow(exposed_users_5)
conversion_rate_6 = sum(exposed_users_6$converted)/nrow(exposed_users_6)
conversion_rate_7 = sum(exposed_users_7$converted)/nrow(exposed_users_7)


conversion_rates = c(conversion_rate_1, conversion_rate_2, conversion_rate_3,
                     conversion_rate_4, conversion_rate_5, conversion_rate_6,
                     conversion_rate_7)

plot(conversion_rates, ylab = "Conversion Rate", xlab = "Day of week",
     main = "Conversion Rates per Group DAY OF WEEK (Exposed Users)", col = 2,
     type = "b", pch = 19)


# CONTROL GROUP USERS DAY OF WEEK
# Asi segmento en grupos para clacular el conversion rate
control_group_users_1 <- control_group_users[control_group_users$mode_impr_day == 1]
control_group_users_2 <- control_group_users[control_group_users$mode_impr_day == 2]
control_group_users_3 <- control_group_users[control_group_users$mode_impr_day == 3]
control_group_users_4 <- control_group_users[control_group_users$mode_impr_day == 4]
control_group_users_5 <- control_group_users[control_group_users$mode_impr_day == 5]
control_group_users_6 <- control_group_users[control_group_users$mode_impr_day == 6]
control_group_users_7 <- control_group_users[control_group_users$mode_impr_day == 7]


conversion_rate_1 = sum(control_group_users_1$converted)/nrow(control_group_users_1)
conversion_rate_2 = sum(control_group_users_2$converted)/nrow(control_group_users_2)
conversion_rate_3 = sum(control_group_users_3$converted)/nrow(control_group_users_3)
conversion_rate_4 = sum(control_group_users_4$converted)/nrow(control_group_users_4)
conversion_rate_5 = sum(control_group_users_5$converted)/nrow(control_group_users_5)
conversion_rate_6 = sum(control_group_users_6$converted)/nrow(control_group_users_6)
conversion_rate_7 = sum(control_group_users_7$converted)/nrow(control_group_users_7)


conversion_rates = c(conversion_rate_1, conversion_rate_2, conversion_rate_3,
                     conversion_rate_4, conversion_rate_5, conversion_rate_6,
                     conversion_rate_7)

plot(conversion_rates, ylab = "Conversion Rate", xlab = "Day of week",
     main = "Conversion Rates per Group DAY OF WEEK (Control Group Users)", col = 3,
     type = "b", pch = 19)


# b 
# EXPOSED USERS HOUR OF DAY
# Asi segmento en grupos para calcular el conversion rate
exposed_users_1 <- exposed_users[exposed_users$mode_impr_hour == 1]
exposed_users_2 <- exposed_users[exposed_users$mode_impr_hour == 2]
exposed_users_3 <- exposed_users[exposed_users$mode_impr_hour == 3]
exposed_users_4 <- exposed_users[exposed_users$mode_impr_hour == 4]
exposed_users_5 <- exposed_users[exposed_users$mode_impr_hour == 5]
exposed_users_6 <- exposed_users[exposed_users$mode_impr_hour == 6]
exposed_users_7 <- exposed_users[exposed_users$mode_impr_hour == 7]
exposed_users_8 <- exposed_users[exposed_users$mode_impr_hour == 8]
exposed_users_9 <- exposed_users[exposed_users$mode_impr_hour == 9]
exposed_users_10 <- exposed_users[exposed_users$mode_impr_hour == 10]
exposed_users_11 <- exposed_users[exposed_users$mode_impr_hour == 11]
exposed_users_12 <- exposed_users[exposed_users$mode_impr_hour == 12]
exposed_users_13 <- exposed_users[exposed_users$mode_impr_hour == 13]
exposed_users_14 <- exposed_users[exposed_users$mode_impr_hour == 14]
exposed_users_15 <- exposed_users[exposed_users$mode_impr_hour == 15]
exposed_users_16 <- exposed_users[exposed_users$mode_impr_hour == 16]
exposed_users_17 <- exposed_users[exposed_users$mode_impr_hour == 17]
exposed_users_18 <- exposed_users[exposed_users$mode_impr_hour == 18]
exposed_users_19 <- exposed_users[exposed_users$mode_impr_hour == 19]
exposed_users_20 <- exposed_users[exposed_users$mode_impr_hour == 20]
exposed_users_21 <- exposed_users[exposed_users$mode_impr_hour == 21]
exposed_users_22 <- exposed_users[exposed_users$mode_impr_hour == 22]
exposed_users_23 <- exposed_users[exposed_users$mode_impr_hour == 23]
exposed_users_24 <- exposed_users[exposed_users$mode_impr_hour == 24]

conversion_rate_1 = sum(exposed_users_1$converted)/nrow(exposed_users_1)
conversion_rate_2 = sum(exposed_users_2$converted)/nrow(exposed_users_2)
conversion_rate_3 = sum(exposed_users_3$converted)/nrow(exposed_users_3)
conversion_rate_4 = sum(exposed_users_4$converted)/nrow(exposed_users_4)
conversion_rate_5 = sum(exposed_users_5$converted)/nrow(exposed_users_5)
conversion_rate_6 = sum(exposed_users_6$converted)/nrow(exposed_users_6)
conversion_rate_7 = sum(exposed_users_7$converted)/nrow(exposed_users_7)
conversion_rate_8 = sum(exposed_users_8$converted)/nrow(exposed_users_8)
conversion_rate_9 = sum(exposed_users_9$converted)/nrow(exposed_users_9)
conversion_rate_10 = sum(exposed_users_10$converted)/nrow(exposed_users_10)
conversion_rate_11 = sum(exposed_users_1$converted)/nrow(exposed_users_11)
conversion_rate_12 = sum(exposed_users_2$converted)/nrow(exposed_users_12)
conversion_rate_13 = sum(exposed_users_3$converted)/nrow(exposed_users_13)
conversion_rate_14 = sum(exposed_users_4$converted)/nrow(exposed_users_14)
conversion_rate_15 = sum(exposed_users_5$converted)/nrow(exposed_users_15)
conversion_rate_16 = sum(exposed_users_6$converted)/nrow(exposed_users_16)
conversion_rate_17 = sum(exposed_users_7$converted)/nrow(exposed_users_17)
conversion_rate_18 = sum(exposed_users_8$converted)/nrow(exposed_users_18)
conversion_rate_19 = sum(exposed_users_9$converted)/nrow(exposed_users_19)
conversion_rate_20 = sum(exposed_users_10$converted)/nrow(exposed_users_20)
conversion_rate_21 = sum(exposed_users_1$converted)/nrow(exposed_users_21)
conversion_rate_22 = sum(exposed_users_2$converted)/nrow(exposed_users_22)
conversion_rate_23 = sum(exposed_users_3$converted)/nrow(exposed_users_23)
conversion_rate_24 = sum(exposed_users_4$converted)/nrow(exposed_users_24)


conversion_rates = c(conversion_rate_1, conversion_rate_2, conversion_rate_3,
                     conversion_rate_4, conversion_rate_5, conversion_rate_6,
                     conversion_rate_7, conversion_rate_8, conversion_rate_9,
                     conversion_rate_10,conversion_rate_11,conversion_rate_12,
                     conversion_rate_13, conversion_rate_14, conversion_rate_15,
                     conversion_rate_16,conversion_rate_17,conversion_rate_18,
                     conversion_rate_19, conversion_rate_20, conversion_rate_21,
                     conversion_rate_22,conversion_rate_23,conversion_rate_24)

plot(conversion_rates, ylab = "Conversion Rate", xlab = "Hour of day",
     main = "Conversion Rates per Group HOUR OF DAY (Exposed Users)", col = 2,
     type = "b", pch = 19)


# CONTROL GROUP USERS HOUR OF DAY
# Asi segmento en grupos para clacular el conversion rate
control_group_users_1 <- control_group_users[control_group_users$mode_impr_hour == 1]
control_group_users_2 <- control_group_users[control_group_users$mode_impr_hour == 2]
control_group_users_3 <- control_group_users[control_group_users$mode_impr_hour == 3]
control_group_users_4 <- control_group_users[control_group_users$mode_impr_hour == 4]
control_group_users_5 <- control_group_users[control_group_users$mode_impr_hour == 5]
control_group_users_6 <- control_group_users[control_group_users$mode_impr_hour == 6]
control_group_users_7 <- control_group_users[control_group_users$mode_impr_hour == 7]
control_group_users_8 <- control_group_users[control_group_users$mode_impr_hour == 8]
control_group_users_9 <- control_group_users[control_group_users$mode_impr_hour == 9]
control_group_users_10 <- control_group_users[control_group_users$mode_impr_hour == 10]
control_group_users_11 <- control_group_users[control_group_users$mode_impr_hour == 11]
control_group_users_12 <- control_group_users[control_group_users$mode_impr_hour == 12]
control_group_users_13 <- control_group_users[control_group_users$mode_impr_hour == 13]
control_group_users_14 <- control_group_users[control_group_users$mode_impr_hour == 14]
control_group_users_15 <- control_group_users[control_group_users$mode_impr_hour == 15]
control_group_users_16 <- control_group_users[control_group_users$mode_impr_hour == 16]
control_group_users_17 <- control_group_users[control_group_users$mode_impr_hour == 17]
control_group_users_18 <- control_group_users[control_group_users$mode_impr_hour == 18]
control_group_users_19 <- control_group_users[control_group_users$mode_impr_hour == 19]
control_group_users_20 <- control_group_users[control_group_users$mode_impr_hour == 20]
control_group_users_21 <- control_group_users[control_group_users$mode_impr_hour == 21]
control_group_users_22 <- control_group_users[control_group_users$mode_impr_hour == 22]
control_group_users_23 <- control_group_users[control_group_users$mode_impr_hour == 23]
control_group_users_24 <- control_group_users[control_group_users$mode_impr_hour == 24]


conversion_rate_1 = sum(control_group_users_1$converted)/nrow(control_group_users_1)
conversion_rate_2 = sum(control_group_users_2$converted)/nrow(control_group_users_2)
conversion_rate_3 = sum(control_group_users_3$converted)/nrow(control_group_users_3)
conversion_rate_4 = sum(control_group_users_4$converted)/nrow(control_group_users_4)
conversion_rate_5 = sum(control_group_users_5$converted)/nrow(control_group_users_5)
conversion_rate_6 = sum(control_group_users_6$converted)/nrow(control_group_users_6)
conversion_rate_7 = sum(control_group_users_7$converted)/nrow(control_group_users_7)
conversion_rate_8 = sum(control_group_users_8$converted)/nrow(control_group_users_8)
conversion_rate_9 = sum(control_group_users_9$converted)/nrow(control_group_users_9)
conversion_rate_10 = sum(control_group_users_10$converted)/nrow(control_group_users_10)
conversion_rate_11 = sum(control_group_users_1$converted)/nrow(control_group_users_11)
conversion_rate_12 = sum(control_group_users_2$converted)/nrow(control_group_users_12)
conversion_rate_13 = sum(control_group_users_3$converted)/nrow(control_group_users_13)
conversion_rate_14 = sum(control_group_users_4$converted)/nrow(control_group_users_14)
conversion_rate_15 = sum(control_group_users_5$converted)/nrow(control_group_users_15)
conversion_rate_16 = sum(control_group_users_6$converted)/nrow(control_group_users_16)
conversion_rate_17 = sum(control_group_users_7$converted)/nrow(control_group_users_17)
conversion_rate_18 = sum(control_group_users_8$converted)/nrow(control_group_users_18)
conversion_rate_19 = sum(control_group_users_9$converted)/nrow(control_group_users_19)
conversion_rate_20 = sum(control_group_users_10$converted)/nrow(control_group_users_20)
conversion_rate_21 = sum(control_group_users_1$converted)/nrow(control_group_users_21)
conversion_rate_22 = sum(control_group_users_2$converted)/nrow(control_group_users_22)
conversion_rate_23 = sum(control_group_users_3$converted)/nrow(control_group_users_23)
conversion_rate_24 = sum(control_group_users_4$converted)/nrow(control_group_users_24)

conversion_rates = c(conversion_rate_1, conversion_rate_2, conversion_rate_3,
                     conversion_rate_4, conversion_rate_5, conversion_rate_6,
                     conversion_rate_7, conversion_rate_8, conversion_rate_9,
                     conversion_rate_10,conversion_rate_11,conversion_rate_12,
                     conversion_rate_13, conversion_rate_14, conversion_rate_15,
                     conversion_rate_16,conversion_rate_17,conversion_rate_18,
                     conversion_rate_19, conversion_rate_20, conversion_rate_21,
                     conversion_rate_22,conversion_rate_23,conversion_rate_24)

plot(conversion_rates, ylab = "Conversion Rate", xlab = "Hour of day",
     main = "Conversion Rates per Group HOUR OF DAY (Control Group Users)", col = 3,
     type = "b", pch = 19)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREGUNTA 5
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

model = lm(converted ~ tot_impr + mode_impr_day + mode_impr_hour, data = exposed_users)
summary(model)
