# Set working directory
setwd(".")

# Read cvs
data <-read.csv2("competencias_digitales.csv") 

# TEST DE NORMALIDAD
# H0: La muestra proviene de una distribución normal
# H1: La muestra no proviene de una distribución normal

# Alfa=0.05
# Si P < Alfa Se rechaza Ho
# Si p >= Alfa No se rechaza Ho

###########################################################################
print("Columna 1")
shapiro.test(data[,1])
###########################################################################
print("Columna 2")
shapiro.test(data[,2])
###########################################################################
print("Columna 3")
shapiro.test(data[,3])
###########################################################################
print("Columna 4")
shapiro.test(data[,4])
###########################################################################
print("Columna 5")
shapiro.test(data[,5])
###########################################################################


