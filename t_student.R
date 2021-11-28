# Set working directory
setwd(".")

# Read cvs
data <-read.csv2("competencias_digitales.csv") 

###############################################################################
#             CONDICIONES                                                     #
###############################################################################

# 1. INDEPENDENCIA: SON PAREADOS

# 2. NORMAL

par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2))
qqnorm(data[,1], xlab = "", ylab = "",
       main = "Inservible-Servible", col = "firebrick")
qqline(data[,1])
qqnorm(data[,2], xlab = "", ylab = "",
       main = "Nula-Preparados", col = "springgreen4")
qqline(data[,2])


# VARIANZA: NO ES NECESARIO

###############################################################################
#             SE HACE                                                         #
###############################################################################

# HIPOTESIS: 
# H0 : No hay diferencia entre las medias, media 1 ??? media 2 = 0
# Ha : Sí hay diferencia entre las medias, media 1 ??? media 2 ??? 0

# Alfa=0.05
# Si P < Alfa Se rechaza Ho
# Si p >= Alfa No se rechaza Ho

# Parámetro estimado (estadístico)
dif_medias <- mean(data$Inservible...Servible) - mean(data$Nula...Preparados)
# En este caso dif_medias =  0.7666667

# Normalidad
shapiro.test(data$Inservible...Servible)

shapiro.test(data$Nula...Preparados)

# Los test encuentran evidencias significativas de que los datos no proceden de poblaciones 
# con distribución normal. Sin embargo, dado que el tamaño de cada grupo es mayor que 30 
# se puede considerar que el t-test sigue siendo suficientemente robusto, 
# aunque es necesario mencionarlo en las conclusiones. 
# Un test no paramétrico basado en la mediana (Mann-Withney-Wilcoxon test) 
# o un test de Bootstraping serían más adecuados. 
# Otra opción sería estudiar si los datos anómalos son excepciones que se pueden 
# excluir del análisis.

t.test(x = data$Inservible...Servible, y = data$Nula...Preparados, alternative = "two.sided",
       mu = 0, paired = TRUE, conf.level = 0.95)



