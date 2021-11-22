# Set working directory
setwd(".")

# Read cvs
 data <-read.csv2("competencias_digitales.csv") 
# data <-read.csv2("configuración_asignatura_aula_virtual.csv") 
# data <-read.csv2("habilidades_insertar_modulos_aula_virtual.csv") 
# data <-read.csv2("destrezas_zoom_teams.csv") 
# data <-read.csv2("tablets_clase.csv") 
# data <-read.csv2("gamificación_aula.csv") 

# Description of the data
descr <- str(data)

# Statistical summary
resume <- summary(data)

# Column names
column_names <- colnames(data)

# Row names
row_names <- rownames(data)

# Quantile
quantile_values <- c()
iqr_values <- c()

for (i in 1:5){
  
  # Quantile
  quantile_values <- append(quantile_values, quantile(unlist(data[i]))) 
  
  # Interquartile Range
  iqr_values <- append(iqr_values, IQR(unlist(data[i])))
}

# Outlayers
for (i in 1:5) {
  boxplot.stats(unlist(data[i]))$out
  
}

# Standard deviation 
sd_values <- c()

for (i in 1:5){
  column_sd <- sd(unlist(data[i]))
  sd_values <- append(sd_values, column_sd) 
}

# Variation
var_values <- c()

for (i in 1:5){
  column_var <- var(unlist(data[i]))
  var_values <- append(var_values, column_var) 
}

# Boxplot
boxplot_overview <- boxplot(data, xlab = "Adjetivos", ylab = "Valoración", col = rgb(1, 0, 0, alpha = 0.4),border = "black", outpch = 25)

# Histogram
hist(x = data$Pesima...Excelente, xlab = " Pésima - Excelente", ylab = "Frecuencia", main = "")
hist(x = data$Nula...Preparados, xlab = " Nula - Preparados", ylab = "Frecuencia", main = "")
hist(x = data$Inservible...Servible, xlab = " Inservible - Servible", ylab = "Frecuencia", main = "")
hist(x = data$Complicado...Funcional, xlab = " Complicado - Funcional", ylab = "Frecuencia", main = "")
hist(x = data$ï..Baja...Alta, xlab = " Baja - Alta", ylab = "Frecuencia", main = "")

# t de student
t.test(data)
