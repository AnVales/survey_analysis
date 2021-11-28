# Preanalisis

# Set working directory
setwd(".")

# Read cvs
data <-read.csv2("competencias_digitales.csv") 

# Mean
mean_values <- c()

for (i in 1:5){
  
  # Mean
  mean_values <- append(mean_values, mean(unlist(data[,i]), na.rm = T))

}

data_imp<-data

for(i in 1:5){
  if(is.na(data_imp$ï..Baja...Alta[i])){
    data_imp$ï..Baja...Alta[i]=mean_values(1)
  }
  if(is.na(data_imp$Pesima...Excelente[i])){
    data_imp$Pesima...Excelente[i]=mean_values(2)
  }
  if(is.na(data_imp$Nula...Preparados[i])){
    data_imp$Nula...Preparados[i]=mean_values(3)
  }
  if(is.na(data_imp$Inservible...Servible[i])){
    data_imp$Inservible...Servible[i]=mean_values(4)
  }
  if(is.na(data_imp$Complicado...Funcional[i])){
    data_imp$Complicado...Funcional[i]=mean_values(5)
  }
}


  
