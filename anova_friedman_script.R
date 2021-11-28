# Set working directory
setwd(".")

# Read cvs
# data <-read.csv2("competencias_digitales.csv") 
# data <-read.csv2("configuración_asignatura_aula_virtual.csv") 
 # data <-read.csv2("habilidades_insertar_modulos_aula_virtual.csv") 
# data <-read.csv2("destrezas_zoom_teams.csv") 
# data <-read.csv2("tablets_clase.csv") 
# data <-read.csv2("gamificación_aula.csv") 

# Change names
prev_names <- names(data)
names(data) <- c("A", "B", "C", "D", "E")
names(data)

# New vectors
A_vector <- c()
B_vector <- c()
C_vector <- c()
D_vector <- c()
E_vector <- c()
id_vector <- c(1:30)

# Fill vectors
for (i in 1:30){
  A_vector <- append(A_vector, "A")
  B_vector <- append(B_vector, "B")
  C_vector <- append(C_vector, "C")
  D_vector <- append(D_vector, "D")
  E_vector <- append(E_vector, "E")
}

# Combine vectors
all_factors <- c(A_vector, B_vector, C_vector, D_vector, E_vector)
all_values <- c(data$A, data$B, data$C, data$D, data$E)
all_id <- c(id_vector, id_vector, id_vector, id_vector, id_vector)

# New dataframe
data_for_anova <- data.frame(factores = all_factors, values = all_values, id = id_vector)
attach(data_for_anova)

# ANOVA 
modelo1 <- aov(values ~ factores, paired = TRUE)
summary.lm(modelo1)
summary(modelo1)

# Tukey
intervals = TukeyHSD(modelo1)
intervals
plot(intervals)

# Plot normal
qqnorm(modelo1$residuals) 
qqline(modelo1$residuals)

# Test normal
shapiro.test(modelo1$residuals)

#Homogeneidad de varianzas
bartlett.test(modelo1$residuals ~ factores)

# FRIEDMAN 
friedman.test(values, factores, id)

# WILCOX
pairwise.wilcox.test(values, factores, paired = TRUE, p.adjust.method = "holm")
