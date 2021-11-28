# Set working directory
setwd(".")

# Read cvs
 data <-read.csv2("competencias_digitales.csv") 
 
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
mean_values <- c()
sd_values <- c()
var_values <- c()

for (i in 1:5){
  
  # Quantile
  quantile_values <- append(quantile_values, quantile(unlist(data[i]))) 
  
  # Interquartile Range
  iqr_values <- append(iqr_values, IQR(unlist(data[i])))
  
  # Mean
  mean_values <- append(mean_values, mean(unlist(data[i])))
  
  # Standard deviation 
  sd_values <- append(sd_values, sd(unlist(data[i])))
  
  # Variation
  var_values <- append(var_values, var(unlist(data[i])))
  
}

# Boxplot
boxplot_overview <- boxplot(data, xlab = "Adjetivos", ylab = "Valoración", col = rgb(1, 0, 0, alpha = 0.4),border = "black", outpch = 25)

# t de student
Media <- 0
t.test(data$Pesima...Excelente, mu = Media)

# Barplot with error bars

# Load ggplot2
library(ggplot2)
names <-  c("Baja-Alta", " Pésima-Excelente", "Nula-Preparados", "Inservible-Servible", "Complicado-Funcional")

df <- data.frame(
  name=names,
  value=mean_values,
  sd=sd_values
)

# dev.new(width=7, height=7, unit="in")

ggplot(df) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="mediumpurple1", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.25, colour="orange", alpha=0.8, size=0.9)+
  labs(title="¿Cómo evaluaría las competencias digitales de los docentes?", x ="Adjetivos", y = "Valoraciones") +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), limits = c(-3, 3))+ 
  theme(
    text = element_text(size=15),
    plot.title = element_text(color="black", size=18, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="gray16", size=16, face="bold"),
    axis.title.y = element_text(color="gray16", size=16, face="bold"),
    panel.background = element_rect(fill = "gray90",
                                    colour = "gray90",
                                    size = 0.9, linetype = "solid")
  )

