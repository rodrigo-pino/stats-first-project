mean_test<- function(sample1, sample2, alpha, alt){
  #  Realiza la prueba de hipotesis para la comparacion de las medias con 
  # varianza poblacional desconocida.
  # Retorna True si se sigue cumpliendo H0
  
  #Comprobando si las varianzas son iguales
  # H0: sigma1^2 = sigma2^2
  # H1: sigma1^2 != sigma2^2
  result <- var.test(sample1,sample2, alternative = "two.sided")
  pvalue <- result$p.value
  varequal <- (pvalue >= alpha) #Si p value >= alpha se consideran iguales las 
                                #varianzas
  result <-t.test(sample1,sample2, alternative = alt, var.equal = varequal)
  return (result$p.value >= alpha)
}


data <- read.csv("../raw_data/adult.data.csv")
low <- data[data$income ==" <=50K",] # Parte de la poblacion con ingresos <=50K
high <- data[data$income ==" >50K",] # Parte de la poblacion con ingresos >50K

#Tomemos dos muestras independientes de cada poblacion que se formo.

#Se toma el tiempo de educacion de 60 personas de cada poblacion.
low_sample <- sample(low$education.num,60)
high_sample <- sample(high$education.num,60)

# 1- Personas con bajos ingresos(<=50K).
# 2- Personas con altos ingresos(>50K).
# mu_i: promedio del tiempo de educacion de la muestra de tipo i.

# Se quiere comprobar la siguiente hipotesis
# H0: mu1 = mu2
# H1: mu1 != mu2
alpha = 0.05 #Estandar
# Por lo que hacemos un llamado a la funcion auxiliar creada
result <- mean_test(low_sample,high_sample,alpha,"two.sided")
if(result == TRUE){ # Se mantiene H0
  print("No existen diferencias significativas")
}else # Se descarta H0
{ 
  print("Existen diferencias significativas")
}

