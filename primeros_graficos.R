library(tidyverse)
corazones <- read.csv('/Volumes/MemoriaEle/HeavyData/heart_failure_clinical_records_dataset.csv')
head(corazones)
summary(corazones)
dim(corazones)
names(corazones)

layout(matrix(c()))


# Conjunto de datos de registros clínicos de fallas del corazón
# Tenemos 13 variables
# 299 observaciones


# Tenemos el evento que fue la muerte del individuo
layout(matrix(c(1:2),ncol=1))
barplot(table(corazones$DEATH_EVENT),col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Evento: Fallecimiento del individuo')
# Periodo de seguimiento
hist(corazones$time,breaks = 50, 
     col='blue', border= 'blue4', main='Tiempo (días)')

# Información general por individuo
layout(matrix(c(1:2),ncol=2))
barplot(table(corazones$sex),col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Sexo')
hist(corazones$age,breaks = 25, col='blue', border= 'blue4', main='Edad')



# VARIABLES EXPLICATIVAS BOOLEANAS

layout(matrix(c(1:4),ncol=2))

barplot(table(corazones$anaemia),col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Anemia')

barplot(table(corazones$high_blood_pressure),col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Hipertensión')

barplot(table(corazones$diabetes),col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Diabético')

barplot(table(corazones$smoking),col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Fuma')



# VARIABLES EXPLICATIVAS CONTINUAS de características de la sange del individuo
layout(matrix(c(1:3),ncol=1))


hist(corazones$ejection_fraction,breaks = 30, 
     col='blue', border= 'blue4', main='Fracción de eyección',ylab=' ')

hist(corazones$platelets,breaks = 30, 
     col='blue', border= 'blue4', main='Plaquetas en la sangre',ylab=' ')

hist(corazones$serum_creatinine,breaks = 50, 
     col='blue', border= 'blue4', main='Nivel de creatinina en la sangre',ylab=' ')

layout(matrix(c(1:2),ncol=1))

hist(corazones$serum_sodium,breaks = 30, 
     col='blue', border= 'blue4', main='Nivel de sodio en la sangre',ylab=' ')

hist(corazones$creatinine_phosphokinase,breaks = 30, 
     col='blue', border= 'blue4', main='Nivel de la encima CPK en la sangre',ylab=' ')
