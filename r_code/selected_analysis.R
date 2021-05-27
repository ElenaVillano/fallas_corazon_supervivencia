# GRAFICOS A COLOCAR EN ESCRITO
rm(list=ls())
library(tidyverse)
library(patchwork)
library(ggplot2)
library(GGally)
#library(ggfortify)
library(survminer)
library(survival)
library(KMsurv)
library(actuar)
library(BGPhazard)
library(rmutil)
theme_set(theme_minimal(base_size = 14))

corazones <- read.csv('/Volumes/MemoriaEle/HeavyData/heart_failure_clinical_records_dataset.csv')

# PREPARANDO VARIABLES
# nombres
corazones <- corazones %>% 
  rename(edad = age,
         sexo = sex,
         anemia = anaemia,
         fumar = smoking,
         presion_alta = high_blood_pressure,
         salida_sangre = ejection_fraction,
         plaquetas = platelets,
         nivel_creati = serum_creatinine,
         nivel_sodio = serum_sodium,
         enzima_cpk = creatinine_phosphokinase,
         fallecimiento = DEATH_EVENT,
         tiempo  = time)

# Logaritmo
corazones$enzima_cpk_log <- log(corazones$enzima_cpk)
corazones$plaquetas_log <- log(corazones$plaquetas)
corazones$log_time <- log(corazones$tiempo)

corazones$deceso <- as.factor(corazones$fallecimiento)

# Agreando id
corazones$sujetos <- seq(1,299,1)

# EXPLORATORIO

# Edad y sexo
pdf(file='../docs/images/edad_sexo.pdf',width = 5.5,height = 3)
layout(matrix(c(1,2,2),ncol=3))
par(mar=c(2,2,1,0),oma=c(0.5,0.5,0.5,0.5))
var <- table(corazones$sexo)
barplot(var,col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Sexo')
text(0.7,as.numeric(var[1])-10,as.numeric(var[1]))
text(1.9,as.numeric(var[2])-10,as.numeric(var[2]))
hist(corazones$edad,breaks = 25, col='blue', border= 'blue4', main='Edad')
dev.off()


# Variables continuas
pdf(file='../docs/images/corrplot.pdf',width = 6.5,height = 6.5)
ggpairs(corazones, columns=c(3,5,7,8,9),
        aes(color=as.factor(sexo),alpha=0.5))
dev.off()

# Variables boleanas
pdf(file='../docs/images/binary.pdf',width = 4.5,height = 4.5)
layout(matrix(c(1:4),ncol=2))
par(mar=c(2,3,2,0),oma=c(0.5,0.5,0.5,0.5))
var <- table(corazones$anemia)
barplot(var,col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Anemia')
text(0.7,as.numeric(var[1])-10,as.numeric(var[1]))
text(1.9,as.numeric(var[2])-10,as.numeric(var[2]))

var <- table(corazones$presion_alta)
barplot(var,col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Hipertensión')
text(0.7,as.numeric(var[1])-10,as.numeric(var[1]))
text(1.9,as.numeric(var[2])-10,as.numeric(var[2]))

var <- table(corazones$diabetes)
barplot(var,col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Diabético')
text(0.7,as.numeric(var[1])-10,as.numeric(var[1]))
text(1.9,as.numeric(var[2])-10,as.numeric(var[2]))

var <- table(corazones$fumar)
barplot(var,col='orange',
        border= 'orange', ylim=c(0,200),xlim=c(0,3),
        main = 'Fuma')
text(0.7,as.numeric(var[1])-10,as.numeric(var[1]))
text(1.9,as.numeric(var[2])-10,as.numeric(var[2]))
dev.off()

# CENSURA
ggplot(corazones, aes(x=tiempo,y=sujetos))+
  geom_linerange(aes(xmin=0,xmax=tiempo,
                     color=deceso),alpha=0.7)+
  geom_point(aes(color=deceso), size=1, shape=20)+
  xlab('Días seguimiento')+
  ylab('Pacientes') + 
  theme(legend.position = c(0.8, 0.3))
ggsave('censura.pdf',  path = '../docs/images/')



# VERIFICACIÓN DE SUPUESTOS 

xfit <- survfit(Surv(tiempo, fallecimiento) ~ 1, 
                conf.type = "log-log", data = corazones)

tj <- xfit$time[xfit$n.event!=0]
sj <- xfit$surv[xfit$n.event!=0]
k <- length(tj)
sje<-0.5*sj+0.5*c(1,sj[-k]) # supervivencia corregida

#tj<-xfit$time[(n[l]+1):n[l+1]]
#sj<-xfit$surv[(n[l]+1):n[l+1]]

holi <- rgb(58,219,197,maxColorValue = 255,alpha=50)
holi2 <- rgb(58,219,197,maxColorValue = 255,alpha=100)

pdf(file='../docs/images/supuestos.pdf',width = 5,height = 5)
layout(matrix(c(1,1,2,2,
                0,3,3,0),ncol=4,byrow = T))
par(mar=c(4, 4.1, 3, 1.5),oma=c(0,0,0,0))
#-Verificacin de supuesto exponencial-
plot(tj,log(sje), main='Exponencial',
     pch=21,bg=holi,col=holi2,cex=2,axes=F)
axis(1)
axis(2,las=2)

#-Verificacin de supuesto weibull-
plot(log(tj),log(-log(sje)), main='Weibull',
     pch=21,bg=holi,col=holi2,cex=2,axes=F)
axis(1)
axis(2,las=2)

#-Verificacin de supuesto lognormal-
qqnorm(log(tj), main='Lognormal',
       pch=21,bg=holi,col=holi2,cex=2,axes=F)
axis(1)
axis(2,las=2)
dev.off()


# PROBABILIDAD DE SUPERVIVENCIA


# Kaplan-Meier 
t <- Surv(corazones$tiempo, corazones$fallecimiento)
xfit <- survfit(Surv(tiempo, fallecimiento) ~ 1, 
                conf.type = "log-log", data = corazones)
xfit
summary(xfit)

p <- ggsurvplot(xfit, 
                conf.int = TRUE, 
                conf.int.fill = "red", 
                break.time.by = 50, 
                legend.title = "", 
                legend.labs = c("Probabilidad de supervivencia general")) #@$plot
p



survdiff(Surv(time,censor)~factor(race),rho=0,data=uis)

survdiff(Surv(time,censor)~factor(race),rho=1,data=uis)



t <- Surv(corazones$tiempo, corazones$fallecimiento)
xfitp<-survreg(t~1,dist="lognormal")

mu<-xfitp$coefficients
sigma<-xfitp$scale

tt<-seq(0,300,,1000)

ft<-dlnorm(tt,mu,sigma)
superv<-1-plnorm(tt,mu,sigma)
riesgo<-ft/superv


pdf(file='../docs/images/funciones.pdf',width = 4,height = 6)
layout(1:3)
par(mar=c(3,5,2,1))
plot(tt,ft,type="l", main='Lognormal', 
     xlab = 'Tiempo', ylab = 'f(t)',axes=F)
axis(1)
axis(2,las=2)
plot(tt,superv,type='l', main='Supervivencia', 
     xlab = 'Tiempo', ylab = 'S(t)',axes=F)
axis(1)
axis(2,las=2)
plot(tt,riesgo,type='l', main='Riesgo', 
     xlab = 'Tiempo', ylab = 'h(t)',axes=F)
axis(1)
axis(2,las=2)
dev.off()







