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
  rename(sexo = sex,
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
corazones$enzima_cpk <- log(corazones$enzima_cpk)
corazones$plaquetas <- log(corazones$plaquetas)
corazones$deceso <- as.factor(corazones$fallecimiento)

# Agreando id
corazones$sujetos <- seq(1,299,1)

#graficar_supervivencia <- function(xfit, xfitp, variable, include.km = FALSE){
  
  pct <- seq(0.01, 0.99, by = 0.01)
  newdata <- list(x = rep(0, length(pct)))
  names(newdata) <- c(variable)
  xpredp <- predict(xfitp, 
                    newdata = newdata, 
                    type = "quantile", 
                    p = pct, 
                    se = TRUE)
  
  df <- tibble(y = 1 - pct, 
               time = xpredp$fit[1,], 
               lower = xpredp$fit[1,]-2*xpredp$se.fit[1,], 
               upper = xpredp$fit[1,]+2*xpredp$se.fit[1,]
  )
  
  df_0 <- df
  
  newdata <- list(x = rep(1, length(pct)))
  names(newdata) <- c(variable)
  xpredp <- predict(xfitp, 
                    newdata = newdata, 
                    type = "quantile", 
                    p = pct, 
                    se = TRUE)
  
  df <- tibble(y = 1 - pct, 
               time = xpredp$fit[1,], 
               lower = xpredp$fit[1,]-2*xpredp$se.fit[1,], 
               upper = xpredp$fit[1,]+2*xpredp$se.fit[1,]
  )
  
  df_1 <- df
  
  if(include.km == TRUE){
    
    #ggsurvplot
    
    p <- ggsurvplot(xfit, 
                    conf.int = TRUE, 
                    break.time.by = 50, 
                    legend.title = "")$plot
    
    p <- p + geom_ribbon(data = df_0, aes(xmin = lower, xmax = upper, y = y, 
                                          fill = "weibull_ic"), 
                         show.legend = FALSE, 
                         fill = "red", 
                         alpha = 0.15) + 
      geom_line(data = df_0, aes(x = time, y = y, color = "weibull_fit"), 
                color = "red") + 
      geom_ribbon(data = df_1, aes(xmin = lower, xmax = upper, y = y, 
                                   fill = "weibull_ic"), 
                  show.legend = FALSE, 
                  fill = "blue", 
                  alpha = 0.15) + 
      geom_line(data = df_1, aes(x = time, y = y, color = "weibull_fit"), 
                color = "blue")
    
    return(p)
    
  } else{
    
    p <- ggplot() + 
      geom_ribbon(data = df_0, aes(xmin = lower, xmax = upper, y = y, 
                                   fill = "weibull_ic"), 
                  show.legend = FALSE, 
                  fill = "red", 
                  alpha = 0.15) + 
      geom_line(data = df_0, aes(x = time, y = y, color = "x_0")) + 
      geom_ribbon(data = df_1, aes(xmin = lower, xmax = upper, y = y, 
                                   fill = "weibull_ic"), 
                  show.legend = FALSE, 
                  fill = "blue", 
                  alpha = 0.15) + 
      geom_line(data = df_1, aes(x = time, y = y, color = "x_1")) + 
      scale_color_manual(name = "grupo", 
                         values = c("x_0" = "red", 
                                    "x_1" = "blue"), 
                         breaks = c("x_0", 
                                    "x_1"), 
                         labels = c(paste0(variable, "=0"), 
                                    paste0(variable, "=1")
                         )
      ) + 
      xlab("Time") + 
      ylab("Survival propability") + 
      ggtitle("Supervivencia usando modelo de vida acelerada") + 
      theme_classic()
    
    return(p)
    
  }
  
}

# PROBABILIDAD DE SUPERVIVENCIA
# Kaplan-Meier 
# sexo
xfit <- survfit(Surv(corazones$tiempo, corazones$fallecimiento) ~ sexo, 
                conf.type = "log-log", data = corazones)
a1 <- ggsurvplot(xfit, 
                 conf.int = TRUE, 
                 break.time.by = 50, 
                 legend.title = "",
                 xlab='',
                 font.y = c(12, "plain", "black"),
                 font.tickslab=c(8,'plain','black'))

# anemia
xfit <- survfit(Surv(corazones$tiempo, corazones$fallecimiento)~anemia, 
                conf.type = "log-log", data = corazones)
a2 <- ggsurvplot(xfit, 
           conf.int = TRUE, 
           break.time.by = 50, 
           legend.title = "",
           font.x = c(12, "plain", "black"),
           font.y = c(12, "plain", "black"),
           font.tickslab=c(8,'plain','black'))

# diabetes
xfit <- survfit(Surv(corazones$tiempo, corazones$fallecimiento)~diabetes, 
                conf.type = "log-log", data = corazones)
a3 <- ggsurvplot(xfit, 
                 conf.int = TRUE, 
                 break.time.by = 50, 
                 legend.title = "",
                 xlab='',
                 ylab='',
                 font.tickslab=c(8,'plain','black'))

# fumar
xfit <- survfit(Surv(corazones$tiempo, corazones$fallecimiento)~fumar, 
                conf.type = "log-log", data = corazones)
a4 <- ggsurvplot(xfit, 
                 conf.int = TRUE, 
                 break.time.by = 50, 
                 legend.title = "",
                 ylab='',
                 xlab='',
                 font.tickslab=c(8,'plain','black'))

# presion alta
xfit <- survfit(Surv(corazones$tiempo, corazones$fallecimiento)~presion_alta, 
                conf.type = "log-log", data = corazones)
a5 <- ggsurvplot(xfit, 
                 conf.int = TRUE, 
                 break.time.by = 50, 
                 legend.title = "",
                 ylab='',
                 font.x = c(12, "plain", "black"),
                 font.tickslab=c(8,'plain','black'))

# nivel creatinina
breaks_2 <- c(0,1.16)
corazones$creati_intervals <- cut(corazones$nivel_creati, c(breaks_2,Inf), 
                    labels = breaks_2, include.lowest = TRUE)

xfit <- survfit(Surv(corazones$tiempo, corazones$fallecimiento)~creati_intervals, 
                conf.type = "log-log", data = corazones)
a6 <- ggsurvplot(xfit, 
                 conf.int = TRUE, 
                 break.time.by = 50, 
                 legend.title = "",
                 xlab='',
                 font.y = c(12, "plain", "black"),
                 font.tickslab=c(8,'plain','black'),
                 legend.labs = c("creati=0", 
                             "creati=1.16"))

# graficamos todos en una sola
splots <- list()

splots[[1]] <- a6
splots[[2]] <- a1
splots[[3]] <- a2
splots[[4]] <- a3
splots[[5]] <- a4
splots[[6]] <- a5

res <- arrange_ggsurvplots(splots,
                    ncol = 2, nrow = 3)
ggsave('niveles.pdf',res,path = '../docs/images/')


breaks<-c(0,30,45)
ef_intervals<-cut(corazones$salida_sangre,c(breaks,Inf), labels = breaks, include.lowest = TRUE)
corazones$ef_intervals <- ef_intervals

xfit <- survfit(Surv(tiempo, fallecimiento)~ef_intervals, 
                conf.type = "log-log", data = corazones)
a <- ggsurvplot(xfit, 
                 conf.int = TRUE, 
                 break.time.by = 50, 
                 legend.title = "",
                font.x = c(12, "plain", "black"),
                font.y = c(12, "plain", "black"),
                font.tickslab=c(8,'plain','black'))

a
ggsave('niv_creati.pdf',  path = '../docs/images/')




######### ex #

# anemia
t <- Surv(corazones$tiempo, corazones$fallecimiento)
xfit <- survfit(Surv(tiempo, fallecimiento)~anemia, 
                conf.type = "log-log", data = corazones)
xfitp <- survreg(t~anemia, dist = "weibull", data = corazones)

xfit
summary(xfit)

variable <- "anemia"
p1 <- graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
d1 <- graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

# diabetes
t <- Surv(corazones$tiempo, corazones$fallecimiento)
xfit <- survfit(Surv(tiempo, fallecimiento)~diabetes, 
                conf.type = "log-log", data = corazones)
xfitp <- survreg(t~diabetes, dist = "weibull", data = corazones)

xfit
summary(xfit)

variable <- "diabetes"
p2 <- graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
d2 <- graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

# presion alta

t <- Surv(corazones$tiempo, corazones$fallecimiento)
xfit <- survfit(Surv(tiempo, fallecimiento)~presion_alta, 
                conf.type = "log-log", data = corazones)
xfitp <- survreg(t~presion_alta, dist = "weibull", data = corazones)

xfit
summary(xfit)

variable <- "presion_alta"
p3 <- graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
d3 <- graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)


# fumar
t <- Surv(corazones$tiempo, corazones$fallecimiento)
xfit <- survfit(Surv(tiempo, fallecimiento)~fumar, 
                conf.type = "log-log", data = corazones)
xfitp <- survreg(t~fumar, dist = "weibull", data = corazones)

xfit
summary(xfit)

variable <- "fumar"
p4 <- graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
d4 <- graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)


# sexo
t <- Surv(corazones$tiempo, corazones$fallecimiento)
xfit <- survfit(Surv(tiempo, fallecimiento)~sexo, 
                conf.type = "log-log", data = corazones)
xfitp <- survreg(t~sexo, dist = "weibull", data = corazones)

xfit
summary(xfit)

variable <- "sexo"
p5 <- graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
d5 <- graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

(p1+p2)/(p3+p4)
#ggsave('km_vida.pdf',  path = '../docs/images/')

######### Modelamiento

survdiff(Surv(time,censor)~factor(race),rho=0,data=uis)

survdiff(Surv(time,censor)~factor(race),rho=1,data=uis)

# REGRESION MODELOS

t <- Surv(corazones$tiempo, corazones$fallecimiento)
#todos
# el que tengo
# el que tiene solo las buenas de variables cont
# anterior más discretas
# 8 modelos. 
xfitc <- coxph(t ~ edad +
                 anemia +
                 enzima_cpk +
                 presion_alta +
                 salida_sangre +
                 nivel_creati +
                 diabetes + 
                 plaquetas +
                 nivel_sodio +
                 fumar, data = corazones)
summary(xfitc)




t <- Surv(corazones$tiempo, corazones$fallecimiento)



xfitc <- coxph(t ~ age +
                 anemia +
                 enzima_cpk +
                 presion_alta +
                 salida_sangre +
                 nivel_creati +
                 plaquetas +
                 nivel_sodio, data = corazones)
summary(xfitc)
xfitc$loglik
#xfitc <- coxph(t ~ age +
#                 anemia +
#                 enzima_cpk, data = corazones)
#summary(xfitc)

ftest <- cox.zph(xfitc)

ggcoxzph(ftest)


xfitc <- coxph(t ~ age +
                 anemia +
                 enzima_cpk +
                 presion_alta +
                 salida_sangre +
                 nivel_creati, data = corazones)
summary(xfitc)

cox_fit <- survfit(xfitc)

plot(cox_fit, main = "cph model", xlab="Days")


autoplot(cox_fit)
e<-residuals(xfitc)
plot(e)
plot(e,corazones$enzima_cpk)
#Comparación de 7 niveles de estres 
survdiff(Surv(BreakTimes,seq(1,1,,length(BreakTimes)))~VoltageLev,data=breakdown)












