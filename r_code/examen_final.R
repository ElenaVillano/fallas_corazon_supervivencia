directorio <- "/home/alfie-gonzalez/Documentos/Maestría/Cuarto Semestre/Temas Selectos de Estadística"
setwd(directorio)

library(tidyverse)
library(ggplot2)
library(ggfortify)
library(survminer)

library(survival)
library(KMsurv)
library(actuar)
library(BGPhazard)
library(rmutil)

datos <- read_csv("heart_failure_clinical_records_dataset.csv")
datos

datos %>% str()

#anaemia, diabetes, high_blood_pressure, sex, smoking

#Kaplan Meier

#sin agrupar

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~1, 
                conf.type = "log-log", data = datos)
xfit
summary(xfit)

p <- ggsurvplot(xfit, 
           conf.int = TRUE, 
           conf.int.fill = "red", 
           break.time.by = 50, 
           legend.title = "", 
           legend.labs = c("km"))$plot
p

#anaemia

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~anaemia, 
                conf.type = "log-log", data = datos)
ggsurvplot(xfit, 
           conf.int = TRUE, 
           break.time.by = 50, 
           legend.title = ""
           )

#diabetes

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~diabetes, 
                conf.type = "log-log", data = datos)
ggsurvplot(xfit, 
           conf.int = TRUE, 
           break.time.by = 50, 
           legend.title = ""
)

#high_blood_pressure

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~high_blood_pressure, 
                conf.type = "log-log", data = datos)
ggsurvplot(xfit, 
           conf.int = TRUE, 
           break.time.by = 50, 
           legend.title = ""
)

#sex

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~sex, 
                conf.type = "log-log", data = datos)
ggsurvplot(xfit, 
           conf.int = TRUE, 
           break.time.by = 50, 
           legend.title = ""
)

#smoking

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~smoking, 
                conf.type = "log-log", data = datos)
ggsurvplot(xfit, 
           conf.int = TRUE, 
           break.time.by = 50, 
           legend.title = ""
)

#Vida acelerada

graficar_supervivencia <- function(xfit, xfitp, variable, include.km = FALSE){
  
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

#sin agrupar

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~1, 
                conf.type = "log-log", data = datos)
xfitp <- survreg(t~1, dist = "weibull", data = datos)

xfitp <- survreg(t~1, dist = "lognormal", data = datos)
xfitp <- survreg(t~1, dist = "loglogistic", data = datos)
xfitp <- survreg(t~age+anaemia+
                   creatinine_phosphokinase+diabetes+ejection_fraction+
                   high_blood_pressure+platelets+serum_creatinine+
                   serum_sodium+sex+smoking, dist = "weibull", data = datos)
xfitp <- survreg(t~age+anaemia+
                   creatinine_phosphokinase+ejection_fraction+
                   high_blood_pressure+serum_creatinine+
                   serum_sodium+smoking, dist = "weibull", data = datos)

xfitp
summary(xfitp)

df_km <- tibble(time = xfit$time, 
                surv = xfit$surv, 
                lower = xfit$lower, 
                upper = xfit$upper)

pct <- seq(0.01, 0.99, by = 0.01)
xpredp <- predict(xfitp, 
                  type = "quantile", 
                  p = pct, 
                  se = TRUE)

df <- tibble(y = 1 - pct, 
             time = xpredp$fit[1,], 
             lower = xpredp$fit[1,]-2*xpredp$se.fit[1,], 
             upper = xpredp$fit[1,]+2*xpredp$se.fit[1,]
             )

p <- ggsurvplot(xfit, 
                conf.int = TRUE, 
                conf.int.fill = "red", 
                break.time.by = 50, 
                legend.title = "", 
                legend.labs = c("km"))$plot

p + geom_ribbon(data = df, aes(xmin = lower, xmax = upper, y = y, 
                               fill = "weibull_ic"), 
                fill = "blue", 
                show.legend = FALSE, 
                alpha = 0.15) + 
  geom_line(data = df, aes(x = time, y = y, color = "weibull_fit")) + 
  scale_color_manual(name = "método", 
                     values = c("km" = "red", 
                                "weibull_fit" = "blue"), 
                     breaks = c("km", 
                                "weibull_fit"), 
                     labels = c("km", 
                                "weibull"
                     )
  )

#anaemia  
  
t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~anaemia, 
                conf.type = "log-log", data = datos)
xfitp <- survreg(t~anaemia, dist = "weibull", data = datos)

xfit
summary(xfit)

variable <- "anaemia"
graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

#diabetes

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~diabetes, 
                conf.type = "log-log", data = datos)
xfitp <- survreg(t~diabetes, dist = "weibull", data = datos)

xfit
summary(xfit)

variable <- "diabetes"
graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

#high_blood_pressure

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~high_blood_pressure, 
                conf.type = "log-log", data = datos)
xfitp <- survreg(t~high_blood_pressure, dist = "weibull", data = datos)

xfit
summary(xfit)

variable <- "high_blood_pressure"
graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

#sex

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~sex, 
                conf.type = "log-log", data = datos)
xfitp <- survreg(t~sex, dist = "weibull", data = datos)

xfit
summary(xfit)

variable <- "sex"
graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

#smoking

t <- Surv(datos$time, datos$DEATH_EVENT)
xfit <- survfit(Surv(time, DEATH_EVENT)~smoking, 
                conf.type = "log-log", data = datos)
xfitp <- survreg(t~smoking, dist = "weibull", data = datos)

xfit
summary(xfit)

variable <- "smoking"
graficar_supervivencia(xfit, xfitp, variable, include.km = TRUE)
graficar_supervivencia(xfit, xfitp, variable, include.km = FALSE)

#Riesgos proporcionales

graficar_supervivencia <- function(xfitc, variable){

  newdata <- data.frame(x = 0)
  names(newdata) <- c(variable)
  pred <- survfit(xfitc, newdata = newdata, conf.type = "log-log")
  
  df <- tibble(
    time = pred$time, 
    y = pred$surv, 
    lower = pred$lower, 
    upper = pred$upper
  )
  
  df_0 <- df
  
  newdata <- data.frame(x = 1)
  names(newdata) <- c(variable)
  pred <- survfit(xfitc, newdata = newdata, conf.type = "log-log")
  
  df <- tibble(
    time = pred$time, 
    y = pred$surv, 
    lower = pred$lower, 
    upper = pred$upper
  )
  
  df_1 <- df
  
  ggplot() + 
    geom_ribbon(data = df_0, aes(x = time, ymin = lower, ymax = upper), 
                show.legend = FALSE, 
                fill = "red", 
                alpha = 0.3) + 
    geom_line(data = df_0, aes(x = time, y = y, color = "x_0")) + 
    geom_ribbon(data = df_1, aes(x = time, ymin = lower, ymax = upper), 
                show.legend = FALSE, 
                fill = "blue", 
                alpha = 0.3) + 
    geom_line(data = df_1, aes(x = time, y = y, color = "x_1")) + 
    ylim(0, 1) + 
    scale_x_continuous(breaks = seq(from = 0, to = 350, by = 50)) + 
    xlab("Time") + 
    ylab("Survival probability") + 
    ggtitle("Supervivencia usando riesgos proporcionales") + 
    scale_color_manual(
      name = "", 
      values = c("x_0" = "red", 
                 "x_1" = "blue"), 
      breaks = c("x_0", "x_1"), 
      labels = c("x_0" = paste0(variable, "=0"), 
                 "x_1" = paste0(variable, "=1"))
    ) + 
    theme_classic()
  
}

#sin agrupar

t <- Surv(datos$time, datos$DEATH_EVENT)
xfitc <- coxph(t~age+anaemia+
                 creatinine_phosphokinase+ejection_fraction+
                 high_blood_pressure+serum_creatinine+
                 serum_sodium+smoking, data = datos)

xfitc
summary(xfitc)

plot(survfit(xfitc), yaxt = "n", 
     xlab = "Time", 
     ylab = "Survival probability", 
     main = "Supervivencia utilizando riesgos proporcionales", 
     font.main = 1, 
     lty = c(1, 1, 1), 
     lwd = c(2, 2, 2), 
     col = c(2, 2, 2))
axis(2, at = seq(0, 1, by = 0.25), las = 2)
     
#anaemia

xfitc <- coxph(t~anaemia, data = datos)

xfitc
summary(xfitc)

variable <- "anaemia"
graficar_supervivencia(xfitc, variable)

#diabetes

xfitc <- coxph(t~diabetes, data = datos)

xfitc
summary(xfitc)

variable <- "diabetes"
graficar_supervivencia(xfitc, variable)

#high_blood_pressure

xfitc <- coxph(t~high_blood_pressure, data = datos)

xfitc
summary(xfitc)

variable <- "high_blood_pressure"
graficar_supervivencia(xfitc, variable)

#sex

xfitc <- coxph(t~sex, data = datos)

xfitc
summary(xfitc)

variable <- "sex"
graficar_supervivencia(xfitc, variable)

#smoking

xfitc <- coxph(t~smoking, data = datos)

xfitc
summary(xfitc)

variable <- "smoking"
graficar_supervivencia(xfitc, variable)

