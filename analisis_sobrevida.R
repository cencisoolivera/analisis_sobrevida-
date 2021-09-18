# El objetivo es desarrollar un análisis del tiempo al evento retiro del ventilador. El grupo que se identifica en el campo evento, define que el paciente ha sido extubado 
# El tiempo máximo de observación es de 30 días.
surv1<-read.csv("Desktop/sobrevida/tablas/sobrevida_1.csv", header = TRUE, sep = ";") # Carga la base de datos
head(surv1)
ventilacion<-as.numeric(surv1[,"ventilacion"]) # Corresponde al tiempo máximo de ventilación mecánica invasiva
evento<-as.numeric(surv1[,"evento"]) # 1= el paciente fue extubado    0= No extubado, tiene ventilación prolongada
evento1<-as.numeric(surv1[,"evento1"])
evento1B<-as.numeric(surv1[,"evento1B"])
condicion<-as.factor(surv1[,"condicion"]) # corresponde a la condiciój de egreso 1 = fallece,   0 = egresa vivo 
hemo<-as.factor(surv1[,"hemo10"]) # pruebo la henoglobina con un valor de 10 gr 
hist(ventilacion)

renal<-as.factor(surv1[,"fallaren"]) # falla renal, identificada por la elevación de creatinina.
hemo7<-as.factor(surv1[,"hemo7"]) # cambia el valor de henoglobina a un vaor de 7 gr
hiperglic<-as.factor(surv1[,"hiperglicemia"]) # durante la hospitalización el azucar subre de 180 mg /dl. Este umbral corresponde a un punto donde el azucar empiez a ser filtrada en la orina por lo alta que está 
hiperna<-as.factor(surv1[,"hiperna"]) # elevación del sodio por encima de 150

hiperta<-as.factor(surv1[,"hta"]) # tiene antecedente de hipertensión arterial
diabetes<-as.factor(surv1[,"dm"]) # tiene antecedente de diqbetes
epoc<-as.factor(surv1[,"epoc"]) # Tiene antecedente de enfermedad pulmonar obstructuva crónica 
hipotiroidismo<-(surv1[,"hipotiroidismo"]) # Tiene enfermedad del tiroides
obesidad<-as.factor(surv1[,"obesidad"]) # identifican sobrepeso en la historia
fallavi<-as.factor(surv1[,"disfuncionvi"]) # falla del lado izquierdo del corazón

fallavd1<-as.factor(surv1[,"disfuncionvd1"]) # Falla del lado derecho del corazón
infeccion<-as.factor(surv1[,"infeccion"]) # Se infectó durante la hospitalizaicñon
evento<-as.numeric(surv1[,"evento"]) # Define la variable de fallos 
fallo<-as.numeric(surv1[,"evento"])

library("survival")
library("ggplot2")
library("ggpubr")

# Estos tres comandos generan la curva de Kaplan Meier, la gráfican y luego realizan un log Rank Test, con un valor de 0,05 como umbral . Pruebo cada condición

km_fit <- survfit(Surv (ventilacion, fallo) ~ hiperna)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ hemo, rho=0)

km_fit <- survfit(Surv (ventilacion, fallo) ~ hemo)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ hemo, rho=0) #significativo p=0,002

km_fit <- survfit(Surv (ventilacion, fallo) ~ hemo7)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ hemo7, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ hiperglic)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ hiperglic, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ hiperna)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ hiperna, rho=0) # significativo p=0,008

km_fit <- survfit(Surv (ventilacion, fallo) ~ hiperta)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ hiperta, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ diabetes)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ diabetes, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ epoc)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ epoc, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ hipotiroidismo)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ hipotiroidismo, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ obesidad)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ obesidad, rho=0) 


km_fit <- survfit(Surv (ventilacion, fallo) ~ fallavi)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ fallavi, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ fallavd)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ fallavd, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ fallavd1)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ fallavd1, rho=0) 

km_fit <- survfit(Surv (ventilacion, fallo) ~ infeccion)
plot(km_fit)
survdiff(Surv(ventilacion,fallo)~ infeccion, rho=0) #significativo p=0,00000000

install.packages("survminer")
library(survminer)

# Genera el modelo de regresión de cox, realicé uno con cada variable y luego un modelo con todas, Solo hay NULL en ecocardiograma. A todos no se les tomó 
cox<- coxph(Surv(ventilacion, fallo)~  fallavd1 + hemo + infeccion + hiperta + hiperna + diabetes + epoc + obesidad, data= surv1, exclude= NULL )
summary(cox)

# genera una evaluación y graficación del ajuste del modelo, imprime los valores y grafica
fit<-cox<- coxph(Surv(ventilacion, fallo)~ fallavd1 )
temp <- cox.zph(fit)
print(temp)
plot(temp)

