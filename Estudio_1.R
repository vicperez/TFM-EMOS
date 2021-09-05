setwd("C:/Users/JohnDoe/Documents")
library(tidyverse)
library(xlsx)
library(multilevel)
library(lattice)

df <- read.csv("gm_df.csv")
df <- df[,-1]

df1 <- df[,c(1,5:51)]
df1 <- unique(df1)
rownames(df1) <- 1:nrow(df1)
df1$SUBNUM <- as.numeric(rownames(df1))

df2 <- df[,c(1:3)]
df2 <- df2[order(df2$fecha_informe),]

df2 <- pivot_wider(df2, names_from = fecha_informe,
                   values_from = tasa14)
df <- merge(df1, df2, by = "zona_basica_salud")
df <- df[,-1]
df <- df %>%
  dplyr::select(SUBNUM, everything())

df <- df[,-c(49:65,90:175)]




# Transformación de la base de datos a una estructura univariada

univ.grow <- make.univ(df, df[,49:72])


# Visualización exploratoria evolución de la tasa de contagios en cada zbs
tiff('figure4.tiff', units= "mm", width= 200, height = 160, res=300, compression = 'lzw')
xyplot(MULTDV~TIME | as.factor(SUBNUM), data = univ.grow, 
       type = c("p", "r", "g"), col = "blue", col.line = "black",
       xlab = "Tiempo", ylab = "Tasa de contagio", main = 'Tabla X. Evolución de la tasa de contagio por zbs')
dev.off()


# Step 1: ICC1

null.model <- lme(MULTDV~1, random=~1|SUBNUM, data = univ.grow,
                  na.action = na.omit, control = list(opt = "optim"))
VarCorr(null.model)       

4620.769/(4620.769+21799.453) #0.174



# Step 2: Estudio del tipo de evolución temporal
model.tiemp.lineal <- lme(MULTDV~TIME, random =~ 1 | SUBNUM, data = univ.grow,
                          na.action = na.omit, control =list(opt = "optim"))
model.tiemp.cuadrado <- update(model.tiemp.lineal,random =~ TIME + I (TIME^2) | SUBNUM)
summary(model.tiemp.cuadrado)
model.tiemp.cubico <- update(model.tiemp.lineal,random =~ TIME + I (TIME^3)| SUBNUM)
summary(model.tiemp.cubico)
model.tiemp.cuarta <- update(model.tiemp.lineal,random =~ TIME + I (TIME^4)| SUBNUM)
summary(model.tiemp.cuarta)


# Contraste
anova(model.tiemp.lineal,model.tiemp.cuadrado,model.tiemp.cubico,model.tiemp.cuarta)


# Step 3: Variabilidad de la pendiente del modelo

model.3b <- update(model.2b, random=~TIME|SUBNUM) 
summary(model.3b)

anova(model.2b, model.3b)  



# Step 4:  Estructuras de error de modelado
model.4a <- update(model.3b, correlation = corAR1()) 
anova(model.3b, model.4a) 

summary(model.4a) 


# Examinar la homocedasticidad en el tiempo

tapply(univ.grow$MULTDV,univ.grow$TIME, var, na.rm = T) 


model.4b <- update(model.4a, weights = varFixed(~SUBNUM)) 
anova(model.4a, model.4b)
summary(model.4b)

# Step 5: Predicción de la variación del intercepto

# MODELO 1
model.5b <- lme(MULTDV~TIME + I (TIME^2), random =~TIME | SUBNUM, 
                correlation=corAR1(), na.action = na.omit, data = univ.grow,
                control = list(opt="optim"))

round(summary(model.5b)$tTable, dig = 2)

AIC(model.5b)
BIC(model.5b)
logLik(model.5b)


# MODELO 2
model.5 <- lme(MULTDV~TIME + I (TIME^2) + edu0  + imig_pr + tama_hog
               + env_pro,  random=~TIME|SUBNUM, 
               correlation=corAR1(), na.action = na.omit, data = univ.grow,
               control = list(opt="optim"))
round(summary(model.5)$tTable, dig = 2)

AIC(model.5)
BIC(model.5)
logLik(model.5)


# MODELO 3
model.int.edu <- lme(MULTDV~TIME + I (TIME^2) + edu0 + imig_pr+
                       tama_hog + env_pro  +  edu0:(TIME^2) ,  random=~TIME|SUBNUM, 
                     correlation=corAR1(), na.action = na.omit, data = univ.grow,
                     control = list(opt="optim"))
round(summary(model.int.edu)$tTable, dig = 2)

AIC(model.5)
BIC(model.5)
logLik(model.5)

# MODELO 4
model.int.env <- lme(MULTDV~TIME + I (TIME^2) + edu0 + imig_pr+
                       tama_hog + env_pro  +  env_pro:(TIME^2) ,  random=~TIME|SUBNUM, 
                     correlation=corAR1(), na.action = na.omit, data = univ.grow,
                     control = list(opt="optim"))
round(summary(model.int.env)$tTable, dig = 2)

AIC(model.5)
BIC(model.5)
logLik(model.5)


# Representación gráfica

# Educación
TDAT<-univ.grow[1:24,c("TIME","edu0","tama_hog","env_pro","imig_pr")]
TDAT
TDAT$tama_hog<-mean(univ.grow$tama_hog) #mean
TDAT$env_pro<-mean(univ.grow$env_pro) #mean
TDAT$imig_pr<-mean(univ.grow$imig_pr) #mean
TDAT$edu0<-mean(univ.grow$edu0)-sd(univ.grow$edu0)  #LOW
a <- as.data.frame(t(predict(model.int.edu,TDAT,level=0)))

write.xlsx(a, 'a.xlsx')


TDAT<-univ.grow[1:24,c("TIME","edu0","tama_hog","env_pro","imig_pr")]
TDAT
TDAT$tama_hog<-mean(univ.grow$tama_hog) #mean
TDAT$env_pro<-mean(univ.grow$env_pro) #mean
TDAT$imig_pr<-mean(univ.grow$imig_pr) #mean
TDAT$edu0<-mean(univ.grow$edu0)  #mean
b <- as.data.frame(t(predict(model.int.edu,TDAT,level=0)))

write.xlsx(b, 'b.xlsx')


TDAT<-univ.grow[1:24,c("TIME","edu0","tama_hog","env_pro","imig_pr")]
TDAT
TDAT$tama_hog<-mean(univ.grow$tama_hog) #mean
TDAT$env_pro<-mean(univ.grow$env_pro) #mean
TDAT$imig_pr<-mean(univ.grow$imig_pr) #mean
TDAT$edu0<-mean(univ.grow$edu0) + sd(univ.grow$edu0) #HIGH
c <- as.data.frame(t(predict(model.int.edu,TDAT,level=0)))

write.xlsx(c, 'c.xlsx')


# Envejecimiento
TDAT<-univ.grow[1:24,c("TIME","edu0","tama_hog","env_pro","imig_pr")]
TDAT
TDAT$tama_hog<-mean(univ.grow$tama_hog) #mean
TDAT$env_pro<-mean(univ.grow$env_pro) - sd(univ.grow$env_pro)  #LOW 
TDAT$imig_pr<-mean(univ.grow$imig_pr) #mean
TDAT$edu0<-mean(univ.grow$edu0)#mean
d <- as.data.frame(t(predict(model.int.env,TDAT,level=0)))

write.xlsx(d, 'd.xlsx')


TDAT<-univ.grow[1:24,c("TIME","edu0","tama_hog","env_pro","imig_pr")]
TDAT
TDAT$tama_hog<-mean(univ.grow$tama_hog) #mean
TDAT$env_pro<-mean(univ.grow$env_pro) #mean
TDAT$imig_pr<-mean(univ.grow$imig_pr) #mean
TDAT$edu0<-mean(univ.grow$edu0)  #mean
e <- as.data.frame(t(predict(model.int.env,TDAT,level=0)))


write.xlsx(e, 'e.xlsx')

TDAT<-univ.grow[1:24,c("TIME","edu0","tama_hog","env_pro","imig_pr")]
TDAT
TDAT$tama_hog<-mean(univ.grow$tama_hog) #mean
TDAT$env_pro<-mean(univ.grow$env_pro) + sd(univ.grow$env_pro) #HIGH
TDAT$imig_pr<-mean(univ.grow$imig_pr) #mean
TDAT$edu0<-mean(univ.grow$edu0) #mean
f <- as.data.frame(t(predict(model.int.env,TDAT,level=0)))

write.xlsx(f, 'f.xlsx')