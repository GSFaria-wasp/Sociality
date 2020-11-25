
library(MuMIn)
library(arm)
library(foreign)
library(MASS)
library(ordinal)
library(ggplot2)
library(yhat)
library(gridExtra)
library(data.table)
library(splitstackshape)
library(dplyr)
library(moments)
library(lme4)


#standard error function will use
st.err<- function( x, na.rm=FALSE) {
  if( na.rm==TRUE) x<- na.omit( x)
  sd( x)/sqrt( length( x))
}

################################################################
#Prediction 1. SIAS is associated with gender and dispersal status
################################################################
data_a<-read.csv("SIAS.csv", header=T)
names(data_a)

#remove non-binary and prefer not to say
data_b<-subset(data_a, Gender!= "Non-binary")
data_c<-subset(data_b, Gender!="Prefer not to say")

#prepare input variables
Male<-ifelse(data_c$Gender=="Male",1,0);Male
data_c$cMale<-Male - (mean(Male))

Disp_Country<- ifelse( data_c$ Country_home=="No",1,0); Disp_Country
data_c$cDispCountry<- Disp_Country - (mean(Disp_Country))

Disp_City<- ifelse( data_c$City_home == "No",1,0); Disp_City
data_c$cDispCity<- Disp_City - (mean(Disp_City))

#look at the data
hist(data_c$Sum_SIAS)
quantile(data_c$Sum_SIAS)
mean(data_c$Sum_SIAS)
st.err(data_c$Sum_SIAS)
skewness(data_c$Sum_SIAS)
range(data_c$Sum_SIAS)

#running this as a normal model since SIAS is so normally distributed - but we pre-registered a clm. 
model.1<- lm(Sum_SIAS ~ cMale + cDispCity + Age, na.action=na.fail,data=data_c)
model.set<-dredge(model.1, REML=FALSE)
top.models<-get.models(model.set, subset=delta<2)
a<-model.avg(top.models, adjusted=FALSE, revised.var=TRUE)
summary(a)
confint(a, full=T)

################################################################
#Prediction 2. Social risk is associated with gender and dispersal status
################################################################

data_d<-read.csv("exp_data.csv", header=T)
names(data_d)

#creating the intransitive measure

for (i in 1:length(data_d$ProlificID)) {
  x = 0
  for (j in 0:9) {
    if (data_d[i,23+j]==data_d[i,23+j+1]) {
      data_d$Intransitive_Count_1[[i]] <- x
    } else {x = x + 1; data_d$Intransitive_Count_1[[i]] <- x}
  }}
for (i in 1:length(data_d$ProlificID)) {
  x = 0
  for (j in 0:9) {
    if (data_d[i,44+j]==data_d[i,44+j+1]) {
      data_d$Intransitive_Count_2[[i]] <- x
    } else {x = x + 1; data_d$Intransitive_Count_2[[i]] <- x}
  }}

#removing non-binary and prefer not to say from Gender responses

data_e<-subset(data_d, Gender!= "Non-binary")
data_f<-subset(data_e, Gender!="Prefer not to say")

#let's remove intransitive above 3:

remove_intrasient <- data_f[(data_f$Intransitive_Count_1>3 | data_f$Intransitive_Count_2>3),]
data_g = data_f[!(data_f$ProlificID %in% remove_intrasient$ProlificID),]

#let's remove participants that did not pass the compreehension test:

remove_compreehension <- data_g[(data_g$Incorrect_11==1 | data_g$Incorrect_12==1 | 
                                   data_g$Incorrect_21==1 | data_g$Incorrect_22==1),]
data_h = data_g[!(data_g$ProlificID %in% remove_compreehension$ProlificID),]

#prepare input variables
Male<-ifelse(data_h$Gender=="Male",1,0);Male
data_h$cMale<-Male - (mean(Male))

Disp_Country<- ifelse( data_h$ Country_home=="No",1,0); Disp_Country
data_h$cDispCountry<- Disp_Country - (mean(Disp_Country))

Disp_City<- ifelse( data_h$City_home == "No",1,0); 
data_h$cDispCity<- Disp_City - (mean(Disp_City))

data_h$risk_d<-(data_h$Threshold.1 - data_h$Threshold)

#look at the data
hist(data_h$risk_d)
quantile(data_h$risk_d)
mean(data_h$risk_d)
st.err(data_h$risk_d)
skewness(data_h$risk_d)
range(data_h$risk_d)

#running this as a normal model since threshold is so normally distributed - but we pre-registered a clm. 
model.2 <- lm(risk_d ~ 
                Threshold + cMale +
                 Age + Disp_City, 
               data=data_h, 
               na.action=na.fail ); summary(model.2)
model.set<-dredge(model.2, REML=FALSE)
top.models<-get.models(model.set, subset=delta<2)
a<-model.avg(top.models, adjusted=FALSE, revised.var=TRUE)
summary(a)
confint(a, full=T)