library(reshape2)
fuel<- melt(fuel, id.vars = c("X__1", "Country"))
CO2<- melt(CO2, id.vars = c("X__1", "Time"))
PM2_5<- melt(PM2_5, id.vars = c("ISO 3166-1.x.x", "Name"))
PM10<- melt(PM10, id.vars = c("code", "Name"))
GDP<- melt(GDP, id.vars = c("Country", "Abbr"))
write.csv(GDP, "C:/Users/15221/Desktop/GDP.csv")
HDI<- melt(HDI, id.vars = c("Code", "Country"))


fuel$Key<-paste(fuel$Code,fuel$Year,sep='')
CO2$Key<-paste(CO2$Code,CO2$Year,sep='')
PM25$Key<-paste(PM25$Code,PM25$Year,sep='')
PM10$Key<-paste(PM10$Code,PM10$Year,sep='')
HDI$Key<-paste(HDI$Code,HDI$Year,sep='')

fc<-merge(fuel,CO2, by=c("Key"), all.x=TRUE)
typeof(fc$CO2)
fc$CO2<-as.double(fc$CO2)
mod1=lm(CO2~Fuel,data=fc)
install.packages("stargazer")
library(stargazer)
stargazer (mod1, type="text", align=TRUE)

fcpm<-merge(fc,PM25, by=c("Key"), all.x=TRUE)
fcpm<-merge(fcpm,PM10, by=c("Key"), all.x=TRUE)
mod2=lm(PM25~Fuel,data=fcpm)
stargazer (mod2, type="text", align=TRUE)
mod3=lm(PM10~Fuel,data=fcpm)
stargazer (mod3, type="text", align=TRUE)
fcpmh<-merge(fcpm,HDI, by=c("Key"), all.x=TRUE)
typeof(fcpmh$HDI)
fcpmh$HDI<-as.double(fcpmh$HDI)
library(QuantPsyc)
mod4=lm(Fuel~HDI,data=fcpmh)
stargazer (mod4, type="text", align=TRUE)
mod5=lm(PM10~PM25,data=fcpmh)
mod6=lm(CO2~HDI,data=fcpmh)
mod7=lm(PM25~HDI,data=fcpmh)
mod8=lm(PM10~HDI,data=fcpmh)
stargazer (mod1, mod2, mod3,mod4, type="text", align=TRUE)
stargazer (mod5, type="text", align=TRUE)
mod9=lm(CO2~Fuel+HDI,data=fcpmh)
mod10=lm(PM25~Fuel+HDI,data=fcpmh)
mod11=lm(PM10~Fuel+HDI,data=fcpmh)
stargazer (mod9,mod10,mod11, type="text", align=TRUE)

library(QuantPsyc)
install.packages("QuantPsyc")
lm.beta(mod4)
stargazer (mod5, type="text", align=TRUE)
write.csv(fcpmh, "C:/Users/15221/Desktop/sum.csv")

install.packages("lfe")
library(lfe)
mod1<-felm(CO2 ~ Fuel| Year, data=sum)
mod2=felm(PM25~Fuel| Year,data=sum)
mod3=felm(PM10~Fuel| Year,data=sum)
stargazer (mod1,mod2,mod3, type="text", align=TRUE)
mod4=felm(PM10~PM25| Year+Code,data=sum)
stargazer (mod4, type="text", align=TRUE)
mod5=felm(Fuel~GDP| Year+Code,data=sum)
stargazer (mod5, type="text", align=TRUE)
mod17=felm(Fuel~sqrt(GDP)| Year+Code,data=sum)
stargazer (mod17, type="text", align=TRUE)

mod9=felm(CO2~Fuel+sqrt(GDP)| Year,data=sum)
mod10=felm(PM25~Fuel+sqrt(GDP)| Year,data=sum)
mod11=felm(PM10~Fuel+sqrt(GDP)| Year,data=sum)
stargazer (mod9,mod10,mod11, type="text", align=TRUE)
mod9=lm(CO2~Fuel+GDP,data=sum)
mod10=lm(PM25~Fuel+GDP,data=sum)
mod11=lm(PM10~Fuel+GDP,data=sum)

lm.beta(mod9)
lm.beta(mod10)
lm.beta(mod11)
sum$Fuel<-as.numeric(sum$Fuel)
sum$CO2<-as.numeric(sum$CO2)
sum$PM25<-as.numeric(sum$PM25)
sum$PM10<-as.numeric(sum$PM10)
sum$GDP<-as.numeric(sum$GDP)

library(class)
knn(sum,sum,cl,k=1000,)


normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
sum<-sum[!is.na(sum$Fuel),]
sum<-sum[!is.na(sum$GDP),]
sum<-sum[!is.na(sum$CO2),]
sum<-sum[!is.na(sum$PM25),]
sum<-sum[!is.na(sum$PM10),]
sum[,8]<- NULL
sumn<- as.data.frame(lapply(sum[,4:8], normalize))
sum1<- sum[,1]
write.csv(sum, "C:/Users/15221/Desktop/sum.csv")
