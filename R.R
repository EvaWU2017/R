rm(list=ls())

##aggregate by id
aggregate(. ~Output, data=X111, sum, na.rm=TRUE)

##wide table to long table
library(reshape2)
long <- melt(fuel, id.vars = c("X__1", "Country"))

# describe
library(Hmisc)

summary(x)
describe(x)
stargazer(YXd,type="text")

subset.before.A<-merge(subset.before,Sarria_distances_final, by=c("Shipping.Zip.Text"), all.x=TRUE)

##lm.beta(mod11)
library(QuantPsyc)

## remove delete
instamaki04<-instamaki03[-which(instamaki03$Product.Title=="Donation"),]

## select column
y <-subset(x, select = c("a","b","c"))
                         
##remove multiple columns
R[,c(1:4)]<- NULL

instamaki<-data.frame(instamaki_data)


## select date
x <- subset(x, x$Order.Date.< as.Date("2017-12-28") & x$Order.Date.>=as.Date("2017-12-21"))

## add column
x <- within(x,{Week<-"2017-12-21"})

## bind rows
x <- bind_rows(d1, d2, d3)

## concat columns
fuel$Key<-paste(fuel$Code,fuel$Year,sep='')

## read excel
Table <- read_excel("C:/Users/15221/Desktop/Final Project/2.xlsx")

## N.A. = 0
x$Time[is.na(x$Time)]<-"0" 

## as character
x$Week<-as.character(x$Week)
as.numeric

## subset those nor in a several values
subset<-subset1[!(subset1$Customer.Id %in% subset1.changeshops$Customer.Id),]

santcugat_shop1 <- subset(instamaki04, instamaki04$Order.Date.>= as.Date("2018-01-23"))
santcugat_shop2 <- subset(santcugat_shop1, santcugat_shop1$Shipping.Zip.Text%in%c("08172", "08173", "08174", "08193", "08195", "08197", "08290"))


## Update name
colnames(CARTA)[1] <- "SKU_new"

##Product.Title<-instamaki05[,1]
##unique(Product.Title)

## convert Order ID to string
#orderid<-instamaki[,3]
#typeof(orderid)
#orderid<-as.character(orderid)
#instamaki<-within(instamaki,{orderid<-orderid})

## orderdate to ordermonth
x<-within(x,{Order.Month.<-as.character(substr(Order.Date.,1,7))})

sort(instamaki05,instamaki05$Order.Date.,decreasing=TRUE)
index<-duplicated(instamaki05$Customer.Id)
instamaki06<-instamaki05[!index,]

stargazer (Satisfaction, type="text", align=TRUE)

## delete all NA rows
x[!is.na(x$Shipping.Zip.Text),]

## export
write.csv(im_d_a, "~/Dropbox/INSTAMAKI/DATA_o/im_clean_agg.csv")


concat:
instamaki $key<-paste(instamaki $a,instamaki $b)

stacked barplot
install.packages("ggplot2")
library(ggplot2)

Final<- with(Final, Final[order(Year, Sectors, F_C),])
Final$F_C <- factor(Final$F_C, levels = c("Fuel","CO2"))
Final$Sectors <- factor(Final$Sectors, levels = c("Aviation","Shipping","Rail","Road","Other"))
ggplot(data=Final, aes(x=F_C, y=Value, fill=Sectors)) + 
geom_bar(stat="identity") + 
facet_grid(~Year) + 
labs(title="ESCAP Transport Sector Fuel Consumption & CO2 Emission", x="", y="Gg for CO2 Emission; ktoe for Fuel Consumption", fill="Sub-sectors") + 
theme(plot.title = element_text(size=25, margin=margin(t=20, b=20)))

removecolumn
R_aviation[,c(1:6)]<- NULL
Sum columns, bind together
other_sum<-colSums (R_other,na.rm = TRUE)
sum<-rbind(aviation_sum,road_sum,rail_sum,navigation_sum,other_sum)


aggregate
R_road <- subset(R, R$IPCC_description=="Road transportation")
R_rail <- subset(R, R$IPCC_description=="Rail transportation")
sum<-merge(R_road,R_rail, by=c("Name"), all.x=TRUE)
sum$sum1970<- sum$`1970` + sum$`1970.x`+sum$`1970.y`

floating pie
## data input (number of reads mapped to each category) 
total=1 
ESCAP_Road=368.7534177/14322.1252
ESCAP_Transport=635.8349773/14322.1252
World_Transport=1605.731554/14322.1252
ESCAP_all=8912.1092025688/14322.1252


iniR=0.3

colors=list(NO='gray95',total='orange1') 

library('plotrix')
library('mapplots') 

pie(1, radius=iniR, init.angle=90, col=c('aliceblue'), border = NA, labels='') 

floating.pie(0,0,c(total-World_Transport, ESCAP_all),radius=4*iniR, startpos=pi/2, col=as.character(colors[c('NO','total')]), border=(colour = 'white')) 
floating.pie(0,0,c(total-World_Transport, World_Transport),radius=3*iniR, startpos=pi/2, col=as.character(colors[c('NO','total')]), border=(colour = 'white')) 
floating.pie(0,0,c(total-ESCAP_Transport, ESCAP_Transport),radius=2*iniR, startpos=pi/2, col=as.character(colors[c('NO','total')]),border=(colour = 'white'))  
floating.pie(0,0,c(total-ESCAP_Road, ESCAP_Road),radius=1*iniR, startpos=pi/2, col=as.character(colors[c('NO','total')]),border=(colour = 'white'))


整理数据
library(reshape2)
melted <- melt(test, "person")
melted$cat <- ''
melted[melted$variable == 'value1',]$cat <- "first"
melted[melted$variable != 'value1',]$cat <- "second"




library(chron)
mod1<-glm(repeatedcustomer ~ Distance + Switch + Time + Interaction + InteractionDistance + Index.RFD.BCN,x=TRUE,family=binomial(link="probit"), data=instamaki11)

library(lfe)
mod2<-felm(Profit ~ Distance + Switch + Time + Interaction + InteractionDistance + Income| Order.Month., data=instamaki13)

Let’s assume we have several groups of labeled samples. The items present in the groups are homogeneous in nature. Now, suppose we have an unlabeled example which needs to be classified into one of the several labeled groups. How do you do that? Unhesitatingly, using kNN Algorithm.
k nearest neighbors is a simple algorithm that stores all available cases and classifies new cases by a majority vote of its k neighbors. This algorithms segregates unlabeled data points into well defined groups.

Knn classification
##normalize
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
sum<-sum[!is.na(sum$CO2),]
sum<-sum[!is.na(sum$PM25),]
sum<-sum[!is.na(sum$PM10),]
sumn<- as.data.frame(lapply(sum[,2:5], normalize))
sum1<- sum[,1]
write.csv(sumn, "C:/Users/15221/Desktop/sumn.csv")
write.csv(sum1, "C:/Users/15221/Desktop/sum1.csv")

##creating training & testing data
set.seed(123)
#random selection of 70% data
seed<- sample(1:nrow(sumn),size=nrow(sumn)*0.7,replace = FALSE) 
sum.train<- sumn[seed,2:5]
sum.test<- sumn[-seed,2:5]
sum.train.label<- sumn[seed,1]
sum.test.label<- sumn[-seed,1, drop = TRUE]
sum.train <- as.data.frame(sum.train)
sum.test <- as.data.frame(sum.test)
sum.train.label <- as.data.frame(sum.train.label)
sum.test.label <- as.data.frame(sum.test.label)

#classification
library(class) 
knn.16<- knn(train=sum.train, test=sum.test, cl=sum.train.label[,1, drop = TRUE], k=16)
knn.10<- knn(train=sum.train[,2:3], test=sum.test[,2:3], cl=sum.train$Code, k=10)
knn.16
##Evaluate the model performance
ACC.16 <- 100 * sum(sum.test.label== knn.16)/NROW(sum.test.label)
ACC.10 <- 100 * sum(sum.test$Code== knn.10)/NROW(sum.test$Code)
ACC.9
ACC.10 
knn.16
knn.16df <- as.data.frame(knn.16)
knn.16df



K means
rm(list=ls())
sumnc <- kmeans(sum[, c(3,5)], 3, nstart = 20)
library(datasets)
sumnc
sumncc <- as.data.frame(sumnc$cluster)


sumncc
write.csv(sumncc, "C:/Users/15221/Desktop/3.csv")

library(ggplot2)
ggplot(sum, aes(Fuel, PM25),color=cluster) + geom_point()
typeof()
sum$cluster<-as.character(sum$cluster)

##随机扰动项的序列相关检验dwtest(res)、异方差性检验bptest(result2), 解释变量的多重共线性检验car包求的vif等； 
library(lmtest)
dwtest(mod1)
library(car)
bptest(mod6)
vif(mod6)
