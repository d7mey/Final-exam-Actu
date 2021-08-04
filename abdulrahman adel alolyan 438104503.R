#Abdulrahman adel alolayan
#438104503
#Q1
data<-read.csv("AutoClaimsTEST.csv",header = T,na.strings = c("X"))
View(data)
str(data)
complete_data<-data[complete.cases(data),] 
incdata<-data[!complete.cases(data),]
View(incdata)
cdata<-data[complete.cases(data),]

state15<-cdata[cdata$STATE =="STATE 15",]
median(state15$AGE)#=64
data[!complete.cases(data$AGE) & data$STATE=="STATE 15",4]=64
#to add the new filled data in to account :
cdata<-data[complete.cases(data),]

f<-cdata[cdata$GENDER=="F","GENDER"]
m<-cdata[cdata$GENDER=="M","GENDER"]
length(f)#2582
length(m)#4189
data[!complete.cases(data$GENDER),3]=c("M","M")


Paym<-ifelse(data$SEVERITY>500,data$SEVERITY-500,0)
data$Payment<-Paym

fp="0"
data$Final_Payment<-fp 

data[data$YNA=="1",8]<-data[data$YNA=="1" ,7]*(1-0.1)
data[data$YNA=="2",8]<-data[data$YNA=="2" ,7]*(1-0.2)
data[data$YNA=="3",8]<-data[data$YNA=="3" ,7]*(1-0.3)

#Q2----------------------------------------------------------------
library(ggplot2)

data2<-read.csv("DentalClaims.csv",header = T)
View(data2)
str(data2)
data2$Provider <- factor(data2$Provider)
data2$Claim <- factor(data2$Claim)

calim<-data2$Claim

ggplot(data2, aes(x=calim)) + geom_histogram()


ClaimsYES<-data2[data2$Claim=="1",]
age=ClaimsYES$Age
paid=ClaimsYES$Paid
prov=ClaimsYES$Provider

ggplot(ClaimsYES, aes(x=age, y=paid)) + geom_point()
ggplot(data=ClaimsYES, aes(x=prov , y=paid))+geom_boxplot()

#Q3----------
data(soa08Act)
library("lifecontingencies")
A45<-Axn(actuarialtable=soa08Act, x=45)
a45<-axn(actuarialtable=soa08Act,x=45)
N<-1000*A45+3*a45
D<-(1-0.1)*a45
G=N/D #19.17494

A46<-Axn(actuarialtable=soa08Act, x=46)
a46<-axn(actuarialtable=soa08Act,x=46)

P=(1000*A45)/(a45)

V=1000*A46-P*a46
