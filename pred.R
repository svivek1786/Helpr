#pred.R

#' @get /pred

prediction <- function(budget)
{
library(RMySQL)
mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.125.0')

query1<-sprintf("select * from marketing_sem_revenue_prediction")
ask = dbGetQuery(mydb,query1)

query2<-sprintf("select * from marketing_sem_prediction")
ask1 = dbGetQuery(mydb,query2)

dbDisconnect(mydb)

names(ask)[5]<-paste("Clicks")
names(ask)[6]<-paste("Cost")
names(ask)[7]<-paste("SEM_Revenue")
names(ask)[8]<-paste("Overall_Revenue")
names(ask)[4]<-paste("Impressions")

names(ask1)[2]<-paste("Impressions")
names(ask1)[3]<-paste("Clicks")
names(ask1)[4]<-paste("Cost")
names(ask1)[5]<-paste("SEM_Revenue")
names(ask1)[6]<-paste("Overall_Revenue")

# ask<-read.csv("C:\\Users\\vicky_000\\Desktop\\data2.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
#  ask1<-read.csv("C:\\Users\\vicky_000\\Desktop\\Book3.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)




lm.fit2<-lm(formula=Clicks~Cost,data=ask1)
input<-data.frame(Cost=as.numeric(budget))
Clicks1<-predict(lm.fit2,input,type="response")



lm.fit<-lm(formula=SEM_Revenue~Cost+Clicks,data=ask)
input<-data.frame(Cost=as.numeric(budget),Clicks=as.numeric(Clicks1))

SEM_revenue1<-predict(lm.fit,input,type="response")


lm.fit1<-lm(formula=Overall_Revenue~category_id+Cost,data=ask1)
input<-data.frame(category_id=0,Cost=as.numeric(budget))

Overall_Revenue<-predict(lm.fit1,input,type="response")

lm.fit5<-lm(formula=Impressions~Cost+category_id,data=ask)
input<-data.frame(Cost=input$Cost,category_id=as.numeric(budget))

Impressions<-predict(lm.fit5,input,type="response")

revenue = cbind(input$Cost,SEM_revenue1,Overall_Revenue,Clicks1,Impressions)
revenue=round(revenue,2)

revenue<-data.frame(revenue)
names(revenue)[1]<-paste("Budget")
names(revenue)[2]<-paste("SEM_Revenue") 
names(revenue)[3]<-paste("Overall_Revenue")
names(revenue)[4]<-paste("Clicks")
names(revenue)[5]<-paste("Impressions")
revenue
return(revenue)
}

