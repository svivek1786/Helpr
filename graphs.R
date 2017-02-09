library
data1$created_date=as.Date(data1$created_date, format='%d/%m/%Y')

windows(width=7*1.5,height=12/2)
par(mar=c(4,4,2,5))
plot(data1$created_date,data1$Temp, ylim=c(1,37),type='l',col="blue", xlab="month", ylab=NA)

library(xts)
ts <- xts(data1$category_id, as.Date(data1$created_date, "%Y-%m-%d"))

# convert daily data
ts_m = apply.monthly(ts, FUN)
ts_y = apply.yearly(ts, FUN)
ts_q = apply.quarterly(ts, FUN)
for (i in 1:nrow(data1))
{
aggregate(cbind(data1$created_date[i]) ~ data1$category_id[i], 
         data = data1, 
         FUN = function(x){NROW(x)})
}
for (i in 1:nrow(b))
{
  plot(dta13$count[i],dta13$data1.category_id[i])
  
  aggregate(cbind(data1$category_id, data1$Temp)~data1$created_date, data=data1, sum, na.rm=TRUE)
}
  
}







plot(data_graph$created_date,data_graph$Temp,type='l',ylim=c(1,37),col="blue", xaxt='n',ann=FALSE,yaxt='n')
> par(new=TRUE)
> plot(graph2$`data_graph$created_date`,graph2$V2,xlab="month",ylab="service demand")
> lines(graph2$`data_graph$created_date`,graph2$V2,xlab="month",ylab="service demand")


ggplot(data = data_graph, aes(x=data_graph$created_date, y=data_graph$category_id)) + geom_line(aes(colour=variable))
ggsave('')
ggplot(data = data_graph, aes(x=data_graph$created_date, y=data_graph$category_name)) + geom_line(aes(colour=data_graph$category_name))



grpah3$created_date=as.Date(grpah3$created_date, format='%d/%m/%Y')
g3<-aggregate(cbind(grpah3$category_id,grpah3$Temp)~grpah3$created_date, data=graph1, sum, na.rm=TRUE)
plot(data_graph$created_date,data_graph$Temp,type='l',ylim=c(1,37),col="blue", xaxt='n',ann=FALSE,yaxt='n',main="category: Electrical")
par(new=TRUE)
plot(g3$`grpah3$created_date`,g3$V1,main="category=Electrical")
lines(g3$`grpah3$created_date`,g3$V1,col="red")

grpah17<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\category17.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
grpah17$created_date=as.Date(grpah17$created_date, format='%d/%m/%Y')
g17<-table(grpah17$created_date,grpah17$category_id)
g17<-data.frame(g17)
plot(data_graph$created_date,data_graph$Temp,type='l',ylim=c(1,37),col="blue", xaxt='n',ann=FALSE,yaxt='n',main="category: carpentry")
axis(4)

par(new=TRUE)
plot(g17$Var1,g17$Freq),main="category=Geyser",ylab="date",xlab="demand")
lines(g17$Var1,g17$Freq)






grpah17<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\category17.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
grpah17$created_date=as.Date(grpah17$created_date, format='%d/%m/%Y')
g17<-table(grpah17$created_date,grpah17$category_id)
g17<-data.frame(g17)
plot(data_graph$created_date,data_graph$Temp,type='l',ylim=c(1,37),col="blue", xaxt='n',ann=FALSE,yaxt='n',main="category: carpentry")
axis(4)
par(new=TRUE)
plot(g17$Var1,g17$Freq,main="category=Geyser",xlab="date",ylab="demand")
lines(g17$Var1,g17$Freq,col="green")









#trend graph conversion/leads

conv1<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\conversion_category_1.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
hr<-substr(conv1$Hour,1,2)
conv1<-table(conv1$category_id,hr)
convr1<-data.frame(conv1)
gh1<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\category2_hour.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
hr<-substr(gh1$Hour,1,2)
ghr1<-table(gh1$category_id,hr)
ghr1<-data.frame(ghr1)
ghcon<-rbind(ghr1,convr1)
ggplot(data=ghcon, aes(x=ghcon$hr, y=ghcon$Freq, group=ghcon$Var1, colour=ghcon$Var1,xlab="hour",ylab="demand"))+geom_line() + geom_point()
+xlab("Hour") +ylab("No of Leads/Conversion") 
+ggtitle("category:cleaning conversion/Leads trend")
+scale_colour_manual(labels = c("leads", "conversion"), values = c("darkblue", "red"))



gh1<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\category1_hr.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
hr<-substr(gh1$created_date, 12,13)
ghr1<-table(gh1$category_id,hr)
ghr1<-data.frame(ghr1)
plot(ghr1$hr,ghr1$Freq,main="category=pest control",xlab="hour",ylab="No of demand")
lines(ghr1$hr,ghr1$Freq)




gh1<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\category1_hour.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
hr<-substr(gh1$Hour, 12,13)
ghr1<-table(gh1$category_id,hr)
ghr1<-data.frame(ghr1)
plot(ghr1$hr,ghr1$Freq,xaxt="n",main="category=pest control",xlab="hour",ylab="No of demand")
axis(1, at=seq(10,200,by =10),las=2)
lines(ghr1$hr,ghr1$Freq)


conv2<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\conversion_category_2.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
hr<-substr(conv2$created_date, 12,13)
conv2<-table(conv2$category_id,hr)
convr2<-data.frame(conv2)
plot(convr2$hr,convr2$Freq,xaxt='n',ann=FALSE,yaxt='n')
lines(convr2$hr,convr2$Freq,col="green")
par(new=TRUE)
gh2<-read.csv("C:\\Users\\vicky_000\\Desktop\\Helpr Graph\\category2_hour.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
hr<-substr(gh2$Hour,1,2)
ghr2<-table(gh2$category_id,hr)
ghr2<-data.frame(ghr2)
plot(ghr2$hr,ghr2$Freq,xaxt="n",main="category=pest control",xlab="hour",ylab="No of demand")
axis(1, at=seq(10,200,by =10),las=2)
lines(ghr2$hr,ghr2$Freq)







        