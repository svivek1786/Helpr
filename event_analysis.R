dat1<-read.csv("D:\\Cust_behav_helpr\\Event1\\Events1.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
dat2<-read.csv("D:\\Cust_behav_helpr\\Event1\\Events2.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
b=merge(dat2,dat1,by=c("Device_id","Date"),all=TRUE)
b$Events.y<-NULL
g<-b[row.names(unique(b[,c("Device_id","Date","Page","Events.x")])),]
g<-b[row.names(unique(b[,c("Device_id","Page")])),]

write.csv(g,"D:\\Cust_behav_helpr\\Event1\\event_trend4.csv")
