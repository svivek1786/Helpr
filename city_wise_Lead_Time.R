#Appliances/Banglore
ask<-read.csv("C:\\Users\\vicky_000\\Desktop\\Lead_Time_Analysis.csv - Lead_Time(1).csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
ask<-subset(ask,ask$category_id == 5)
ask<-subset(ask,ask$city_id == 2)
ask$created_date=as.Date(ask$created_date, format='%d/%m/%Y')
ask$task_from_date_time=as.Date(ask$task_from_date_time, format='%d/%m/%Y')

weekFrom<- weekdays(as.Date(ask$created_date,'%d/%m/%Y'))
weekTo<- weekdays(as.Date(ask$task_from_date_time,'%d/%m/%Y'))
ask2<-table(weekFrom,weekTo,ask$category_id)

ask2<-data.frame(ask2)
ask2$weekFrom <- factor(ask2$weekFrom, levels = c( "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ask2$weekTo<-substr(ask2$weekTo, 1, 2)
ask2$weekTo <- factor(ask2$weekTo, levels = c( "Su","Mo", "Tu", "We", "Th", "Fr", "Sa"))

ggplot(data=ask2, aes(x=ask2$weekFrom, y=ask2$Freq, fill=ask2$weekTo)) +geom_bar(stat="identity",position=position_dodge(0.75),colour="black")+geom_text(aes(label=ask2$weekTo), vjust=1, color="black",position = position_dodge(0.75), size=1.75)+scale_fill_manual(values=c('#ba778e','#ba778e','#ba778e','#ba778e','#ba778e','#ba778e','#ba778e'))+xlab("Booking Day") +ylab("Frequency")+ggtitle(label="Appliances Banglore")+labs(fill="Task Day")

#Plumbing/Banglore
ask<-read.csv("C:\\Users\\vicky_000\\Desktop\\Lead_Time_Analysis.csv - Lead_Time(1).csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
ask<-subset(ask,ask$category_id == 4)
ask<-subset(ask,ask$city_id == 2)
ask$created_date=as.Date(ask$created_date, format='%d/%m/%Y')
ask$task_from_date_time=as.Date(ask$task_from_date_time, format='%d/%m/%Y')

weekFrom<- weekdays(as.Date(ask$created_date,'%d/%m/%Y'))
weekTo<- weekdays(as.Date(ask$task_from_date_time,'%d/%m/%Y'))
ask2<-table(weekFrom,weekTo,ask$category_id)

ask2<-data.frame(ask2)
ask2$weekFrom <- factor(ask2$weekFrom, levels = c( "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ask2$weekTo<-substr(ask2$weekTo, 1, 2)
ask2$weekTo <- factor(ask2$weekTo, levels = c( "Su","Mo", "Tu", "We", "Th", "Fr", "Sa"))

ggplot(data=ask2, aes(x=ask2$weekFrom, y=ask2$Freq, fill=ask2$weekTo)) +geom_bar(stat="identity",position=position_dodge(0.75),colour="black")+geom_text(aes(label=ask2$weekTo), vjust=1, color="black",position = position_dodge(0.75), size=1.75)+scale_fill_manual(values=c('#00c6ff','#00c6ff','#00c6ff','#00c6ff','#00c6ff','#00c6ff','#00c6ff'))+xlab("Booking Day") +ylab("Frequency")+ggtitle(label="Plumbing Banglore")+labs(fill="Task Day")

#Electrical/Banglore
ask<-read.csv("C:\\Users\\vicky_000\\Desktop\\Lead_Time_Analysis.csv - Lead_Time(1).csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
ask<-subset(ask,ask$category_id == 3)
ask<-subset(ask,ask$city_id == 2)
ask$created_date=as.Date(ask$created_date, format='%d/%m/%Y')
ask$task_from_date_time=as.Date(ask$task_from_date_time, format='%d/%m/%Y')

weekFrom<- weekdays(as.Date(ask$created_date,'%d/%m/%Y'))
weekTo<- weekdays(as.Date(ask$task_from_date_time,'%d/%m/%Y'))
ask2<-table(weekFrom,weekTo,ask$category_id)

ask2<-data.frame(ask2)
ask2$weekFrom <- factor(ask2$weekFrom, levels = c( "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ask2$weekTo<-substr(ask2$weekTo, 1, 2)
ask2$weekTo <- factor(ask2$weekTo, levels = c( "Su","Mo", "Tu", "We", "Th", "Fr", "Sa"))

ggplot(data=ask2, aes(x=ask2$weekFrom, y=ask2$Freq, fill=ask2$weekTo)) +geom_bar(stat="identity",position=position_dodge(0.75),colour="black")+geom_text(aes(label=ask2$weekTo), vjust=1, color="black",position = position_dodge(0.75), size=1.75)+scale_fill_manual(values=c('orange','orange','orange','orange','orange','orange','orange'))+xlab("Booking Day") +ylab("Frequency")+ggtitle(label="Electrical Banglore")+labs(fill="Task Day")

#Carpentry/Banglore
ask<-read.csv("C:\\Users\\vicky_000\\Desktop\\Book21.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
ask<-subset(ask,ask$category_id == 6)
ask<-subset(ask,ask$city_id == 2)
ask$created_date=as.Date(ask$created_date, format='%d/%m/%Y')

ask$task_date=as.Date(ask$task_from_date_time, format='%d/%m/%Y')

#ask$task_start_time=as.Date(ask$task_start_time, format='%d/%m/%Y')

weekFrom<- weekdays(as.Date(ask$created_date,'%d/%m/%Y'))
weekTo<- weekdays(as.Date(ask$task_from_date_time,'%d/%m/%Y'))
ask2<-table(weekFrom,weekTo,ask$category_id)

ask2<-data.frame(ask2)
ask2$weekFrom <- factor(ask2$weekFrom, levels = c( "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ask2$weekTo<-substr(ask2$weekTo, 1, 2)
ask2$weekTo <- factor(ask2$weekTo, levels = c( "Su","Mo", "Tu", "We", "Th", "Fr", "Sa"))

ggplot(data=ask2, aes(x=ask2$weekFrom, y=ask2$Freq, fill=ask2$weekTo)) +geom_bar(stat="identity",position=position_dodge(0.75),colour="black")+geom_text(aes(label=ask2$weekTo), vjust=1, color="black",position = position_dodge(0.75), size=1.75)+scale_fill_manual(values=c('#E69F00','#E69F00','#E69F00','#E69F00','#E69F00','#E69F00','#E69F00'))+xlab("Booking Day") +ylab("Frequency")+ggtitle(label="Carpentry Banglore")+labs(fill="Task Day")

#PC/Banglore
ask<-read.csv("C:\\Users\\vicky_000\\Desktop\\Lead_Time_Analysis.csv - Lead_Time(1).csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
ask<-subset(ask,ask$category_id == 2)
ask<-subset(ask,ask$city_id == 2)
ask$created_date=as.Date(ask$created_date, format='%d/%m/%Y')
ask$task_from_date_time=as.Date(ask$task_from_date_time, format='%d/%m/%Y')

weekFrom<- weekdays(as.Date(ask$created_date,'%d/%m/%Y'))
weekTo<- weekdays(as.Date(ask$task_from_date_time,'%d/%m/%Y'))
ask2<-table(weekFrom,weekTo,ask$category_id)

ask2<-data.frame(ask2)
ask2$weekFrom <- factor(ask2$weekFrom, levels = c( "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ask2$weekTo<-substr(ask2$weekTo, 1, 2)
ask2$weekTo <- factor(ask2$weekTo, levels = c( "Su","Mo", "Tu", "We", "Th", "Fr", "Sa"))

ggplot(data=ask2, aes(x=ask2$weekFrom, y=ask2$Freq, fill=ask2$weekTo)) +geom_bar(stat="identity",position=position_dodge(0.75),colour="black")+geom_text(aes(label=ask2$weekTo), vjust=1, color="black",position = position_dodge(0.75), size=1.75)+scale_fill_manual(values=c('#fd7168','#fd7168','#fd7168','#fd7168','#fd7168','#fd7168','#fd7168'))+xlab("Booking Day") +ylab("Frequency")+ggtitle(label="PC")+labs(fill="Task Day")


#cleaning/Banglore
ask<-read.csv("C:\\Users\\vicky_000\\Desktop\\Lead_Time_Analysis.csv - Lead_Time(1).csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
ask<-subset(ask,ask$category_id == 1)
ask<-subset(ask,ask$city_id == 2)
ask$created_date=as.Date(ask$created_date, format='%d/%m/%Y')
ask$task_from_date_time=as.Date(ask$task_from_date_time, format='%d/%m/%Y')

weekFrom<- weekdays(as.Date(ask$created_date,'%d/%m/%Y'))
weekTo<- weekdays(as.Date(ask$task_from_date_time,'%d/%m/%Y'))
ask2<-table(weekFrom,weekTo,ask$category_id)

ask2<-data.frame(ask2)
ask2$weekFrom <- factor(ask2$weekFrom, levels = c( "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ask2$weekTo<-substr(ask2$weekTo, 1, 2)
ask2$weekTo <- factor(ask2$weekTo, levels = c( "Su","Mo", "Tu", "We", "Th", "Fr", "Sa"))

ggplot(data=ask2, aes(x=ask2$weekFrom, y=ask2$Freq, fill=ask2$weekTo)) +geom_bar(stat="identity",position=position_dodge(0.75),colour="black")+geom_text(aes(label=ask2$weekTo), vjust=1, color="black",position = position_dodge(0.75), size=1.75)+scale_fill_manual(values=c('#24c75a','#24c75a','#24c75a','#24c75a','#24c75a','#24c75a','#24c75a'))+xlab("Booking Day") +ylab("Frequency")+ggtitle(label="Cleaning Banglore")+labs(fill="Task Day")
