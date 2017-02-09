#scoring.R

#' @get /scoring

getCustScoring <- function(cus_id)
{
  library(DBI)
  library(devtools)
  library(RMySQL)
  library(party)
  library(stringi)
  library(stringr)
  
 
  mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.125.0')
  
  
  querylp<-sprintf("select * from customer_scoring;")
 
  datacheck=dbGetQuery(mydb, querylp)
  dbDisconnect(mydb)
  
  
 for( i in length(datacheck$customer_id))
 {
   
   if(datacheck$customer_id[i]==cus_id)
   {
     
     
     mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.123.49')
     
     querycs<-sprintf("select * from customer_scoring where customer_id not in ('CUSTOMER');")
     querycs <- sub("CUSTOMER",as.numeric(cus_id),querycs);
     
     datacs=dbGetQuery(mydb, querycs)
     train<-datacs
     train[is.na(train)]<-0
     train$get<-2
     
     query5<-sprintf("select * from customer_scoring where customer_id='CUSTOMER';")
     query5 <- sub("CUSTOMER",as.numeric(cus_id),query5)
     
     
     test<-dbGetQuery(mydb, query5)
     test$get<-1
     data=rbind(test,train)
     test<-subset(data,data$get==1)
     output.fit<-ctree(as.factor(customer_score)~cancelled_count+completed_count+amount_spend_task+amount_spend_subscription,data=train)
     p<-predict(output.fit,test)
     test$customer_score<-p
     
     #dbWriteTable(mydb, value = test, name = "customer_scoring", overwrite=TRUE ) 
  
     queryd<-sprintf("DELETE FROM customer_scoring where customer_id = 'CUSTOMER'")
     queryd <- sub("CUSTOMER",as.numeric(cus_id),queryd);
     dbSendQuery(mydb,queryd)
     
     
     
     querys<- sprintf("INSERT IGNORE INTO customer_scoring (customer_id,cancelled_count,completed_count,amount_spend_subscription,amount_spend_task, customer_score ) VALUES ('CUSTOMER','CANCELLED','COMPLETED','AMOUNT_S','AMOUNT_T','SCORE');")
     querys <- sub("CUSTOMER",as.numeric(test$customer_id),querys);querys <- sub("CANCELLED",as.numeric(test$cancelled_count),querys);querys <- sub("COMPLETED",as.numeric(test$completed_count),querys);querys <- sub("AMOUNT_S",as.numeric(test$amount_spend_subscription),querys);querys <- sub("AMOUNT_T",as.numeric(test$amount_spend_task),querys);querys <- sub("SCORE",as.numeric(test$customer_score),querys);
     dbSendQuery(mydb,querys)
     
     
     #queryv<- sprintf("UPDATE customer_scoring set customer_score='SCORE' where customer_scoring_id= 'CUS_SC_ID' AND customer_id='CUSTOMER';")
     #queryv <- sub("CUS_SC_ID",as.numeric(test$customer_scoring_id),queryv);queryv <- sub("CUSTOMER",as.numeric(cus_id),queryv);queryv <- sub("SCORE",as.numeric(test$customer_score),queryv)
     #dbSendQuery(mydb,queryv)
     
     queryu<- sprintf("UPDATE customer_scoring set last_modified_date=now() where customer_id='CUSTOMER'")
     queryu <- sub("CUSTOMER",as.numeric(cus_id),queryu)
     dbSendQuery(mydb,queryu)
     dbDisconnect(mydb)
     cus_id<-test$customer_score[test$customer_id==cus_id] 
     
   }
  else if(datacheck$customer_id[i]!=cus_id)
  {
    
    mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.123.49')
    
    
    
    train<-datacheck
    train[is.na(train)]<-0
    train$customer_scoring_id<-NULL
    train$last_modified_date<-NULL
    train$created_date<-NULL
    train$get<-2
    
    
    query1 <- sprintf("select DISTINCT i.customer_id,SUM(i.gross_amount) as amount_spend_task,c.created_date AS created_date from invoice i INNER JOIN customer c on c.customer_id=i.customer_id INNER JOIN task t ON i.task_id=t.task_id WHERE t.STATUS= 'COMPLETED' AND c.customer_id = 'CUSTOMER' GROUP BY i.customer_id;")
    query1 <- sub("CUSTOMER",as.numeric(cus_id),query1)
    
    
    
    query2 <- sprintf(" select DISTINCT c.customer_id,SUM(su.gross_amount) as amount_spend_subscription from customer c
                      INNER JOIN subscription_user_plan su ON su.customer_id=c.customer_id
                      WHERE su.subscription_invoice_number IS NOT NULL AND c.customer_id = 'CUSTOMER' GROUP BY c.customer_id;
                      ")
    query2 <- sub("CUSTOMER",as.numeric(cus_id),query2)
    
    
    query3 <- sprintf("select c.customer_id,count(t.task_id) AS cancelled_count  from customer c inner join task t on c.customer_id=t.customer_id where t.status='cancelled' AND c.customer_id = 'CUSTOMER'  GROUP BY c.customer_id  ;")
    query3 <- sub("CUSTOMER",as.numeric(cus_id),query3)
    
    query4 <- sprintf("select c.customer_id,count(t.task_id) AS completed_count from customer c inner join task t on c.customer_id=t.customer_id where t.status='completed' AND c.customer_id = 'CUSTOMER' GROUP BY c.customer_id  ; ")
    query4 <- sub("CUSTOMER",as.numeric(cus_id),query4)
    
    data1 = dbGetQuery(mydb, query1)
    data2 = dbGetQuery(mydb, query2)
    data3 = dbGetQuery(mydb, query3)
    data4 = dbGetQuery(mydb, query4)
    
    b=merge(data3,data4,by="customer_id",all=TRUE)
    c=merge(b,data2,by="customer_id",all=TRUE)
    test=merge(c,data1,by="customer_id",all=TRUE)
    test[is.na(test)]<-0
    test$customer_score<-3
    test$get<-1
    test$created_date<-NULL
    train<-rbind(train,test)
    test<-subset(train,train$get==1)
   
    output.fit<-ctree(as.factor(customer_score)~cancelled_count+completed_count+amount_spend_task+amount_spend_subscription,data=train)
    p<-predict(output.fit,test)
    test$customer_score<-p
    
    
    queryd<-sprintf("DELETE FROM customer_scoring where customer_id = 'CUSTOMER'")
    queryd <- sub("CUSTOMER",as.numeric(cus_id),queryd);
    dbSendQuery(mydb,queryd)
    
    
    querys<- sprintf("INSERT IGNORE INTO customer_scoring (customer_id,cancelled_count,completed_count,amount_spend_subscription,amount_spend_task, customer_score ) VALUES ('CUSTOMER','CANCELLED','COMPLETED','AMOUNT_S','AMOUNT_T','SCORE');")
    querys <- sub("CUSTOMER",as.numeric(test$customer_id),querys);querys <- sub("CANCELLED",as.numeric(test$cancelled_count),querys);querys <- sub("COMPLETED",as.numeric(test$completed_count),querys);querys <- sub("AMOUNT_S",as.numeric(test$amount_spend_subscription),querys);querys <- sub("AMOUNT_T",as.numeric(test$amount_spend_task),querys);querys <- sub("SCORE",as.numeric(test$customer_score),querys);
    dbSendQuery(mydb,querys)
    
    queryu<- sprintf("UPDATE customer_scoring set last_modified_date=now() where customer_id='CUSTOMER'")
    queryu <- sub("CUSTOMER",as.numeric(cus_id),queryu)
    dbSendQuery(mydb,queryu)
    dbDisconnect(mydb)
    cus_id<-test$customer_score[test$customer_id==cus_id]
    
  }
   
   
   
 }
 
  
  
  
  return(cus_id)
  
 
  
  
}



