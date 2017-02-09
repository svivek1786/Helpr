#scoring.R

#' @get /scoring


getCustScoring <- function(cus_id)
{
  library(DBI)
  library(devtools)
  library(RMySQL)
  library(plyr)
  library(stringi)
  library(stringr)
  

  
  mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.123.49')
  
  query1 <- sprintf("select c.customer_id,c.first_name,count(t.task_id) AS Task_Rescheduled,SUM(t.final_amount) as Amount_spend from helpr.task t INNER JOIN task_reschedule_history AS trh ON trh.task_id=t.task_id INNER JOIN task_history AS th ON (th.task_id = t.task_id AND th.task_status = 'RESCHEDULE' AND th.delete_flag = 1 ) INNER JOIN customer c on c.customer_id=t.customer_id where c.customer_id = 'CUSTOMER' GROUP BY c.customer_id; ")
  query1 <- sub("CUSTOMER",as.numeric(cus_id),query1)
  
  
  query3 <- sprintf("select c.customer_id,count(t.task_id) AS cancelled_count from customer c inner join task t on c.customer_id=t.customer_id where t.status='cancelled' AND c.customer_id = 'CUSTOMER' GROUP BY c.customer_id  ;")
  query3 <- sub("CUSTOMER",as.numeric(cus_id),query3)
  
  
  query4 <- sprintf("select c.customer_id,count(t.task_id) AS completed_count from customer c inner join task t on c.customer_id=t.customer_id where t.status='completed' AND c.customer_id = 'CUSTOMER'  GROUP BY c.customer_id  ; ")
  query4 <- sub("CUSTOMER",as.numeric(cus_id),query4)
  
  
  query5 <- sprintf("select c.customer_id,AVG(tr.rating) AS AVERAGE_RATING from task t inner join team_rating tr on t.customer_id=tr.customer_id inner join customer c on c.customer_id =t.customer_id where c.customer_id = 'CUSTOMER' GROUP BY c.customer_id  ;")
  query5 <- sub("CUSTOMER",as.numeric(cus_id),query5)
  
  query6 <- sprintf("select c.customer_id,AVG(pr.rating) AS AVERAGE_RATING from task t inner join provider_rating pr on t.customer_id=pr.customer_id  inner join customer c on c.customer_id =t.customer_id where c.customer_id = 'CUSTOMER' GROUP BY c.customer_id  ;")
  query6 <- sub("CUSTOMER",as.numeric(cus_id),query6)
  
  data1 = dbGetQuery(mydb, query1)
  data3 = dbGetQuery(mydb, query3)
  data4 = dbGetQuery(mydb, query4)
  data5 = dbGetQuery(mydb, query5)
  data6 = dbGetQuery(mydb, query6)
  b=merge(data3,data4,by="customer_id",all=TRUE)
  c=merge(b,data1,by="customer_id",all=TRUE)
  
  d=merge(c,data5,by="customer_id",all=TRUE)
  m=merge(d,data6,by="customer_id",all=TRUE)

  #m$AVERAGE_RATING=apply(m[8:9,1,mean])
  m[is.na(m)]<-0
  
  
  
  
  m$customer_score<-ifelse(m$cancelled_count <= '1' & m$completed_count >= '15'& m$Amount_spend>='5000',5,
                    ifelse( m$cancelled_count <= '1' & m$completed_count >= '2'& m$Amount_spend>='5000',5,
                    ifelse(m$cancelled_count=='0' & m$completed_count =='3'& m$Amount_spend>='500',3,
                    ifelse(m$cancelled_count=='1' & m$completed_count =='1'& m$Amount_spend=='0',1,
                    ifelse(m$cancelled_count>='0' & m$completed_count >='5'& m$Amount_spend>='1000',4,
                    ifelse(m$cancelled_count=='1' & m$completed_count =='2'& m$Amount_spend>='500',3,      
                                                               
                                                               
                    ifelse(m$cancelled_count=='1' & m$completed_count =='1',3,
                    ifelse(m$cancelled_count=='1' & m$completed_count =='0',1,
                    ifelse(m$cancelled_count=='1' & m$completed_count =='2'& m$Amount_spend <= '500' ,3,
                    ifelse(m$cancelled_count=='0' & m$completed_count =='1'& m$Amount_spend <= '500',2,  
                    ifelse(m$cancelled_count=='0' & m$completed_count =='1'& m$Amount_spend <= '1000',3, 
                                                                                                  
                    ifelse(m$cancelled_count >= '2' & m$completed_count >='3' & m$Amount_spend >= '1000',2,
                    ifelse(m$cancelled_count >= '9' & m$completed_count >= '3',3,
                    ifelse(m$cancelled_count >= '10' & m$completed_count >= '1' ,2,
                    ifelse(m$cancelled_count >= '20' & m$completed_count >= '5' ,2,
                   ifelse(m$cancelled_count >= '15' & m$completed_count >= '0',1,2))))))))))))))))
  m$customer_score<-as.numeric(m$customer_score)

 # dbdata$first_name.y<-NULL
 # dbdata$first_name.x<-NULL
 # dbdata$first_name<-NULL
  
  
  #dbWriteTable(mydb, name='helpr.cust_scoring', value=m)
  dbWriteTable(mydb, value = m, name = "cust_scoring", append = TRUE ) 
  dbDisconnect(mydb)
  #dbSendQuery(mydb, 'drop table if exists cust_scoring')
  
  cus_id<-m$customer_score[m$customer_id==cus_id]
  return(cus_id)
}

