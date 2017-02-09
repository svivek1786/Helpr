#scoring.R

#' @get /scoring


getGeoCode <- function(cus_id)
{
  library(DBI)
  library(devtools)
  library(RMySQL)
  
mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.123.49')

query1 <- sprintf("select c.phone_number,c.first_name,t.customer_id,count(t.task_id) AS Task_Rescheduled,SUM(t.final_amount) as Amount_spend from helpr.task t
INNER JOIN task_reschedule_history AS trh ON trh.task_id=t.task_id
                  INNER JOIN task_history AS th ON (th.task_id = t.task_id AND th.task_status = 'RESCHEDULE' AND th.delete_flag = 1 )
                  INNER JOIN customer c on c.customer_id=t.customer_id
                  GROUP BY c.phone_number; ")

query3 <- sprintf("select c.phone_number,count(t.task_id) AS cancelled_count from customer c
inner join task t on c.customer_id=t.customer_id 
                  where t.status='cancelled' GROUP BY c.phone_number;")


query4 <- sprintf("select c.phone_number,count(t.task_id) AS completed_count from customer c
inner join task t on c.customer_id=t.customer_id 
                  where t.status='completed'GROUP BY c.phone_number; ")


data1 = dbGetQuery(mydb, query1)
data3 = dbGetQuery(mydb, query3)
data4 = dbGetQuery(mydb, query4)
b=merge(data3,data4,by="phone_number",all=TRUE)
m=merge(b,data1,by="phone_number",all=TRUE)
m[is.na(m)]<-0
m$customer_id.y<-NULL
m$Amount_spend.y<-NULL
m$Task_Reschedule.y<-NULL
m$first_name.y<-NULL
dbDisconnect(mydb)



m$customer_score<-ifelse(m$cancelled_count <= '1' & m$completed_count >= '15'& m$Amount_spend>='5000',5,
                         ifelse( m$cancelled_count <= '1' & m$completed_count >= '2'& m$Amount_spend>='5000',5,
                         ifelse(m$cancelled_count=='0' & m$completed_count =='3'& m$Amount_spend>='500',3,
                         ifelse(m$cancelled_count=='1' & m$completed_count =='1'& m$Amount_spend=='0',1,
                         ifelse(m$cancelled_count>='0' & m$completed_count >='5'& m$Amount_spend>='1000',4,
                                ifelse(m$cancelled_count=='1' & m$completed_count =='2'& m$Amount_spend>='500' ,3,      
                                
                                
                                ifelse(m$cancelled_count=='1' & m$completed_count =='1',3,
                                       ifelse(m$cancelled_count=='1' & m$completed_count =='0',1,
                                       ifelse(m$cancelled_count=='1' & m$completed_count =='2'& m$Amount_spend <= '500' ,3,
                        ifelse(m$cancelled_count=='0' & m$completed_count =='1'& m$Amount_spend <= '500',2,  
                               ifelse(m$cancelled_count=='0' & m$completed_count =='1'& m$Amount_spend <= '1000',3, 
                        
                      ifelse(m$cancelled_count >= '2' & m$completed_count >='3' & m$Amount_spend >= '1000',2,
                      ifelse(m$cancelled_count >= '9' & m$completed_count >= '3',3,
                      ifelse(m$cancelled_count >= '10' & m$completed_count >= '1' ,2,
                             ifelse(m$cancelled_count >= '20' & m$completed_count >= '5' ,2,
                             ifelse(m$cancelled_count<='2' & m$completed_count <='2'& m$Amount_spend <= '500',2 ,
                                    ifelse(m$cancelled_count>='20' & m$completed_count <='5'& m$Amount_spend <= '300',3 ,
                                    ifelse(m$cancelled_count >= '15' & m$completed_count >= '0',1,2))))))))))))))))))

#dbWriteTable(mydb, name='helpr.cust_scoring', value=m)
#dbSendQuery(mydb, 'drop table if exists cust_scoring')

cus_id<-m$customer_score[m$customer_id==cus_id]
return(cus_id)
}

