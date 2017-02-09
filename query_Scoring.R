
mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.123.49')

  query1 <- sprintf("select DISTINCT i.customer_id,c.first_name,SUM(i.gross_amount) as Amount_spend,c.created_date AS created_date from invoice i 
INNER JOIN customer c on c.customer_id=i.customer_id 
INNER JOIN task t ON i.task_id=t.task_id
                    WHERE t.STATUS= 'COMPLETED' GROUP BY i.customer_id;
                    ")
  
  query2 <- sprintf(" select DISTINCT c.customer_id,SUM(su.gross_amount) as Amount_spend_subscription from customer c
                    INNER JOIN subscription_user_plan su ON su.customer_id=c.customer_id
                    WHERE su.subscription_invoice_number IS NOT NULL GROUP BY c.customer_id;
                    ")
  


query3 <- sprintf("select c.customer_id,count(t.task_id) AS cancelled_count  from customer c inner join task t on c.customer_id=t.customer_id where t.status='cancelled'  GROUP BY c.customer_id  ;")



query4 <- sprintf("select c.customer_id,count(t.task_id) AS completed_count from customer c inner join task t on c.customer_id=t.customer_id where t.status='completed' GROUP BY c.customer_id  ; ")


data1 = dbGetQuery(mydb, query1)
data2 = dbGetQuery(mydb, query2)
data3 = dbGetQuery(mydb, query3)
data4 = dbGetQuery(mydb, query4)

b=merge(data3,data4,by="customer_id",all=TRUE)
c=merge(b,data2,by="customer_id",all=TRUE)
m=merge(c,data1,by="customer_id",all=TRUE)
m[is.na(m)]<-0