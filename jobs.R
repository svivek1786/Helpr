job<-read.csv("C:\\Users\\vicky_000\\Desktop\\job4.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
job$address_count<-nchar(job$address)
m<-difftime(job$task_from_date_time,job$created_date,units="hours")
td <- seconds_to_period(m)
tims<-substr(td,1,1)
job$tim_diff<-tims
jobs.features<-job
jobs.features$class<-NULL
jobs.features$task_from_date_time<-NULL
jobs.features$created_date<-NULL
jobs.features$task_id<-NULL
jobs.features$task_id<-NULL
jobs.features$created_date<-NULL
is.numeric(job.features)
results<-kmeans(jobs.features,2)

output.t<-ctree(as.factor(class)~address_count,data=job4)

