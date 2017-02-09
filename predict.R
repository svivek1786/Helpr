
## 75% of the sample size
smp_size <- floor(0.75 * nrow(job4))
#split test set 
train_ind <- sample(seq_len(nrow(job4)), size = smp_size)
train<-job4[train_ind,]
test<-job4[-train_ind,]
test$class<-NULL

job6 <- subset(job4,job4$total_price >= 10000)


output.fit<-ctree(as.factor(class)~address+item_count,data=train)
p<-predict(output.fit,test)


