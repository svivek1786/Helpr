f<-readLines('C:\\Users\\vicky_000\\Desktop\\helprrr\\vivektestresult.txt')
v<-substring(f,80)
gsub("&"," ",v,fixed=TRUE)