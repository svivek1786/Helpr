library(plumber)
r<-plumb("pred.R")
r$run(port=8003)