library(plumber)
r<-plumb("pred_cat.R")
r$run(port=8004)