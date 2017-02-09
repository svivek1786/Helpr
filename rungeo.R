library(plumber)
r<-plumb("geo.R")
r$run(port=8000)