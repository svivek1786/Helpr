#geo.R

#' @get /geo

getGeoCode <- function(address)
{
  library("RJSONIO") #Load Library
  library("base")
  gcStr <- gsub(' ','%20',address) #Encode URL Parameters
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  lat <- data.json["results.geometry.location.lat"]
  lng <- data.json["results.geometry.location.lng"]
  gcodes <- c(lat,lng)
  gcodes<-as.numeric(gcodes)
  #names(gcodes) <- c("Latitude", "Longitude")
  
  
  earth.dist <- function (long1, lat1, long2, lat2)
  {
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
  }
  
  
  
  
  
  gdiff<-earth.dist(gcodes[1],gcodes[2],11.0618,76.9558)
  gdiff1<-earth.dist(gcodes[1],gcodes[2],12.9618,77.59468)
  gdiff2<-earth.dist(gcodes[1],gcodes[2],17.3850,78.48675)
  gdiff3<-earth.dist(gcodes[1],gcodes[2],13.0827,80.2707)
  gdiff4<-earth.dist(gcodes[1],gcodes[2],18.5204,73.8567)
  
 
  
  if(is.na(gdiff >=50 && gdiff1 >=50 && gdiff2 >=50 && gdiff3>=50 && gdiff4>=50))
  {
    g<-c("NULL","NULL")
    g<-as.character(g)
    sprintf(g)
  }else if(gdiff >=50 && gdiff1 >=50 && gdiff2 >=50 && gdiff3>=50 && gdiff4>=50)
  {
    g<-c("NULL","NULL")
    g<-as.character(g)
    sprintf(g)
    
  }else{
  
  #geocodes<-toJSON(as.list(head(gcodes)))
  return(gcodes)
  return (names(gcodes))
  }

}
