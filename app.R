shiny.maxRequestSize=30*1024^2

library(shiny)
library(RMySQL)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)
library(leaflet)
library(data.table)
ui<-fluidPage(
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    
    sidebarPanel("Helpr Reports",
                 
                 
                 
                 selectInput("Reports", 
                             label = "Reports",
                             choices = list('Maps-Task Completed')),
                 
                 
                 checkboxGroupInput("category", 
                                    label = "category",
                                    choices = list("cleaning"=1,"Pest-Control"=2,"Electrical"=3,"Plumbing"=4,"Appliances"=5,"Carpentry"=6)),
                 
                 selectInput("city", 
                             label = "city",
                             choices = list("Bengaluru","Chennai","Hyderabad","Pune","coimbatore")),
                 
                 dateRangeInput("daterange", "Date range:",
                                start  = "2016-08-01",
                                end    = "2016-08-31",
                                min    = "2016-01-01",
                                max    = "2019-05-21",
                                format = "yyyy-mm-dd",
                                separator = " - "),
                 actionButton("doQuery", label = "Generate Report"),
                 
                 tags$head(tags$style(type="text/css", "
                                      #loadmessage {
                                      position: fixed;
                                      top: 0px;
                                      left: 0px;
                                      width: 100%;
                                      padding: 5px 0px 5px 0px;
                                      text-align: center;
                                      font-weight: bold;
                                      font-size: 100%;
                                      color: #000000;
                                      
                                      z-index: 105;
                                      }
                                      ")),
                 
                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                  tags$div("Please Wait...",id="loadmessage"))
                 
                 
                 
                 ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Marker", verbatimTextOutput("markers"))
        
        
        
      )
    )
    
                 ))






shinyServer<-function(input,output){
  
  
  
  #task cancelled in map reports with date
  
  loadDataDate <- eventReactive(input$doQuery, {
    
    
    mydb <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='52.66.125.0')
    
    # query1 <- sprintf("select task_id,category_id,created_date from helpr.task where category_id ='%s'",input$category)
    
    
    
    #mydb1 <- dbConnect(MySQL(), user='helpr', password='Q#2dT9C&xapk', dbname='helpr', host='gethelpr.in')
    
    
    
    query2<- sprintf("SELECT count(t.task_id),t.zipcode,g.longitude,g.latitude FROM task t 
                     INNER JOIN city c ON c.city_id = t.city_id 
                     INNER JOIN zipcode_geocode g ON t.zipcode=g.zipcode
                     WHERE t.status= 'CANCELLED'AND  t.category_id in ( '%s','%s','%s','%s','%s','%s') AND t.task_date between 'DATE1' and 'DATE2' AND c.city_name= '%s' GROUP BY t.zipcode ORDER BY t.zipcode",input$category[1],input$category[2],input$category[3],input$category[4],input$category[5],input$category[6],input$city)
    query2 <- sub("DATE1",input$daterange[1],query2);query2 <- sub("DATE2",input$daterange[2],query2);
    
    
    
    
    data = dbGetQuery(mydb,query2)
    
    dbDisconnect(mydb)
    
    data$zipcode=as.character(data$zipcode)
    #a<-geocode(data$zipcode)
    #data$lon<-a$lon
    #data$lat<-a$lat
    setnames(data,"count(t.task_id)","count")
    data
    
  })
  
  
  
  output$map <- renderLeaflet({
    
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lng=loadDataDate()$longitude,lat=loadDataDate()$latitude,radius=loadDataDate()$count,clusterOptions = markerClusterOptions())})
  output$markers <- renderPrint({print(loadDataDate(),width = getOption("width"),color="red",size="2")})
  
  
  
  
  
  
}

shinyApp(ui=ui,server=shinyServer)