library(shiny)
#runApp("~/Smith/Junior Year/Data Science/shiny")
#http://www.r-bloggers.com/more-powerful-iconv-in-r/
  # Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  require(maps)
  require(googleVis)
  #http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
  clean.text = function(x)
  {
    # tolower
    x = iconv(x, 'UTF-8', 'latin1') #ASCII
    #x = tolower(x)
    # remove rt
   # x = gsub("rt", "", x)
    # remove at
   # x = gsub("@\\w+", "", x)
    # remove punctuation
   # x = gsub("[[:punct:]]", "", x)
    # remove numbers
    # x = gsub("[[:digit:]]", "", x)
    # remove links http
   # x = gsub("http\\w+", "", x)
    # remove tabs
   # x = gsub("[ |\t]{2,}", "", x)
    # remove blank spaces at the beginning
   # x = gsub("^ ", "", x)
    # remove blank spaces at the end
   # x = gsub(" $", "", x)
    return(x)
  }
  #load data read csv
  #setwd("~/Desktop/SHINY")
  #http://stackoverflow.com/questions/8434019/column-names-have-periods-inserted-where-there-should-be-spaces
  #summary<-read.csv("summary combined FINAL.csv",check.names=FALSE)
   summary<-read.csv("http://www.dropbox.com/s/c2311bb5nx18ypr/summary%20combined%20FINAL.csv",check.names=FALSE)

 # summaryLocation<-read.csv("for map combined FINAL.csv",check.names=FALSE)
  summaryLocation<-read.csv("http://www.dropbox.com/s/ub4hmbcac9tesbh/for%20map%20combined%20FINAL.csv",check.names=FALSE)
  #z<-apply(summary[,1:34],1,which.max) 
  #http://r.789695.n4.nabble.com/finding-max-value-in-a-row-and-reporting-colum-name-td2309358.html
  #summary[,35]=names(summary)[z] 
  #full<-read.csv("full tweets combined FINAL.csv",check.names=FALSE)
   full<-read.csv("http://www.dropbox.com/s/5ycwa4jksrowsol/full%20tweets%20combined%20FINAL.csv",check.names=FALSE)

full$text<-clean.text(full$text)
  
 
  
  
  
  
  
  
  
  
  
  
  datasetInput <- reactive({
    #switch(
    input$searchTerm #,
           #"leaves" = "leaves"
           #"hot chocolate"="hot chocolate"
          # "cold"="cold"
           #"chilly"="chilly"
           #"Snuggie/snuggie"=c("snuggie", "Snuggie")
           #"wind(y)" = c("wind", "windy")
           #"rain/rainy day" = c("rainy day","rain")
           #"scarf/scarves" = c("scarves","scarf")
           #"earmuffs" = "earmuffs"
           #"pumpkin" = "pumpkin"
           #"sweater (weather)" = c("sweater","sweater weather")
           #"Uggs/uggs" = c("Uggs","uggs")
           #"snow" = "snow"
           #"snowman" = "snowman"
           #"snowflake" = "snowflake"
           #"autumn" = "autumn"
           #"winter" = "winter"
           #"gloves" = "gloves"
           #"storm" = "storm"
           #"warm (weather)" = c("warm weather","warm")
           #"sunny" = "sunny"
           #"cozy" = "cozy"
           #"electric blanket" = "electric blanket"
           #"Thanksgiving"= "Thanksgiving"
           #"holiday" = "holiday"
           #"icy" = "icy"
           #"blizzard" = "blizzard")
    # "term"="term"
   # )

  })
  
  locationInput<- reactive({
   # switch(
      input$city
      #) #,
          # "New York City" = "New York City"
          # "Los Angeles"="Los Angeles"
          # "Chicago"="Chicago"
          # "New Orleans"="New Orleans"
          # "Seattle"="Seattle"
          # "Minneapolis" = "Minneapolis"
          # "Denver" = "Denver"
          # "San Antonio" = "San Antonio"
          # "Atlanta"="Atlanta"
    # "term"="term"
  })
  
  
  output$freqPlot <- renderPlot({
    #term<-subset(summary,)
    term<-summary[,c(paste(datasetInput()), "day")]
    plot(term[,1]~term[,2],xlab="Time (in days)",ylab="Frequency",type="b",main="November 1st to December 1st")
    
  })

  
  output$freqPlotTemp <- renderPlot({
    term<-summaryLocation[,c(paste(datasetInput()), "day","avgTemp","location")]
    term<-subset(term, location==paste(locationInput()))
    #http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
    par(mar=c(5,4,4,5)+.1)
    plot(term[,1]~term[,2],type="l",col="red",xlim=c(0,30),ylim=c(0,10),xlab="days",ylab="frequency")
    par(new=TRUE)
    plot(term[,2],term[,3],type="l",col="blue",xaxt="n",yaxt="n",xlim=c(0,30),ylim=c(0,80),xlab="",ylab="")
    axis(4)
    mtext("temperature in degrees F",side=4,line=3)
    legend("topleft",col=c("red","blue"),lty=1,legend=c("frequency                    ","temperature"))
   # print(term)
   # term<-subset(summary, location==paste(locationInput()))
   # term<-term[,c(paste(datasetInput()), "day","avgTemp")]
   # term<-subset(summary, location=="Chicago")
    #term<-term[,c("day", "avgTemp",paste(datasetInput()))]
    #term<-summary[,c(locationInput(), "day","avgTemp")]
    #plot(term[,1]~term[,2],type="l",xlim=c(0,30),ylim=c(0,80),col="red",main="red-frequency    blue-temperature")
    #lines(term[,2],term[,3],type="l",col="blue")
  })


  myDay <- reactive({
    as.integer(input$day)
  })
  

  
  output$gvis <- renderGvis({
    full$day=as.integer(full$day)
   myData <- subset(full, 
                 (day > (myDay()-1)) & (day < (myDay()+1)))
   
    #print(head(myData))
    myData$loc=paste(myData$lat, myData$lon, sep=":")
    
    #myData$text=as.character(myData$text)
  
    


    #GRandom<-
      gvisGeoChart(myData,locationvar="loc", hovervar= 'text' ,# paste(names(myData)[2]), #"text",
                   options=list(region='US', displayMode='Markers',  resolution='provinces',
                              width=800, height=500, 
                              colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                              )
                              
                 )     

  })
  

  
  
  
  
  

  #map('state')
  #long<-c(-74,-118.25,-122.33,-87.63,-98.5,-84.39,-104.98,-90.05,-93.27)
  #lat<-c(40,34.05,47.61,41.88,29.42,33.76,39.74,29.97,44.98)
  #longT<-c()
  #latT<-c()
  #longW<-c()
  #latW<-c()
  #points(long, lat, pch=21, col="black")
  #points(within0$longitude, within0$latitude, pch=22, col="blue")
  # mostFreq<-findMostFreq() or precompute this and have a vector for each location over time
  
  #text()
  #text()
  
  output$mapMotion <- renderPlot({
    map('state')
    #-74, 40
   long<-c(-74,-118.25,-122.33,-87.63,-98.5,-84.39,-104.98,-90.05,-93.27)
    lat<-c(41,34.05,47.61,41.88,29.42,33.76,39.74,29.97,44.98)
    points(long, lat, pch=21, col="black")
#summaryLocation$day=integer(summaryLocation$day)
  # test<-subset(summaryLocation, day==1)
#byDay<-subset(summary,day=2)
    #byDay<-subset(summary,day==myDay())
    #byDay<-subset(summary,day==myDay()) #this isn't working/myDay?
    byDay <- subset(summaryLocation,  (summaryLocation$day > (myDay()-1)) & (summaryLocation$day < (myDay()+1)))
    #text(-75,27,labels=paste(byDay$date[as.integer(myDay())]))
   # text(-75,27,labels="11/1/13")
    #print(nrow(byDay))
    #assume ordered
#assume extra column with most frequent term as string
    for (i in 1:nrow(byDay)){
    text(byDay$lon[i]-.25,byDay$lat[i]+.75,labels=paste(byDay[i,40])) #"maxFreq"
    text(byDay$lon[i]+2,byDay$lat[i]-.75,labels=paste(round(byDay[i,"avgTemp"],digits=2), "degrees F",sep=" "))
    text(-75,27,labels=paste(byDay$date[i]))
    #pull lat lon from data not like this, not flexible enough?
    }
    
    #for (i in 1:nrow(test)){
    #  text(test$lon[i]-.25,test$lat[i]+.75,labels="1") #"maxFreq"
     # text(test$lon[i]+2,test$lat[i]-.75,labels=paste(round(test[i,"avgTemp"],digits=2), "degrees F",sep=" "))
      #pull lat lon from data not like this, not flexible enough?
    #}
  })


})
  

  
  
#})

#require(maps)
#map('state')
#long<-c(-74,-118.25,-122.33,-87.63,-98.5,-84.39,-104.98,-90.05,-93.27)
#lat<-c(40,34.05,47.61,41.88,29.42,33.76,39.74,29.97,44.98)
#longT<-c()
#latT<-c()
#longW<-c()
#latW<-c()
#points(long, lat, pch=21, col="black")
#points(within0$longitude, within0$latitude, pch=22, col="blue")
# mostFreq<-findMostFreq() or precompute this and have a vector for each location over time

#text()
#text()
#need a bar that has day or something indicating time


#frequency over time for each search term precomputed + seperated by location
#weather over time for each location, precomputed (avg?)
#then easy to pick what to plot

#location and search terms correspond to a number (so do data frames)
#then pull up the dataset that corresponds to that number
#different number of characters in name?
