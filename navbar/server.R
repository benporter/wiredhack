library(shiny)
library(sqldf)
library(ggplot2)
require(gridExtra)
library(ggmap)

#require(rCharts)
#library(googleVis)

#load preprocessed data from proprocessing.R



# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  df1 <- reactive({
    df <- students
    if (!input$male) {
      df <- subset(students,!(Gender=="Male"))
    }
    
    if (!input$female) {
      df <- subset(students,!(Gender=="Female"))
    }
    return(df)
  })
  
  clustFiltered <- reactive({
    df <- sqldf(paste("select * from studentsClust where clusterNum in ('",input$cluster,"')",sep=""))
    return(df)
  })
  
  output$clustPlot <- renderPlot({
    df <- clustFiltered()
    #g <- ggplot(df, aes(Gender),stat="bin") + geom_bar()
    #g <- g + ggtitle(paste("Gender"))
    #g <- g + xlab("") + ylab("Number of Students")
    
    g2 <- ggplot(df, aes(Gender),stat="bin") 
    g2 <- g2 + geom_bar(fill="#0066CC", colour="#0066CC")
    g2 <- g2 + ggtitle(paste("Gender"))
    g2 <- g2 + xlab("") + ylab("Number of Students")
    g2 <- g2 + theme(panel.background = element_rect(fill = "white"))
    
    print(g2)
  })
  
  
  df2 <- reactive({
    df <- df1()
    if(input$raceFilter=="AllRaces") {
      #do no subsetting
    } else {
      racePick <- subset(Racemap,shortname==input$raceFilter)
      df <- sqldf(paste("select * from df where RaceText in ('",racePick$longname,"')",sep=""))
    }    
    return(df)
  })
  
  thechoice <- reactive({
    thepick <- subset(HSmap,shortname==input$HSpick)
    return(thepick)
  })
  
  latlonStudentJitter <- reactive({
     jitter <- input$jitter # amount to shift
     df <- latlonStudent
     df$lon_num_rand <- df$lon_num + rnorm(n=nrow(latlonStudent),mean=0,sd=jitter)
     df$lat_num_rand <- df$lat_num + rnorm(n=nrow(latlonStudent),mean=0,sd=jitter)
    return(df) 
  })
  
  HSsubset <- reactive({
    df <- latlonStudentJitter()
     if(thechoice()$shortname == "AllHighSchools") { 
       return(df) 
       } else { 
         df <- sqldf(paste("select * from df where HighSchool in ('",thechoice()$longname,"')",sep=""))
       }
    return(df)
  })
  
  output$map <- renderPlot({
     map <- ggmap(base_map_10,extent = "device",legend = "none") + geom_point(aes(x = lon_num_rand, y = lat_num_rand), colour = "blue", 
                                                                          data = HSsubset(), alpha = .20,size=2,na.rm=TRUE) 
     map <- map + theme(legend.position="none")
     print(map)
  })
  
  output$distPlot <- renderPlot({
    df <- df2() 
    
    g1 <- ggplot(df, aes(RegistrationStatus),stat="bin") 
    g1 <- g1 + geom_bar(fill="#FFCC99", colour="#FFCC99")
    g1 <- g1 + ggtitle(paste("Registration Status"))
    g1 <- g1 + xlab("") + ylab("Number of Students")
    g1 <- g1 + coord_flip()
    g1 <- g1 + theme(panel.background = element_rect(fill = "white"))
        
    g2 <- ggplot(df, aes(EnrollmentStatus),stat="bin") 
    g2 <- g2 + geom_bar(fill="#0066CC", colour="#0066CC")
    g2 <- g2 + ggtitle(paste("Enrollment Status"))
    g2 <- g2 + xlab("") + ylab("Number of Students")
    g2 <- g2 + theme(panel.background = element_rect(fill = "white"))
    
    g3 <- ggplot(df, aes(x=Age))
    g3 <- g3 + geom_histogram(binwidth = 1,fill="#006666", colour="#006666")
    g3 <- g3 + ggtitle(paste("Age Histogram"))
    g3 <- g3 + theme(panel.background = element_rect(fill = "white"))
    
    dftop10Programs <- sqldf("select Program, count(*) as count
                         from df 
                         group by Program
                         order by count desc
                         limit 10")
    
    dfProgramFilter <- subset(df,Program %in% dftop10Programs$Program)
    dfProgramFilter <- within(dfProgramFilter, 
                              Program <- factor(Program, 
                                          levels=names(sort(table(Program), 
                                                            decreasing=FALSE))))
    g4 <- ggplot(dfProgramFilter, aes(x=Program))
    g4 <- g4 + geom_bar(fill="#FFCC99", colour="#FFCC99")
    g4 <- g4 + ggtitle(paste("Top Programs"))
    g4 <- g4 + xlab("") + ylab("Number of Students")
    g4 <- g4 + coord_flip()
    g4 <- g4 + theme(panel.background = element_rect(fill = "white"))
    
    # collapse charts into a 2 by 2 matrix
    print(grid.arrange(g1, g2, 
                       g3, g4, 
                       ncol=2))
  })
  
  output$genderProgram <- renderPlot({
    g5 <- ggplot(GPTP, aes(Program, PercentofTotal,fill=Gender)) + geom_bar(position="dodge")
    g5 <- g5 + coord_flip()
    g5 <- g5 + ggtitle(paste("Registration Status"))
    g5 <- g5 + xlab("") + ylab("Number of Students")
    g5 <- g5 + theme(panel.background = element_rect(fill = "white"))
    g5 <- g5 + scale_fill_manual(values=c("#FFCC99","#0066CC"))
    print(g5)
  })
  
  output$dfExplorer = renderDataTable({
    students
  })
  
  # Debugging Output:  variable
  output$debug <- renderText({
    #print(thechoice()$longname)
    print(paste("The cluster value is: ",input$cluster," with class ", class(input$cluster),sep=""))
  })
  
  # Debugging Output: data table
  output$dfprint <- renderTable({
    #df <- sqldf(paste("select * from top10feeders  where HighSchool in ('",thechoice()$longname,"')",sep=""))
    df <- clustFiltered()
    head(df)
  })
  

})
