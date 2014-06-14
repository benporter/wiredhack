library(shiny)
#require(rCharts)
library(sqldf)
library(ggplot2)
#library(googleVis)
require(gridExtra)

students <- read.csv("~/ShinyApps/WiredHack.csv")

feederSchools <- sqldf("select HighSchool,count(*) as studentcount
                       from students
                       group by HighSchool
                       order by studentcount desc
                       limit 10")

top10feeders <- sqldf("select s.*
      from feederSchools fs left join
      students s on 
      fs.HighSchool = s.HighSchool")

HSmap <- data.frame("longname" = c("Out of State HS","Rock Hill High School","Northwestern High School", 
                    "York Comprehensive High School","Clover High School","Fort Mill High School",       
                    "Lancaster County School","District Chester High School","South Pointe High School",     
                    "Nation Ford High School"),
                    "shortname" = c("OutofStateHS","RockHillHighSchool","NorthwesternHighSchool",
                                  "YorkComprehensiveHighSchool","CloverHighSchool",
                                  "FortMillHighSchool", "LancasterCountySchool",
                                  "DistrictChesterHighSchool","SouthPointeHighSchool","NationFordHighSchool")
                    )

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
  
  df2 <- reactive({
    df <- df1()
    if (!input$AmericanIndian) {df <- subset(students,!(Gender=="Male"))}
    if (!input$Asian) {df <- subset(students,!(Gender=="Male"))}
    if (!input$Black) {df <- subset(students,!(Gender=="Male"))}
    if (!input$Hispanic) {df <- subset(students,!(Gender=="Male"))}
    if (!input$Morethanone) {df <- subset(students,!(Gender=="Male"))}
    if (!input$NativeHawaiian) {df <- subset(students,!(Gender=="Male"))}
    if (!input$Non-ResidentAlien) {df <- subset(students,!(Gender=="Male"))}
    if (!input$Non-ResidentAlien) {df <- subset(students,!(Gender=="Male"))}
    if (!input$Non-ResidentAlien) {df <- subset(students,!(Gender=="Male"))}
    
    #checkboxInput("American Indian","AmericanIndian", TRUE),
    #checkboxInput("Asian","Asian", TRUE),
    #checkboxInput("Black","Black", TRUE),
    #checkboxInput("Hispanic", "Hispanic",TRUE),
    #checkboxInput("More than one", "Morethanone",TRUE),
    #checkboxInput("Native Hawaiian", "NativeHawaiian",TRUE),
    #checkboxInput("Non-Resident Alien", "Non-ResidentAlien",TRUE),
    #checkboxInput("Unknown", "Unknown",TRUE),
    #checkboxInput("White", "White", TRUE) 
    return(df)
  })
  
  thechoice <- reactive({
    thepick <- subset(HSmap,shortname==input$HSpick)
    return(thepick)
  })
  
  output$distPlot <- renderPlot({
    df <- df1()
    
    
    
    g1 <- ggplot(df, aes(RegistrationStatus),stat="bin") 
    g1 <- g1 + geom_bar(fill="#FFCC99", colour="gray")
    g1 <- g1 + ggtitle(paste("Registration Status"))
    g1 <- g1 + xlab("") + ylab("Number of Students")
    g1 <- g1 + coord_flip()
    
    
    g2 <- ggplot(df, aes(EnrollmentStatus),stat="bin") 
    g2 <- g2 + geom_bar(fill="#0066CC", colour="#0066CC")
    g2 <- g2 + ggtitle(paste("Enrollment Status"))
    g2 <- g2 + xlab("") + ylab("Number of Students")
    
    g3 <- ggplot(df, aes(x=Age))
    g3 <- g3 + geom_histogram(binwidth = 1)
    g3 <- g3 + geom_bar(fill="#006633", colour="gray")
    g3 <- g3 + ggtitle(paste("Age Histogram"))
    
    #Program
    
    #hist(students$Age)
    #hist(students$Gender)
    #hist(students$RaceText)
    #boxplot(data=students,formula=Age ~ Gender)
    
    print(grid.arrange(g1, g2, g3,ncol=2))
  })
  
  output$distPlot2 <- renderPlot({
    df <- df1()
    g <- ggplot(df, aes(EnrollmentStatus),stat="bin") + geom_bar()
    g <- g + ggtitle(paste("Enrollment Status"))
    p <- p + xlab("") + ylab("Number of Students")
    print(g)
  })
    
  
  
  output$debug <- renderText({
    #print(thechoice()$longname)
    print(input$HSpick)
  })
  
  output$dfprint <- renderTable({
    df <- sqldf(paste("select * from top10feeders  where HighSchool in ('",thechoice()$longname,"')",sep=""))
    head(df)
  })
  
  
  output$dfExplorer = renderDataTable({
    students
    dfEx <- subset(students,select=!(colnames(students) %in% c("X","StudentNumber")))
  })
})
