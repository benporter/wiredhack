library(shiny)
require(rCharts)
library(sqldf)
library(ggplot2)

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
  
  #folder.chosen <- reactive({
  #  if (input$inputmethod=="normal")
  #  { return(substr(input$dataset, 1, regexpr("\\/",input$dataset)[1])) }
  #  else { return(NULL) }
  #})
  
  thechoice <- reactive({
    thepick <- subset(HSmap,shortname==input$HSpick)
    return(thepick)
  })
  
  output$debug <- renderText({
    #print(thechoice()$longname)
    print(input$HSpick)
  })
  
  output$dfprint <- renderTable({
    df <- sqldf(paste("select * from top10feeders  where HighSchool in ('",thechoice()$longname,"')",sep=""))
    head(df)
  })
  
  
  
  

  HSProgram <- reactive({
    HSProgram <-sqldf("select HighSchool, Program, count(*) as count
                       from students
                      group by HighSchool, Program")
  
    #remove small values
    HSProgram <- subset(HSProgram,count > 30)
    colnames(HSProgram) <- c("source","target","value")
    HSProgram$source <- as.character(HSProgram$source)
    HSProgram$target <- as.character(HSProgram$target)
    return(HSProgram)
    
  })
  
  #output$sankeyHSProgram <-  renderChart({ 
  output$sankeyHSProgram <-  renderChart2({ 
    sankeyPlot <- rCharts$new()
    #sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey")
    #sankeyPlot$setLib("/home/benporter/ShinyApps/sankey/libraries/widgets/d3_sankey/")
    #sankeyPlot$setTemplate(afterScript = "<script></script>")
    sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
    sankeyPlot$setTemplate(script = "/home/benporter/ShinyApps/sankey/layouts/chart.html")
        
    sankeyPlot$set(data=HSProgram(),nodeWidth = 20,
    #sankeyPlot$set(data=HSProgram,nodeWidth = 20,
                 nodePadding = 5,
                 layout = 32,
                 width = 960,
                 height = 500)
    return(sankeyPlot)
  })
    
  output$distPlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    par(mfrow=c(1,2))
    hist(students$Age)
    #hist(students$Gender)
    #hist(students$RaceText)
    #boxplot(data=students,formula=Age ~ Gender)
    
  })
  
  output$rChart3 <- renderChart({
     hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
     n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
     n1$print()
  })
  
  
  output$dfExplorer = renderDataTable({
    students
  })
})
