#Dumping ground for partially working code

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
  #hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
  #n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
  df <-sqldf("select RegistrationStatus, Gender, count(*) as Freq
             from students
             group by RegistrationStatus, Gender
             order by RegistrationStatus, Gender")
  
  #n1 <- nPlot(Count ~ RaceText, Group = 'Gender',data = df,type = 'pieChart')
  #n1$chart(donut = TRUE)
  n1 <- nPlot(Freq ~ RegistrationStatus, Group = "Gender",data = df, type = "multiBarChart")
  n1$addParams(dom = 'rChart3')
  #n1$chart(color = c('blue','orange','red'))
  #n1$chart(stacked=TRUE)
  
  #n1$print()
  n1
})

output$map <- renderGvis({
  zipDF <- sqldf("select Zip, count(*) as Students 
                 from students
                 group by Zip")
    nrow(zipDF)
    
    zipDF <- subset(zipDF,Zip != c("","00000"))
    zipDF <- subset(zipDF,!is.na(Zip))
    zipDF <- subset(zipDF,Students>5)
    zipDF$ziplength <- nchar(zipDF$Zip)
    zipDF <- subset(zipDF,ziplength==5)
    
    gvisGeoChart(zipDF,"Zip",colorvar="Students"
                       ,options=list(region="US", displayMode="auto", #region="US-SC"
                        #magnifyingGlass.enable=TRUE,
                        #magnifyingGlass="{enable: true, zoomFactor: 20}",
                        #magnifyingGlass.zoomFactor=8,
                        resolution="provinces",#metros
                        #width=400, height=380,
                        colorAxis="{colors:['#FFFFFF', '#0000FF']}",
                        enableScrollWheel=TRUE))
    
    })
  