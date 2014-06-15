#Dumping ground for partially working code


GenderProgram <-sqldf("select Gender, Program, count(*) as count
                       from students
                       group by Gender, Program")
TopPrograms <- sqldf("select Program, 
                             count(*) as count
                      from students
                      group by Program
                      order by count desc
                      limit 20")
GPTP <- sqldf("select gp.Gender,gp.Program,gp.count as gpcount,
                      tp.count as tpcount
              from GenderProgram gp inner join
              TopPrograms tp on 
              gp.Program = tp.Program")

GPTP$PercentofTotal <- GPTP$gpcount / GPTP$tpcount


g5 <- ggplot(GPTP, aes(Program, PercentofTotal,fill=Gender)) + geom_bar(position="dodge")
g5 <- g5 + coord_flip()
g5 <- g5 + ggtitle(paste("Registration Status"))
g5 <- g5 + xlab("") + ylab("Number of Students")
g5 <- g5 + theme(panel.background = element_rect(fill = "white"))
g5

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
  
  #sankeyPlot$set(data=HSProgram(),nodeWidth = 20,
  sankeyPlot$set(data=HSProgram,nodeWidth = 20,
                 nodePadding = 5,
                 layout = 32,
                 width = 960,
                 height = 500)
  return(sankeyPlot)
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
  

output$probplot <- renderPlot({
  
  #mc.box.offset <- 4.9
  
  p <- ggplot(m.input.data(), aes(category, value, fill=variable),stat="indentity") + geom_bar() + scale_x_discrete(limits=c("Engine 7","Engine 11 via 36th","Engine 11 via 33rd"))
  p <- p + xlab("Engine and Route") + ylab("Travel Time, Minutes")
  
  p <- p + scale_fill_manual(values = c("MaxDelay" = "#FFA500","AvgDelay" = "#CD8500","GoogleMapTime" = "#00688B"),
                             name="",
                             breaks=c("MaxDelay", "AvgDelay", "GoogleMapTime"),
                             labels=c("Maximum Delay", "Average Delay " ,"Average Travel Time (Google Maps)"))
  #p <- p + opts(legend.position = "bottom",legend.direction = "vertical")
  p <- p + opts(legend.position = "right",legend.direction = "vertical")
  #p <- p + geom_text(aes(1, mc.box.offset, label="Meck County Avg"),colour='black',size=5)
  
  # add or remove the Meck County Avg Response time line
  if(input$charmeckavg==TRUE) {
    p <- p + geom_hline(aes(yintercept=4.566666667))
    p <- p + geom_text(aes(1, 4.9, label="Meck County Avg"),colour='gray40',size=4)
  }
  
  p <- p + ggtitle(paste("Response Times to Site\nTrain Length: ",graph.title(),sep=""))
  print(p)
  
})
