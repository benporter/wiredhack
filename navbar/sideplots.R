HSProgram <-sqldf("select HighSchool, Program, count(*) as count
                       from students
                    group by HighSchool, Program")

#remove small values
HSProgram <- subset(HSProgram,count > 30)
colnames(HSProgram) <- c("source","target","value")
HSProgram$source <- as.character(HSProgram$source)
HSProgram$target <- as.character(HSProgram$target)

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

############### Gender ##########################

GProgram <-sqldf("select Gender, Program, count(*) as count
                  from students
                  group by Gender, Program")
#remove small values
GProgram <- subset(GProgram,count > 30)
colnames(GProgram) <- c("source","target","value")
GProgram$source <- as.character(GProgram$source)
GProgram$target <- as.character(GProgram$target)

sankeyPlot2 <- rCharts$new()
#sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey")
#sankeyPlot$setLib("/home/benporter/ShinyApps/sankey/libraries/widgets/d3_sankey/")
#sankeyPlot$setTemplate(afterScript = "<script></script>")
sankeyPlot2$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
sankeyPlot2$setTemplate(script = "/home/benporter/ShinyApps/sankey/layouts/chart.html")

#sankeyPlot$set(data=HSProgram(),nodeWidth = 20,
sankeyPlot2$set(data=GProgram,nodeWidth = 20,
               nodePadding = 5,
               layout = 32,
               width = 960,
               height = 500)
sankeyPlot2