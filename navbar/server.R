library(shiny)
#require(rCharts)
library(sqldf)
library(ggplot2)
#library(googleVis)
require(gridExtra)
library(ggmap)

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

HSmap <- data.frame("longname" = c("All High Schools","Out of State HS","Rock Hill High School","Northwestern High School", 
                    "York Comprehensive High School","Clover High School","Fort Mill High School",       
                    "Lancaster County School","District Chester High School","South Pointe High School",     
                    "Nation Ford High School"),
                    "shortname" = c("AllHighSchools","OutofStateHS","RockHillHighSchool","NorthwesternHighSchool",
                                  "YorkComprehensiveHighSchool","CloverHighSchool",
                                  "FortMillHighSchool", "LancasterCountySchool",
                                  "DistrictChesterHighSchool","SouthPointeHighSchool","NationFordHighSchool")
                    )

Racemap <- data.frame("longname" = c( "All Races","American Indian","Asian","Black","Hispanic","More than one",
                                      "Native Hawaiian","Non-Resident Alien","Unknown","White"),
                      "shortname" =c("AllRaces","AmericanIndian","Asian","Black","Hispanic","Morethanone","NativeHawaiian",
                                     "Non-ResidentAlien","Unknown","White")
                      )

central_location <- "452 South Anderson Road, Rock Hill, South Carolina 29730"
base_map_10 <- get_map(central_location ,zoom = 10)
load(file="/home/benporter/ShinyApps/latlotstudent.Rdata") 

##########################
# Gender and Program Plot
##########################
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

###############
# Clustering 
###############

studentsPreClust <- students[,c("StudentNumber","Age","Gender")]
studentsPreClust$PercentFemale <- ifelse(studentsPreClust$Gender == "Female",1,0)
studentsPreClust$Gender <- NULL
studentsPreClust$AgeNorm <- 3 * (studentsPreClust$Age - min(studentsPreClust$Age)) / (max(studentsPreClust$Age))
studentsPreClust$Age <- NULL
rownames(studentsPreClust) <- studentsPreClust$StudentNumber
studentsPreClust$StudentNumber <- NULL
clusteringModel <- kmeans(x=studentsPreClust,centers=6)
clusterNum <- data.frame("clusterNum"=as.character(clusteringModel$cluster))
studentsClust <- cbind(students,clusterNum)


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
    df <- sqldf(paste("select * from studentsClust where clusterNum in ('",as.character(input$cluster),"')",sep=""))
    return(df)
  })
  
  output$clustPlot <- renderPlot({
    df <- clustFiltered()
    g <- ggplot(df, aes(Gender),stat="bin") + geom_bar()
    g <- g + ggtitle(paste("Gender"))
    p <- p + xlab("") + ylab("Number of Students")
    print(g)
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
     jitter <- input$jitter
     df <- latlonStudent
     #df$lon_num_rand <- df$lon_num + runif(n=nrow(latlonStudent),min=(-1*jitter),max=jitter)
     #df$lat_num_rand <- df$lat_num + runif(n=nrow(latlonStudent),min=(-1*jitter),max=jitter)
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
     #data = latlonStudent
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
    #g3 <- g3 + geom_bar(fill="#006633", colour="#006633")
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
    
    #Program
    
    #hist(students$Age)
    #hist(students$Gender)
    #hist(students$RaceText)
    #boxplot(data=students,formula=Age ~ Gender)
    
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
  
  output$distPlot2 <- renderPlot({
    df <- df1()
    g <- ggplot(df, aes(EnrollmentStatus),stat="bin") + geom_bar()
    g <- g + ggtitle(paste("Enrollment Status"))
    p <- p + xlab("") + ylab("Number of Students")
    print(g)
  })
    
  
  
  output$debug <- renderText({
    #print(thechoice()$longname)
    print(paste("The choice is: ",thechoice()$shortname,sep=""))
  })
  
  output$dfprint <- renderTable({
    #df <- sqldf(paste("select * from top10feeders  where HighSchool in ('",thechoice()$longname,"')",sep=""))
    df <- clustFiltered()
    head(df)
  })
  
  output$dfExplorer = renderDataTable({
    dfEx <- subset(students,select=!(colnames(students) %in% c("X","StudentNumber")))
  })
})
