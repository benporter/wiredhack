# Preprocessing File
# Used for creating objects that are read into global.R, which makes the 
# objects available to ui.R and server.R

library(sqldf)
library(ggmap)

students <- read.csv("~/ShinyApps/navbar/WiredHack.csv")

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
load(file="/home/benporter/ShinyApps/navbar/latlotstudent.Rdata") 

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

rm(studentsPreClust,clusterNum,clusteringModel,
   central_location,feederSchools,top10feeders)

save.image(file="/home/benporter/ShinyApps/navbar/wiredhack.rdata")
