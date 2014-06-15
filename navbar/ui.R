library(shiny)
library(sqldf)
library(ggplot2)
require(gridExtra)
library(ggmap)

#require(rCharts)
#options(RCHART_LIB = 'sankey')
#library(googleVis)

shinyUI(navbarPage("York Tech",
                   tabPanel("The Student Body",
                            sidebarPanel(
                              h6("Gender"),
                              checkboxInput("male", "Male", TRUE),
                              checkboxInput("female", "Female", TRUE),
                              h6("Race"),
                              radioButtons("raceFilter", "", #"Race Filter",
                                           list("AllRaces"="AllRaces",
                                                "American Indian"="AmericanIndian",
                                                "Asian"="Asian",
                                                "Black"="Black" ,   
                                                "Hispanic"= "Hispanic",
                                                "More than one"= "Morethanone",
                                                "Native Hawaiian"= "NativeHawaiian",         
                                                "Non-Resident Alien"= "Non-ResidentAlien",
                                                "Unknown"= "Unknown",
                                                "White"= "White"),
                                           selected="All Races")                                                          
                            ),
                              mainPanel(h3("Explore our student body"),
                                plotOutput("distPlot")
                              )
                            ),
                   tabPanel("Map",
                            sidebarPanel(
                              h6("The Top 10 Feeder Schools:"),
                              radioButtons("HSpick", "Select Your High School",
                                           list("All High Schools" = "AllHighSchools",
                                                "Out of State HS" = "OutofStateHS",
                                                "Rock Hill High School" = "RockHillHighSchool",
                                                "Northwestern High School" = "NorthwesternHighSchool" ,   
                                                "York Comprehensive High School" = "YorkComprehensiveHighSchool",
                                                "Clover High School" = "CloverHighSchool",
                                                "Fort Mill High School" = "FortMillHighSchool",         
                                                "Lancaster County School" = "LancasterCountySchool",
                                                "District Chester High School" = "DistrictChesterHighSchool",
                                                "South Pointe High School" = "SouthPointeHighSchool",       
                                                "Nation Ford High School" = "NationFordHighSchool"),
                                           selected="All High Schools"),
                              h6("Map Parameters"),
                              sliderInput("jitter", "Point Jitter:", 
                                          min = 0, max = 0.5, value = 0.05, step= 0.01)
                            ),
                            mainPanel(
                                h4("Follow in their footsteps. Apply to York Technical College today!"),
                                plotOutput("map")
                            )
                   ),
                   tabPanel("Explorer",
                            h4("Find people like you:"),
                            dataTableOutput("dfExplorer")
                            ),
                  tabPanel("Program by Gender",
                           plotOutput("genderProgram")
                  ),
                  tabPanel("Student Clusters",
                           sidebarPanel(
                           radioButtons("cluster", "Choose Cluster",
                                        list("1"="1",
                                             "2"="2",
                                             "3"="3",
                                             "4"="4" ,   
                                             "5"="5",
                                             "6"="6"),
                                        selected="1")
                           ),
                           mainPanel(
                             h5("It is not a homogenous set of students, but of distinct groups of people.")
                             ,plotOutput("clustPlot")
                           )
                  ),
                  tabPanel("Debug"
                           ,textOutput("debug")
                           ,tableOutput("dfprint")
                  )
))

