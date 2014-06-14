library(shiny)
#library(rCharts)
library(sqldf)
library(ggplot2)
#library(googleVis)
require(gridExtra)

#options(RCHART_LIB = 'sankey')

shinyUI(navbarPage("Ben Porter",
                   tabPanel("The Student Body",
                            sidebarPanel(
                              h6("Gender"),
                              checkboxInput("male", "Male", TRUE),
                              checkboxInput("female", "Female", TRUE),
                              h6("Race"),
                              radioButtons("raceFilter", "Race Filter",
                                           list("AllRaces","AllRaces",
                                                "American Indian","AmericanIndian",
                                                "Asian","Asian",
                                                "Black","Black" ,   
                                                "Hispanic", "Hispanic",
                                                "More than one", "Morethanone",
                                                "Native Hawaiian", "NativeHawaiian",         
                                                "Non-Resident Alien", "Non-ResidentAlien",
                                                "Unknown", "Unknown",
                                                "White", "White"),
                                           selected="All Races")
                              #checkboxInput("American Indian","AmericanIndian", TRUE),
                              #checkboxInput("Asian","Asian", TRUE),
                              #checkboxInput("Black","Black", TRUE),
                              #checkboxInput("Hispanic", "Hispanic",TRUE),
                              #checkboxInput("More than one", "Morethanone",TRUE),
                              #checkboxInput("Native Hawaiian", "NativeHawaiian",TRUE),
                              #checkboxInput("Non-Resident Alien", "Non-ResidentAlien",TRUE),
                              #checkboxInput("Unknown", "Unknown",TRUE),
                              #checkboxInput("White", "White", TRUE)                              
                            ),
                              mainPanel(
                                plotOutput("distPlot")
                                #,plotOutput("distPlot2")
                              )
                            ),
                   tabPanel("Map"
                            #,showOutput("map","leaflet"))
                            #,htmlOutput("map")
                   ),
                   tabPanel("High School",
                            sidebarPanel(
                              h6("random commentary"),
                              radioButtons("HSpick", "Select Your High School",
                                           list("Out of State HS" = "OutofStateHS",
                                                "Rock Hill High School" = "RockHillHighSchool",
                                                "Northwestern High School" = "NorthwesternHighSchool" ,   
                                                "York Comprehensive High School" = "YorkComprehensiveHighSchool",
                                                "Clover High School" = "CloverHighSchool",
                                                "Fort Mill High School" = "FortMillHighSchool",         
                                                "Lancaster County School" = "LancasterCountySchool",
                                                "District Chester High School" = "DistrictChesterHighSchool",
                                                "South Pointe High School" = "SouthPointeHighSchool",       
                                                "Nation Ford High School" = "NationFordHighSchool"),
                                           selected="Rock Hill High School")
                              )
                           #,showOutput("rChart3", "nvd3")
                           ),
                   tabPanel("Explorer",
                            dataTableOutput("dfExplorer")),
                   tabPanel("Debug",textOutput("debug"),
                                    tableOutput("dfprint")
                            ),
                   sidebarPanel(
                     sliderInput("obs", 
                                 "Number of observations:", 
                                 min = 1,
                                 max = 1000, 
                                 value = 500)                   
                     
                   )
))

