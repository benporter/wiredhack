library(shiny)
library(rCharts)
library(sqldf)
library(ggplot2)

#options(RCHART_LIB = 'sankey')

shinyUI(navbarPage("Ben Porter",
                   tabPanel("The Student Body",
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
                            ),
                              mainPanel(
                                plotOutput("distPlot")
                              )
                            ),
                   tabPanel("High School"
                            #,showOutput("sankeyHSProgram","sankey")
                            #,showOutput("sankeyHSProgram","sankey")
                            ),
                            #showOutput("rChart3", "polycharts")),
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

