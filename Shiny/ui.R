library(shinydashboard)
library(shiny)

dashboardPage( skin = "green",
  
  dashboardHeader(title = "Video Game Sales "),
  dashboardSidebar(
    h4("Choose a Publisher"),
    selectInput("selection", "",
                choices = publishers),
    actionButton("update", "Change"),
    
    
    hr(),
    
    sidebarMenu(
      
      menuItem("General",tabName = "general",icon = icon("database")),
      menuItem("Sales ",tabName = "Sales",icon = icon("area-chart"),
               menuSubItem("Top Games",tabName = "Sales",icon = icon("bullseye")),
                menuSubItem("By Platform",tabName = "Platform",icon = icon("pie-chart")),
               menuSubItem("By Genre",tabName = "Genre",icon = icon("bar-chart-o")),
               menuSubItem("By Year",tabName = "Year",icon = icon("line-chart"))
               ),
     
      menuItem("Comparison",tabName = "comparison",icon = icon("random")),
      menuItem("Predictions",tabName = "prediction",icon = icon("forward")),
      menuItem("Github Source Code", href="https://github.com/surajvv12/Cricket_sentiment_analysis_twitter/tree/master/Shiny", icon = icon("github")),
      menuItem("Follow us on :-", href="#", newtab=F),
      menuSubItem("Facebook", href="https://www.facebook.com/datasciencezing", icon = icon("facebook")),
      menuSubItem("@datasciencezing", href="https://twitter.com/datasciencezing", icon = icon("twitter")),
      menuSubItem("Blog", href="http://www.datascience-zing.com", icon=icon("hand-o-right"))
      
      
      
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "general",
       
       fluidRow(
         
         tabBox(title = "Video Game Sales Global Analysis",width = 12,
                
                tabPanel(title = tagList(shiny::icon("star"),"Top Platforms"),
                         
                         plotOutput("bar_platform")
                ),
                
                tabPanel(title = tagList(shiny::icon("gamepad"),"Top Publishers"),
                         
                         plotOutput("bar_publisher")
                ), 
                
                tabPanel(title = tagList(shiny::icon("calendar"),"Year Sales"),
                         
                         plotOutput("bar_year")
                ), 
                tabPanel(title = tagList(shiny::icon("usd"),"Year Revenue"),
                         
                         plotOutput("bar_year_revenue")
                ) 
                
                
         )
         
         
       )
       
       
      ),
      
      
      
      tabItem(tabName = "Sales",
              
              fluidRow(
                
                tabBox(title = "Top Selling Games",width = 12,
                       
                       tabPanel(title = tagList(shiny::icon("globe"),"Global"),
                                
                                plotOutput("bar_global1")
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("dollar"),"NA"),
                                
                                plotOutput("bar_NA1")
                       ), 
                       
                       tabPanel(title = tagList(shiny::icon("eur"),"EU"),
                                
                                plotOutput("bar_EU1")
                       ), 
                       tabPanel(title = tagList(shiny::icon("yen"),"JP"),
                                
                                plotOutput("bar_JP1")
                       ), 
                       
                       tabPanel(title = tagList(shiny::icon("map"),"Others"),
                                
                                plotOutput("bar_OTH1")
                       ) 
                )
                
                
              )
              
              
      ),
      
      tabItem(tabName = "Platform",
              
              fluidRow(
                
                tabBox(title = "Top Selling Platforms",width = 12,
                       
                       tabPanel(title = tagList(shiny::icon("globe"),"Global"),
                                
                               box(htmlOutput("pie_global1"),height = 500,width = 300),
                               box(plotOutput("bar_global2"), width = 300)
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("dollar"),"NA"),
                                
                                box(htmlOutput("pie_NA1"),height = 500,width = 300),
                                box(plotOutput("bar_NA2"), width = 300)
                       ),
                       
                      
                       tabPanel(title = tagList(shiny::icon("eur"),"EU"),
                                
                                box(htmlOutput("pie_EU1"),height = 500,width = 300),
                                box(plotOutput("bar_EU2"), width = 300)
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("yen"),"JP"),
                                
                                box(htmlOutput("pie_JP1"),height = 500,width = 300),
                                box(plotOutput("bar_JP2"), width = 300)
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("map"),"OTHERS"),
                                
                                box(htmlOutput("pie_OTH1"),height = 500,width = 300),
                                box(plotOutput("bar_OTH2"), width = 300)
                       )
                )
                
                
              )
              
              
      ),
      
      
      tabItem(tabName = "Genre",
              
              fluidRow(
                
                tabBox(title = "Top Selling Genre",width = 12,
                       
                       tabPanel(title = tagList(shiny::icon("globe"),"Global"),
                                
                                box(htmlOutput("pie_global2"),height = 500,width = 300),
                                box(plotOutput("bar_global3"), width = 300)
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("dollar"),"NA"),
                                
                                box(htmlOutput("pie_NA2"),height = 500,width = 300),
                                box(plotOutput("bar_NA3"), width = 300)
                       ),
                       
                       
                       tabPanel(title = tagList(shiny::icon("eur"),"EU"),
                                
                                box(htmlOutput("pie_EU2"),height = 500,width = 300),
                                box(plotOutput("bar_EU3"), width = 300)
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("yen"),"JP"),
                                
                                box(htmlOutput("pie_JP2"),height = 500,width = 300),
                                box(plotOutput("bar_JP3"), width = 300)
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("map"),"OTHERS"),
                                
                                box(htmlOutput("pie_OTH2"),height = 500,width = 300),
                                box(plotOutput("bar_OTH3"), width = 300)
                       )
                )
                
                
              )
              
              
      ),
      
      tabItem(tabName = "Year",
              
              fluidRow(
                
                tabBox(title = "Sales By Year",width = 12,
                       
                       tabPanel(title = tagList(shiny::icon("globe"),"Global"),
                                
                                htmlOutput("line_global1")
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("dollar"),"NA"),
                                
                                htmlOutput("line_NA1")
                       ),
                       
                       
                       tabPanel(title = tagList(shiny::icon("eur"),"EU"),
                                
                                htmlOutput("line_EU1")
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("yen"),"JP"),
                                
                                htmlOutput("line_JP1")
                       ),
                       
                       tabPanel(title = tagList(shiny::icon("map"),"OTHERS"),
                                
                                htmlOutput("line_OTH1")
                       )
                )
                
                
              )
              
              
      ),
      
      tabItem(tabName = "comparison",
       
       fluidRow(
         
         tabBox(title = "Sales in countries by Year",width = 12,
                
                tabPanel(title = tagList(shiny::icon("globe"),"Comparison"),
                         
                         htmlOutput("line_comparison")
                )
                
                
         )
         
         
       )
       
       
      ),
      
      tabItem(tabName = "prediction",
       
       fluidRow(
         
         tabBox(title = "Prediction of Global Sales",width = 12,
                
                tabPanel(title = tagList(shiny::icon("gavel"),"ARIMA"),
                         
                         plotOutput("forecastArima"),
                        box(
                          h4(" Sales of Previous 5 years and Prediction for next 5 Years"),
                         tableOutput("ArimaPredictedTable"), 
                         height = 500, width = 300),
                        
                        box(
                          h4("ACF and PACF for Residuals "),
                          plotOutput("ResidualsPlot"),height = 500,width = 300)
                         
                ),
                
                tabPanel(title = tagList(shiny::icon("globe"),"ETS"),
                         
                         plotOutput("forecastETS"),
                         h4(" Sales of Previous 5 years and Prediction for next 5 Years"),
                         tableOutput("ETSPredictedTable")
                         
                         
                )
                
                 )
         
         
       )
       
       
      )
      
      
      
      
      
      
      
      
    )
    
    
  )
)




