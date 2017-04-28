#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histograms", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Scatter Plots", tabName = "scatter", icon = icon("dashboard")),
      menuItem("Crosstabs", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Election Bar Charts", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Box Plot content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         sliderInput("TVM1", "Select Cutoff for Trump Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         sliderInput("HVM1", "Select Cutoff for Hillary Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         actionButton(inputId= "click1", label ="Generate Box Plot"),
                         hr(), # Add space after button.
                         plotOutput("plot1", height=1000)
                ))
      ),
      # End Histogram tab content.
      
      # Begin Histogram content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         sliderInput("TVM1", "Select Cutoff for Trump Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         sliderInput("HVM1", "Select Cutoff for Hillary Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         actionButton(inputId= "click2", label ="Generate Histogram"),
                         hr(), # Add space after button.
                         plotOutput("plot2", height=1000)
                ))
      ),
      # End Histogram tab content.
      
      # Begin Scatterplot content.
      tabItem(tabName = "scatter",
              tabsetPanel(
                tabPanel("Data",  
                         sliderInput("TVM1", "Select Cutoff for Trump Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         sliderInput("HVM1", "Select Cutoff for Hillary Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         actionButton(inputId= "click3", label ="Generate Scatter Plot"),
                         hr(), # Add space after button.
                         plotOutput("plot3", height=1000)
                ))
      ),
      # End Scatterplot tab content.
      
      # Begin Crosstabs content.
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("Data",  
                         sliderInput("TVM1", "Select Cutoff for Trump Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         sliderInput("HVM1", "Select Cutoff for Hillary Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         actionButton(inputId= "click4", label ="Generate Crosstab"),
                         hr(), # Add space after button.
                         plotOutput("plot4", height=1000)
                ))
      ),
      # End Crosstabs tab content.
      
      
      
      # Begin Bar Charts tab content.
      tabItem(tabName = "barchart",
              tabsetPanel(
                tabPanel("Data",  
                         sliderInput("TVM1", "Select Cutoff for Trump Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         sliderInput("HVM1", "Select Cutoff for Hillary Landslide Victory:", 
                                     min = .5, max = 1,  value = .75),
                         actionButton(inputId= "click5", label ="Generate Bar Chart"),
                         hr(), # Add space after button.
                         plotOutput("plot5", height=1000)
                ))
      )
      # End Bar Charts tab content.
    )
  )
)

