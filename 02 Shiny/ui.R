#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Election Bar Charts", tabName = "BarCharts", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Bar Charts tab content.
      tabItem(tabName = "BarCharts",
        tabsetPanel(
            tabPanel("Data",  
              sliderInput("TVM1", "Select Cutoff for Trump Landslide Victory:", 
                          min = .5, max = 1,  value = .75),
              sliderInput("HVM1", "Select Cutoff for Hillary Landslide Victory:", 
                          min = .5, max = 1,  value = .75),
              actionButton(inputId= "click1", label ="Generate Bar Charts"),
              hr(), # Add space after button.
              plotOutput("plot1", height=1000),
              hr(),
              plotOutput("plot2", height=1000),
              hr(),
              plotOutput("plot3", height=1000)
            ))
        )
      # End Crosstab tab content.
    )
  )
)

