# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require("plotly")

dfs <- data.frame(query(
  data.world(propsfile = "www/.data.world"),
  dataset="vcjaladi/s-17-dv-final-project", type="sql",
  query="SELECT CountyElections.State, CountyElections.Region, sum(CountyElections.`Total.Population`), sum(CountyElections.votes), sum(CountyElections.votes16_trumpd), sum(CountyElections.votes16_clintonh), sum(CountyElections.votes16_johnsong), sum(CountyElections.votes16_steinj),avg(CountyElections.rep16_frac) as RepPCT, avg(CountyElections.dem16_frac) as DemPCT, avg(CountyElections.`At.Least.Bachelor.s.Degree`), avg(CountyElections.`At.Least.High.School.Diploma`), avg(CountyElections.`Less.Than.High.School`),avg(CountyElections.`Graduate.Degree`), avg(CountyElections.`White.not.Latino.Population`), avg(CountyElections.`African.American.Population`),avg(CountyElections.`Native.American.Population`), avg(CountyElections.`Asian.American.Population`),avg(CountyElections.`Population.some.other.race.or.races`), avg(CountyElections.`Latino.Population`),avg(CountyElections.`Management.professional.and.related.occupations`), avg(CountyElections.`Service.occupations`),avg(CountyElections.`Sales.and.office.occupations`), avg(CountyElections.`Farming.fishing.and.forestry.occupations`),avg(CountyElections.`Construction.extraction.maintenance.and.repair.occupations`), avg(CountyElections.`Production.transportation.and.material.moving.occupations`), avg(CountyElections.`Adult.obesity`), avg(CountyElections.Diabetes), avg(CountyElections.Uninsured), avg(CountyElections.Unemployment), 
Income.AreaName, sum(Income.B19013_001) / 1000 as median_income_thousands, 
  Poverty.AreaName, sum(Poverty.B17001_002) / 1000 as numinpov_thousands, 
  Race.AreaName, sum(Race.B02001_002) / 1000 as sum_white_thousands
  
  FROM `finalproject_ElectionsData.csv/finalproject_ElectionsData` CountyElections 
  join uscensusbureau.`acs-2015-5-e-income`.`USA_All_States.csv/USA_All_States` Income ON (CountyElections.State = Income.AreaName)
  join uscensusbureau.`acs-2015-5-e-poverty`.`USA_All_States.csv/USA_All_States` Poverty ON (CountyElections.State = Poverty.AreaName)
  join uscensusbureau.`acs-2015-5-e-race`.`USA_All_States.csv/USA_All_States` Race ON (CountyElections.State = Race.AreaName)
  WHERE CountyElections.State != 'Alaska'
  GROUP BY CountyElections.State
  ORDER BY CountyElections.State"
))

#View(dfs)

region_list <- unique(dfs$Region)
region_list <- append(list("All" = "All"), region_list)
swing_states <- c("Colorado","Florida","Iowa", "Michigan", "Minnesota", "Nevada", "New Hampshire", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "Wisconsin")

shinyServer(function(input, output) { 
  
  ### ADD UNIQUE WIDGETS FOR SLIDER TABS ###
  # These widgets are for the Box Plot tab.
  TVM <- reactive({input$TVM1})
  HVM <- reactive({input$HVM1})
  
# Begin Data Tab ------------------------------------------------------------------ 
  df_dat <- eventReactive(input$click0, {
    dfs %>% dplyr::mutate(voter_turnout= sum(votes/Total.Population))
  })
  
  output$Data1 <- renderDataTable({DT::datatable(df_dat(),
                                                 rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  # End Data Tab ---------------------------------------------------------------------------
# Begin Box Plot Tab ------------------------------------------------------------------ 
  
  # Parameterization 
  df_bp <- eventReactive(input$click1, {
    
    dfs %>% dplyr::mutate(
      victory_margin = if_else(RepPCT >0.5,
                               (if_else(RepPCT > TVM(), 'Trump Landslide Victory',
                                        'Trump Victory')
                               ),
                               if_else(DemPCT > HVM(),
                                       'Hillary Landslide Victory',
                                       'Hillary Victory')))
  })
  
  # # output the plot
  output$plot1 <- renderPlotly({
    p<- ggplot(df_bp()) +
      theme(axis.text.x=element_text(size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_boxplot(aes(x=victory_margin, y=Unemployment, fill=victory_margin)) +
      labs(title="Unemployment by Victory Margin", y="Unemployment (%)", x="Victory Type")
    ggplotly(p)
    
  })
  
  
  # End Boxplot Tab ___________________________________________________________  
  
  
# Begin Histogram Tab --------------------------------------------------------------
  
  # Parameterization 
  df_hg <- eventReactive(input$click2, {
    
    dfs %>% dplyr:: mutate(voter_turnout= sum(votes/Total.Population))
  })

  # # output the plot
  output$plot2 <- renderPlotly({
    p<- ggplot(df_hg()) +
      theme(axis.text.x=element_text(size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_histogram(aes(x=Less.Than.High.School, binwidth=1)) +
      labs(title="States with Education Level Below High School", y="Count", x="% with Education Level Less than High School") 
    ggplotly(p)
  })
  
  
  # End Histogram Tab ___________________________________________________________   
  
  
  
  
# Begin Scatterplot Tab ------------------------------------------------------------------ REMOVE SLIDERS, FIX COLOR?
  
  # Parameterization 
  df_sp <- eventReactive(input$click3, {
    
    dfs %>% dplyr::mutate(
      poverty_ratio = (numinpov_thousands * 1000) / Total.Population)
    
  })
  
  # # output the plot
  output$plot3 <- renderPlot({
    ggplot(df_sp()) +
      theme(axis.text.x=element_text(size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_point(aes(x=Uninsured, y=DemPCT, color=poverty_ratio, size=poverty_ratio)) +
      geom_smooth(aes(x=Uninsured, y=DemPCT)) +
      labs(title="Percentage of Democratic Voters vs. Percentage Uninsured", y="Democratic Voters (%)", x="Uninsured (%)")+ 
      scale_color_gradient(low="yellow", high="red")
  })

  output$plotZ <- renderPlot({
    brush = brushOpts(id="plot_brush", delayType = "throttle", delay = 30)
    bdf=brushedPoints(df_sp(), input$plot_brush)
    if( !is.null(input$plot_brush) ) {
      df_sp() %>% dplyr::filter(State %in% bdf[, "State"]) %>%
    ggplot() + geom_col(aes(x=State, y=Uninsured, fill=poverty_ratio, size=4)) + guides(size=FALSE) + 
        scale_fill_gradient(low="yellow",high="red")
    }
  })


  
# End Scatterplot Tab ___________________________________________________________   
  
  

  
# Begin Crosstab Tab ------------------------------------------------------------------ 
  # Parameterization 
  df_ct <- eventReactive(input$click4, {
    
    dfs %>% dplyr::mutate(
      victory_margin = if_else(RepPCT >0.5,
                               (if_else(RepPCT > TVM(), 'Trump Landslide Victory',
                                        'Trump Victory')
                               ),
                               if_else(DemPCT > HVM(),
                                       'Hillary Landslide Victory',
                                       'Hillary Victory'))) %>% dplyr::group_by(Region)})
  
  # # output the plot
  output$plot4 <- renderPlotly({
    p<- ggplot(df_ct()) +
      theme(axis.text.x=element_text(size=10, vjust=0.5)) +
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_tile(aes(x=victory_margin, y=State, fill=median_income_thousands)) +
      geom_text(aes(x=victory_margin, y=State, label=median_income_thousands)) +
      labs(title="Median Incomes by Victory Margin and Region", y="Region", x="Victory Type") 
    ggplotly(p)
  })
  
  
# End Crosstab Tab ___________________________________________________________ 
  
  
  
  
# Begin Barchart Tab ------------------------------------------------------------------ FIX COLOR SCHEME
  
  # Parameterization 
  df_bc <- eventReactive(input$click5, {
  
  dfs %>% dplyr::mutate(
    victory_margin = if_else(RepPCT >0.5,
                             (if_else(RepPCT > TVM(), 'Trump Landslide Victory',
                                      'Trump Victory')
                                      ),
                     if_else(DemPCT > HVM(),
                             'Hillary Landslide Victory',
                                      'Hillary Victory'))) %>% dplyr::filter(State %in% swing_states)
  })
  
  # # output the plot
  output$plot5 <- renderPlot({
    ggplot(df_bc()) +
    theme(axis.text.x=element_text(size=16, vjust=0.5)) +
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_col(aes(x=State, y=median_income_thousands, fill=victory_margin)) +
    geom_text(aes(x=State, y=median_income_thousands, label=median_income_thousands, hjust=-0.25)) +
    geom_hline(aes(yintercept= mean(median_income_thousands))) +
    labs(title="Medium Income in the Swing States by Victor", y="Medium Income (Thousands USD)", x="State") +
    coord_flip() + 
    scale_fill_brewer(palette="Set2")
  
  })
  

# End Barchart Tab ___________________________________________________________
  

# End ShinyServer
})