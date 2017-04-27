# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)


dfs <- data.frame(query(
  data.world(propsfile = "www/.data.world"),
  dataset="vcjaladi/s-17-dv-final-project", type="sql",
  query="SELECT CountyElections.State, sum(CountyElections.`Total.Population`), sum(CountyElections.votes), sum(CountyElections.votes16_trumpd), sum(CountyElections.votes16_clintonh), sum(CountyElections.votes16_johnsong), sum(CountyElections.votes16_steinj),avg(CountyElections.rep16_frac) as RepPCT, avg(CountyElections.dem16_frac) as DemPCT, avg(CountyElections.`At.Least.Bachelor.s.Degree`), avg(CountyElections.`At.Least.High.School.Diploma`), avg(CountyElections.`Less.Than.High.School`),avg(CountyElections.`Graduate.Degree`), avg(CountyElections.`White.not.Latino.Population`), avg(CountyElections.`African.American.Population`),avg(CountyElections.`Native.American.Population`), avg(CountyElections.`Asian.American.Population`),avg(CountyElections.`Population.some.other.race.or.races`), avg(CountyElections.`Latino.Population`),avg(CountyElections.`Management.professional.and.related.occupations`), avg(CountyElections.`Service.occupations`),avg(CountyElections.`Sales.and.office.occupations`), avg(CountyElections.`Farming.fishing.and.forestry.occupations`),avg(CountyElections.`Construction.extraction.maintenance.and.repair.occupations`), avg(CountyElections.`Production.transportation.and.material.moving.occupations`), avg(CountyElections.`Adult.obesity`), avg(CountyElections.Diabetes), avg(CountyElections.Uninsured), avg(CountyElections.Unemployment), 
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

View(dfs)

shinyServer(function(input, output) { 
  # These widgets are for the Crosstabs tab.
  TVM <- reactive({input$TVM1})
  HVM <- reactive({input$HVM1})
  
# Begin Crosstab Tab ------------------------------------------------------------------
  
  # Parameterization 
  df_full <- eventReactive(input$click1, {
  
  dfs %>% dplyr::mutate(
    victory_margin = if_else(RepPCT >0.5,
                             (if_else(RepPCT > TVM(), 'Trump Landslide Victory',
                                      'Trump Victory')
                                      ),
                     if_else(DemPCT > HVM(),
                             'Hillary Landslide Victory',
                                      'Hillary Victory')))
  })
  
  # # output the plots
  output$plot1 <- renderPlot({ggplot(df_full()) +
    theme(axis.text.x=element_text(size=16, vjust=0.5)) +
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_col(aes(x=AreaName, y=median_income_thousands, fill=victory_margin)) +
    geom_text(aes(x=AreaName, y=median_income_thousands, label=median_income_thousands, hjust=-0.25)) +
    labs(title="Median Income for Each State by Election Victory Margin", y="Medium Income (Thousands USD)", x="State") +
    coord_flip()

  })
  output$plot2 <- renderPlot({ggplot(df_full()) +
      theme(axis.text.x=element_text(size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_col(aes(x=AreaName, y=numinpov_thousands, fill=victory_margin)) +
      geom_text(aes(x=AreaName, y=numinpov_thousands, label=numinpov_thousands, hjust=-0.25)) +
      labs(title="Population Below Poverty Line for Each State by Election Victory Margin", y="Citizens in Poverty (Thousands)", x="State") +
      coord_flip()
  })
  output$plot3 <- renderPlot({ggplot(df_full()) +
      theme(axis.text.x=element_text(size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_col(aes(x=AreaName, y=sum_white_thousands, fill=victory_margin)) +
      geom_text(aes(x=AreaName, y=sum_white_thousands, label=sum_white_thousands, hjust=-0.25)) +
      labs(title="Population of Whites for Each State by Election Victory Margin", y="Citizens Identifying as White (Thousands)", x="State") +
      coord_flip()

  })
})
# End Crosstab Tab ___________________________________________________________