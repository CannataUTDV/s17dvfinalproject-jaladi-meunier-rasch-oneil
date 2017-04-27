# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)


df1 <- data.frame(query(
  data.world(propsfile = "www/.data.world"),
  dataset="vcjaladi/s-17-dv-project-6", type="sql",
  query="select AreaName, rep16_frac as RepPCT, dem16_frac as DemPCT
  from electiondatabystate
  where AreaName != 'Alaska'
  order by AreaName"
  ))

df2 <- data.frame(query(
  data.world(propsfile = "www/.data.world"),
  dataset="uscensusbureau/acs-2015-5-e-income", type="sql",
  query="select AreaName, sum(B19013_001) / 1000 as median_income
        from USA_All_States
        group by AreaName"))

df3 <- data.frame(query(
  data.world(propsfile = "www/.data.world"),
  dataset="uscensusbureau/acs-2015-5-e-poverty", type="sql",
  query="select AreaName, sum(B17001_002) / 1000 as numinpov
         from USA_All_States
         group by AreaName"))

df4 <- data.frame(query(
  data.world(propsfile = "www/.data.world"),
  dataset="uscensusbureau/acs-2015-5-e-race", type="sql",
  query="select AreaName, sum(B02001_002) / 1000 as sum_race
  
  from USA_All_States
  group by AreaName"))

# Pairwise inner joins of dataframes
print("Joining Data Frames")
dfs <- dplyr::inner_join(df1,df2, by ="AreaName")
dfs <- dplyr::inner_join(dfs,df3, by= "AreaName")
dfs <- dplyr::inner_join(dfs,df4, by= "AreaName")

#View(dfs)

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
    geom_col(aes(x=AreaName, y=median_income, fill=victory_margin)) +
    geom_text(aes(x=AreaName, y=median_income, label=median_income, hjust=-0.25)) +
    labs(title="Median Income for Each State by Election Victory Margin", y="Medium Income (Thousands USD)", x="State") +
    coord_flip()

  })
  output$plot2 <- renderPlot({ggplot(df_full()) +
      theme(axis.text.x=element_text(size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_col(aes(x=AreaName, y=numinpov, fill=victory_margin)) +
      geom_text(aes(x=AreaName, y=numinpov, label=numinpov, hjust=-0.25)) +
      labs(title="Population Below Poverty Line for Each State by Election Victory Margin", y="Citizens in Poverty (Thousands)", x="State") +
      coord_flip()
  })
  output$plot3 <- renderPlot({ggplot(df_full()) +
      theme(axis.text.x=element_text(size=16, vjust=0.5)) +
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_col(aes(x=AreaName, y=sum_race, fill=victory_margin)) +
      geom_text(aes(x=AreaName, y=sum_race, label=sum_race, hjust=-0.25)) +
      labs(title="Population of Whites for Each State by Election Victory Margin", y="Citizens Identifying as White (Thousands)", x="State") +
      coord_flip()

  })
})
# End Crosstab Tab ___________________________________________________________