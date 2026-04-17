#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
SeasonChoices <- c("2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15")
PositionChoices <- c("Forwards", "Defensemen")
ClusteringChoices <- c("Production", "Production + Size")

plot_lookup_df <- expand.grid(
  season_label = SeasonChoices,
  position_label = PositionChoices,
  clustering_label = ClusteringChoices,
  stringsAsFactors = FALSE) %>%
  mutate(
    season_code = gsub("-", "_", season_label),
    position_code = case_when(
      position_label == "Forwards" ~ "Forwards", 
      position_label == "Defensemen" ~ "Defenseman", 
      TRUE ~ NA_character_
    ),
    cluster_suffix = case_when( 
      clustering_label == "Production" ~ "", 
      clustering_label == "Production + Size" ~ "HW", 
      TRUE ~ NA_character_
    ),
    GGPlotName = paste0("Blackhawks", position_code, cluster_suffix, season_code, "PCAScoresGGPlot.rds"),
    PlotNote = c("Cluster 1 is top 6 level forwards, Cluster 2 is bottom 6 level forwards, Cluster 3 is middle 6 level forwards",
                 "Cluster 1 is bottom 6 level forwards, Cluster 2 is middle 6 level forwards, Cluster 3 is top 6 level forwards",
                 "Cluster 1 is middle 6 level forwards, Cluster 2 is top 6 level forwards, Cluster 3 is bottom 6 level forwards",
                 "Cluster 1 is top 6 level forwards, Cluster 2 is bottom 6 level forwards, Cluster 3 is middle 6 level forwards",
                 "Cluster 1 is middle 6 level forwards, Cluster 3 is bottom 6 level forwards, Cluster 3 is top 6 level forwards",
                 "Cluster 1 is bottom 6 level forwards, Cluster 2 is middle 6 level forwards, Cluster 3 is top 6 level forwards",
                 "Cluster 1 is bottom pairing level defensemen, Cluster 2 is second pairing level defensemen, Cluster 3 is top pairing level defensemen",
                 "Cluster 1 is top pairing level defensemen, Cluster 2 is second pairing level defensemen, Cluster 3 is bottom pairing level defensemen",
                 "Cluster 1 is second pairing level defensemen, Cluster 2 is bottom pairing level defensemen, Cluster 3 is top pairing level defensemen",
                 "Cluster 1 is top pairing level defensemen, Cluster 2 is second pairing level defensemen, Cluster 3 is bottom pairing level defensemen",
                 "Cluster 1 is bottom pairing level defensemen, Cluster 2 is top pairing level defensemen, Cluster 3 is second pairing level defensemen",
                 "Cluster 1 is top pairing level defensemen, Cluster 2 is second pairing level defensemen, Cluster 3 is bottom pairing level defensemen",
                 "Cluster 1 is top 6 level forwards, Cluster 2 is middle 6 level forwards, Cluster 3 is bottom 6 level forwards",
                 "Cluster 2 is top 6 level forwards, Clusters 1 and 3 are both lower level of production, the main difference is there are slightly better producers in Cluster 1",
                 "Cluster 1 is middle 6 level forwards, Cluster 2 is bottom six level forwards, Cluster 3 is top six level forwards",
                 "Cluster 1 is top 6 level forwards, Cluster 2 is bottom six level forwards, Cluster 3 is middle six level forwards",
                 "Cluster 1 is top 6 level forwards, Clusters 2 and 3 represent middle and bottom 6 forwards and are very similar, Cluster 2 is slightly better in production",
                 "Cluster 1 is bottom 6 level forwards, Cluster 2 is top 6 level forwards, and cluster 3 is middle six level forwards",
                 "Cluster 1 is second pairing level defenseman, Cluster 2 is top pairing level defensemen, Cluster 3 is bottom pairing level defensemen",
                 "Cluster 1 is second pairing level defensemen, Cluster 2 is bottom pairing level defensemen, Cluster 3 is top pairing level defensemen",
                 "Cluster 1 and 3 are both top 4 defensemen clusters with Cluster 3 having slightly better offensive output, Cluster 2 is bottom pairing level defensemen",
                 "Cluster 1 is second pairing level defensemen, Cluster 2 is bottom pairing level defensemen, Cluster 3 is top pairing level defenesmen",
                 "Cluster 1 is second pairing level defensemen, Cluster 2 is bottom pairing level defenesmen, Cluster 3 is top pairing level defensemen",
                 "Cluster 1 is bottom pairing level defenesmen, Cluster 2 is top pairing level defensemen, Cluster 3 is second pairing level defensemen")
  )
# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Chicago Blackhawks Dynasty Player Clustering (2009-15)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("season", "Select Season", choices = SeasonChoices),
            selectInput("position", "Select Position Group", choices = PositionChoices),
            selectInput("clustering", "Select Data For Clustering", choices = ClusteringChoices)),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("ClusterPlot", height = "600px"),
          h3(textOutput("Note"))
        )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  selected_file_path <- reactive({
    req(input$season, input$position, input$clustering) 
    plot_info <- plot_lookup_df %>%
      filter(
        season_label == input$season,
        position_label == input$position,
        clustering_label == input$clustering
      )
    return(plot_info$GGPlotName)
  })
  current_ggplot <- reactive({
    path_to_load <- selected_file_path()
    return(readRDS(path_to_load))
  })
  output$ClusterPlot <- renderPlotly({
    plot_to_render <- current_ggplot()
    plotly_obj <- ggplotly(plot_to_render, tooltip = "text")
    return(plotly_obj)
  })
  output$Note <- renderText({
    plot_info <- plot_lookup_df %>%
      filter(
        season_label == input$season,
        position_label == input$position,
        clustering_label == input$clustering
      )
    return(plot_info$PlotNote)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
