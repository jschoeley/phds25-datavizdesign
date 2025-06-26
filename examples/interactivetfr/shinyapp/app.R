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
library(sf)
library(ggplot2)
library(plotly)

maptemplates <- readRDS('shinydata.rds')
regions <-
  maptemplates$fert |> st_cast("MULTIPOLYGON")
background <-
  maptemplates$background |> st_cast("MULTIPOLYGON")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Dynamic TFR"),
  
  # Sidebar with a slider and radio button
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Year",
                  min = 2014,
                  max = 2023,
                  value = 2014),
      radioButtons("pal",
                   "Palette",
                   choices = c(
                     "Viridis" = 'viridis',
                     "Magma" = 'magma'
                   ),
                   selected = 'viridis')
    ),
    
    # Show a plot
    mainPanel(
      plotlyOutput("map")
    )
  )
)

# Define server logic required to draw a map
server <- function(input, output) {
  
  dat_subset <- reactive({
    dat_subset <-
      regions |>
      filter(year == input$year)
  })
  
  output$map <- renderPlotly({
    
    p <-
      ggplot() +
      # background
      geom_sf(
        data = background
      ) +
      # tfr choropleth
      geom_sf(
        aes(fill = tfr), color = NA,
        data = dat_subset()
      ) +
      # france outline
      geom_sf(
        fill = NA, lwd = 1, color = 'grey50',
        data = maptemplates$outline
      ) +
      # # cities
      geom_sf(
        data = maptemplates$cities,
        shape = 21, color = 'white', fill = 'black',
        size = 2.5
      ) +
      geom_sf_label(
        aes(label = city),
        hjust = -0.1, vjust = -0.1,
        alpha = 0.6,
        # no label outline
        label.size = 0,
        data = maptemplates$cities
      ) +
      scale_fill_viridis_c(
        option = input$pal,
        limits = c(0.9, 2.5),
        oob = scales::squish
      ) +
      theme_void()
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
