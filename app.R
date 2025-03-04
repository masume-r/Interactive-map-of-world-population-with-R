library(shiny)
library(ggplot2)
library(plotly)
library(maps)
library(dplyr)
library(rvest)
library(magrittr)

# Obtaining demographic data
url <- 'https://www.worldometers.info/world-population/population-by-country/'
web_data <- read_html(url)
population_table <- web_data %>%
  html_node('table') %>%
  html_table(fill = TRUE)
colnames(population_table) <- c("Rank", "Country", "Population", "Yearly_Change", "Net_Change",
                                "Density", "Land_Area", "Migrants", "Fertility_Rate", "Median_Age",
                                "Urban_Pop", "World_Share")
population_table <- population_table %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))

# map data
world_map <- map_data("world") %>%
  left_join(population_table, by = c("region" = "Country"))

world_map <- world_map %>%
  mutate(
    Population = as.numeric(Population),
    hover_text = ifelse(is.na(Population), 
                        paste0("Country: ", region, "<br>Population: Data Not Available"), 
                        paste0("Country: ", region, "<br>Population: ", formatC(Population, format = "f", big.mark = ",", digits = 0)))
  )

# User interface
ui <- fluidPage(
  titlePanel("Interactive World Population Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select Continent:",
                  choices = c("All", "Asia", "Africa", "America", "Europe", "Oceania"),
                  selected = "All")
    ),
    mainPanel(
      plotlyOutput("worldMap")
    )
  )
)

# server
server <- function(input, output) {
  output$worldMap <- renderPlotly({
    filtered_data <- if (input$continent == "All") {
      world_map
    } else {
      world_map %>% filter(Continent == input$continent)
    }
    
    p <- ggplot() +
      geom_polygon(data = filtered_data, aes(x = long, y = lat, group = group, fill = Population),
                   color = "black", size = 0.3) +  
      scale_fill_gradientn(colors = c("yellow", "orange", "red", "darkred"),
                           values = scales::rescale(c(min(filtered_data$Population, na.rm = TRUE), 
                                                      median(filtered_data$Population, na.rm = TRUE),
                                                      mean(filtered_data$Population, na.rm = TRUE),
                                                      max(filtered_data$Population, na.rm = TRUE))),
                           na.value = "gray") +  
      coord_fixed(1.3) +  
      theme_minimal(base_size = 14) +  
      labs(title = paste("World Population -", input$continent),
           x = "Longitude", y = "Latitude",
           fill = "Population (people)")

    ggplotly(p, tooltip = "hover_text", width = 1200, height = 800) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        hovermode = "closest",
        yaxis = list(tickformat = ",.0f"),
        xaxis = list(tickformat = ",.0f"),
        annotations = list(
          list(
            x = 0.5,  
            y = 1.15,  
            text = "<b>Hover over a country to see details</b>",  
            showarrow = FALSE,
            xref = "paper",  
            yref = "paper",  
            font = list(size = 18, color = "black")
          )
        )
      )
  })
}

# اجرای برنامه
shinyApp(ui, server)
