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

# استخراج جدول جمعیت
population_table <- web_data %>%
  html_node('table') %>%
  html_table(fill = TRUE)

# تغییر نام ستون‌ها
colnames(population_table) <- c("Rank", "Country", "Population", "Yearly_Change", "Net_Change",
                                "Density", "Land_Area", "Migrants", "Fertility_Rate", "Median_Age",
                                "Urban_Pop", "World_Share")

# تبدیل جمعیت به عدد صحیح
population_table <- population_table %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))

# دریافت داده‌های نقشه
world_map <- map_data("world")

# ترکیب داده‌های نقشه با جمعیت
world_map <- world_map %>%
  left_join(population_table, by = c("region" = "Country"))

# نمایش "Data Not Available" برای کشورهایی که جمعیت ندارند
world_map <- world_map %>%
  mutate(
    Population = as.numeric(Population),
    hover_text = ifelse(is.na(Population), 
                        paste0("Country: ", region, "<br>Population: Data Not Available"), 
                        paste0("Country: ", region, "<br>Population: ", formatC(Population, format = "f", big.mark = ",", digits = 0)))
  )

# تعریف کشورها بر اساس قاره‌ها
asia_countries <- c(
  "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei",
  "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", "Iran", "Iraq", "Israel",
  "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia",
  "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine",
  "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria",
  "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan", "United Arab Emirates",
  "Uzbekistan", "Vietnam", "Yemen"
)


africa_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde",
  "Cameroon", "Central African Republic", "Chad", "Comoros", "Democratic Republic of the Congo",
  "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon",
  "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia",
  "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique",
  "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "São Tomé and Príncipe",
  "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan",
  "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
)


america_countries <- c(
  "Canada", "United States", "Mexico", 
  "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama",
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay",
  "Peru", "Suriname", "Uruguay", "Venezuela"
)


europe_countries <- c(
  "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium",
  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
  "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland",
  "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania",
  "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
  "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom",
  "Vatican City"
)

oceania_countries <- c(
  "Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", "Nauru", "New Zealand",
  "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu"
)

# افزودن ستون قاره‌ها به داده‌ها
world_map <- world_map %>%
  mutate(Continent = case_when(
    region %in% asia_countries ~ "Asia",
    region %in% africa_countries ~ "Africa",
    region %in% america_countries ~ "America",
    region %in% europe_countries ~ "Europe",
    region %in% oceania_countries ~ "Oceania",
    TRUE ~ "Other"
  ))

# تعریف اپلیکیشن Shiny
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

server <- function(input, output) {
  output$worldMap <- renderPlotly({
    # فیلتر داده‌ها بر اساس قاره انتخاب‌شده
    filtered_data <- if (input$continent == "All") {
      world_map
    } else {
      world_map %>% filter(Continent == input$continent)
    }
    
    # رسم نقشه
    p <- ggplot() +
      geom_polygon(data = filtered_data, aes(x = long, y = lat, group = group, fill = Population, text = hover_text),
                   color = "black", size = 0.3) +  
      scale_fill_gradientn(colors = c("yellow", "orange", "red", "darkred"),
                           values = scales::rescale(c(min(filtered_data$Population, na.rm = TRUE), 
                                                      median(filtered_data$Population, na.rm = TRUE),
                                                      mean(filtered_data$Population, na.rm = TRUE),
                                                      max(filtered_data$Population, na.rm = TRUE))),
                           na.value = "gray", labels = scales::label_number(scale_cut = scales::cut_short_scale()))  +
      coord_fixed(1.3) +  
      theme_minimal(base_size = 14) +  
      labs(title = paste("World Population -", input$continent),
           x = "Longitude", y = "Latitude",
           fill = "Population (people)")
    
    # تبدیل به نقشه تعاملی با `plotly`
    ggplotly(p, tooltip = "text", width = 1200, height = 800) %>%
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
