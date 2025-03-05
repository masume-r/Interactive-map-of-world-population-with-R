🌍 Interactive World Population Map

📊 An interactive map visualizing global population data in a user-friendly way!

This project provides an interactive world population map built using R, Shiny, ggplot2, and Plotly. The map allows users to explore global population distribution with intuitive filtering options.

🎯 Why This Visualization?

Understanding population distribution across the world is crucial for:
✅ Demographic Analysis – Compare population sizes across continents.
✅ Urban Planning – See which regions have high population density.
✅ Data Transparency – Uses real-time data from Worldometers.
✅ Intuitive Filtering – Users can explore the population by continent or the entire world.
🚀 Key Features

✅ 📌 Interactive Map:

    The map dynamically updates based on the selected continent.
    Hover over any country to see its exact population.
    Countries without data are displayed in gray with a "Data Not Available" label.

✅ 🌍 Continent-Based Filtering:

    Easily switch between continents: Asia, Africa, America, Europe, Oceania, or the entire world.
    Each country is colored based on its population size (Yellow → Orange → Red → Dark Red).

✅ 📊 Smooth Hover Animation:

    Hovering over a country displays its population in real-time.
    Population numbers are formatted properly for readability (1B, 500M, etc.).

✅ 🔄 Real-Time Data from Worldometers

    The app scrapes live population data from Worldometers.
    This ensures up-to-date and accurate demographic information.


## 🚀 Installation
1. Install R and RStudio.
2. Install required packages:
   ```r
   install.packages(c("shiny", "ggplot2", "plotly", "dplyr", "maps", "rvest", "magrittr"))

   git clone https://github.com/yourusername/world-population-map.git

   shiny::runApp("app.R")

   This project leverages the power of Shiny for interactivity, ggplot2 for mapping, and Plotly for an interactive user experience

🎥 Watch how the app works! Click play below:

