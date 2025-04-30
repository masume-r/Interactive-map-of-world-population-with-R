# 🌍 Interactive World Population Map

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
[![Watch Demo]](https://github.com/masume-r/Interactive-map-of-world-population-with-R/releases/download/v2.0-beta/inmap.mp4)

## 📌 기술 평가 항목 대응 내역

본 프로젝트는 R 언어와 다양한 시각화/웹 라이브러리를 활용하여 세계 인구 데이터를 실시간으로 시각화하는 대화형 웹 애플리케이션을 구현하였습니다. 다음의 기술 항목을 포함합니다:

- **[1. 데이터사이언스 공통]**  
  R 언어 및 `ggplot2`, `plotly`, `dplyr`, `rvest`, `maps`, `magrittr` 라이브러리를 활용하여 데이터 수집, 처리, 시각화를 수행하였습니다.

- **[3. 데이터 수집 및 정제 ]**  
  `rvest`를 사용하여 Worldometers 웹사이트에서 실시간 국가별 인구 데이터를 크롤링하고, 분석 가능한 형태로 정제하였습니다.

- **[4. 데이터 수집 및 정제 ]**  
  수집한 데이터를 대륙 및 국가별로 정렬하고, 시각화를 통해 인구 분포를 직관적으로 파악할 수 있는 구조로 구성하였습니다.

- **[8. 데이터 시각화]**  
  `ggplot2`로 세계지도를 생성하고, `plotly`를 활용하여 사용자가 대륙을 선택하거나 국가 위에 마우스를 올렸을 때 실시간 정보를 확인할 수 있도록 인터랙티브하게 시각화하였습니다.


