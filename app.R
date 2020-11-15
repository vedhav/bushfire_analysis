library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(shinyjs)


total_bushfire_per_day <- read_csv("total_bushfire_per_day.csv")
names(total_bushfire_per_day) <- c("date", "count")
total_bushfire_per_day$date <- as.POSIXct(total_bushfire_per_day$date, format = "%B %d, %Y") %>% as.Date()
bushfire_distribution <- read_csv("bushfire_distribution.csv")
fire_map <- read_csv("fire_map.csv")
property_area_burned_over20 <- read_csv("property_area_burned_over20.csv")
rainfall_over_decade <- read_csv("rainfall_over_decade.csv") %>%
  select(year = Year, rain_color = "Rain Color",
    avg_rainfall = "Avg. Annual Rainfall in mm", diff_rainfall = "Difference in Avg. Annual Rainfall in mm")
temp_over_decade <- read_csv("temp_over_decade.csv") %>%
  select(year = Year, temp_color = "Avg Temp Color",
    avg_temp = "Avg. Annual Avg Temp", diff_temp = "Difference in Avg. Annual Avg Temp") %>%
  mutate(diff_color = ifelse(diff_temp > 0, "green", "red"))
# source: https://www.kaggle.com/nagarajbhat/australian-bush-fire-satellite-data-nasa
# fire_nrt_V1_101674 <- read_csv("fire_nrt_V1_101674.csv") %>%
#   select(date = acq_date, latitude, longitude, brightness = bright_ti4) %>%
#   group_by(date, latitude, longitude) %>% summarise(brightness = mean(brightness))
new_fire_map <- readRDS("new_fire_map.rds")
master_weather_data <- read_csv("master_weather_data.csv") %>% filter(date >= "2019-09-4") %>% filter(date <= "2020-01-13")
wind_directions <- tibble(
  symbols = c("W", "NE", "WSW", "N", "ENE", "NW", "WNW", "NNW",
    "SSE", "NNE", "SW", "SE", "ESE", "E", "SSW", "S"),
  full_forms = c("West", "North East", "West -> South West", "North",
    "East -> North East", "North West", "West -> North West", "North -> North West",
    "South -> South East", "North -> North East", "South West", "South East",
    "East -> South East", "East", "South -> South West", "South")
)

popUpWindow <- function(
  popUpText, title = NULL, footer = NULL, easyClose = TRUE,
  color = "#333", bg_color = "#f7f7f7") {
  tags$div(
    class = "showmodal",
    showModal(
      modalDialog(
        style = paste0('color: ', color, '; background-color: ', bg_color),
        title = title, tags$div(HTML(popUpText), align = "center"), footer = footer, easyClose = easyClose
      )
    )
  )
}


ui <- bs4DashPage(
  title  = "Bushfire analysis",
  useShinyjs(),
  tags$head(
    tags$style(HTML("hr {border-top: 0.5px solid #922E04;}")),
    tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png")
  ),
  bs4TabCard(
    title = "",
    elevation = 2,
    id = "main_tabs",
    width = 12,
    collapsible = FALSE, 
    closable = FALSE,
    bs4TabPanel(
      tabName = "Bushfire",
      fluidRow(
        column(
          12, align = "center",
          extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }"),
          plotlyOutput("number_of_fires", height = "32vh")
        ),
        column(12, hr()),
        column(
          12, align = "center",
          column(12, style = "color: #E05207; font-size: 16px;", textOutput("fire_map_title")),
          fluidRow(
            column(3, uiOutput("fire_map_left")),
            column(6, leafletOutput("fire_map_plot", height = "48vh", width = "100%")),
            column(3, plotlyOutput("fire_map_right"))
          )
        )
      )
    ),
    bs4TabPanel(
      tabName = "Annual Mean Rainfall",
      fluidRow(
        column(
          12,
          plotlyOutput("rainfall_stats", height = "70vh")
        )
      )
    ),
    bs4TabPanel(
      tabName = "Annual Temp Anomaly",
      fluidRow(
        column(
          12,
          plotlyOutput("temperature_stats", height = "70vh")
        )
      )
    ),
    bs4TabPanel(
      tabName = "Weather plots",
      bs4TabCard(
        title = "",
        elevation = 2,
        id = "weather_tabs",
        width = 12,
        collapsible = FALSE, 
        closable = FALSE,
        bs4TabPanel(
          tabName = "Temperature",
          fluidRow(
            column(
              12,
              plotlyOutput("min_temp_plot", height = "35vh")
            ),
            column(12, hr()),
            column(
              12,
              plotlyOutput("max_temp_plot", height = "35vh")
            )
          )
        ),
        bs4TabPanel(
          tabName = "Rainfall",
          fluidRow(
            column(
              12,
              plotlyOutput("rainfall_plot", height = "70vh")
            )
          )
        ),
        bs4TabPanel(
          tabName = "Sunshine",
          fluidRow(
            column(
              12,
              plotlyOutput("sunshine_plot", height = "35vh")
            ),
            column(
              12,
              plotlyOutput("evaporation_plot", height = "35vh")
            )
          )
        ),
        bs4TabPanel(
          tabName = "Wind",
          fluidRow(
            column(
              12,
              plotlyOutput("max_wind_plot", height = "70vh")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$min_temp_plot <- renderPlotly({
    plot_ly(
      master_weather_data,
      x = ~date,
      y = ~minimum_temperature_c,
      marker = list(color = "#2ba9ed"),
      type = "bar"
    ) %>%
      layout(
        title = list(text = "Minimum Temperature recorded that day", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Minimum temperature (Centigrade)")
      )
  })
  output$max_temp_plot <- renderPlotly({
    plot_ly(
      master_weather_data,
      x = ~date,
      y = ~maximum_temperature_c,
      marker = list(color = "#ed892b"),
      type = "bar"
    ) %>%
      layout(
        title = list(text = "Maximum Temperature recorded that day", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Maximum temperature (Centigrade)")
      )
  })
  output$rainfall_plot <- renderPlotly({
    plot_ly(
      master_weather_data,
      x = ~date,
      y = ~rainfall_mm,
      marker = list(color = "#2ed9e8"),
      type = "bar"
    ) %>%
      layout(
        title = list(text = "Rainfall vs Date", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Rainfall (mm)")
      )
  })
  output$sunshine_plot <- renderPlotly({
    plot_ly(
      master_weather_data,
      x = ~date,
      y = ~sunshine_hours,
      marker = list(color = "#e8b02e"),
      type = "bar"
    ) %>%
      layout(
        title = list(text = "Hours of sunshine on a day", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Sunshine (hr)")
      )
  })
  output$evaporation_plot <- renderPlotly({
    plot_ly(
      master_weather_data,
      x = ~date,
      y = ~evaporation_mm,
      marker = list(color = "#2ec6e8"),
      type = "bar"
    ) %>%
      layout(
        title = list(text = "Evaporation recorded that day", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Evaporation (mm)")
      )
  })
  output$max_wind_plot <- renderPlotly({
    plot_ly(
      master_weather_data,
      x = ~date,
      y = ~speed_of_maximum_wind_gust_km_h,
      marker = list(color = "#475ad6"),
      type = "bar"
    ) %>%
      layout(
        title = list(text = "Maximum Wind speed recorded that day", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Wind speed (km/hr)")
      )
  })
  plot_data <- new_fire_map %>%
      filter(year == 2020) %>% filter(month == 1) %>%
      filter(day == "13")
  output$fire_map_title <- renderText("Fire Detection Spots on 13/1/2020")
  output$fire_map_plot <- renderLeaflet({
    leaflet(plot_data) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~longitude, lat = ~latitude, intensity = ~brightness,
        blur = 7, max = 0.5, radius = 7
      )
  })
  output$number_of_fires <- renderPlotly({
    plot_ly(
      total_bushfire_per_day,
      x = ~date,
      y = ~count,
      color = ~count,
      colors = 'Oranges',
      type = "bar"
    ) %>%
    layout(
      title = list(text = "Number of Fires", font = list(color = "#E05207")),
      xaxis = list(title = "Date", range = c(min(total_bushfire_per_day$date), max(total_bushfire_per_day$date))),
      yaxis = list(title = "Bushfire count"),
      yaxis = list(title = "Bushfire count"),
      legend = list(y = 0.5, yanchor = "center")
    )
  })
  observe({
    click_event_data <- event_data("plotly_click")
    if (is.null(click_event_data)) {
      return()
    }
    tryCatch({
      current_year <- as.numeric(format(as.Date(click_event_data$x), "%Y"))
      current_month <- as.numeric(format(as.Date(click_event_data$x), "%m"))
      current_day <- format(as.Date(click_event_data$x), "%d")
      plot_data <- new_fire_map %>%
        filter(year == current_year) %>% filter(month == current_month) %>%
        filter(day == current_day)
      info_data <- master_weather_data %>%
        filter(year == current_year) %>% filter(month == current_month) %>%
        filter(day == current_day)
      glimpse(info_data)
      wind_direction <- wind_directions %>% filter(symbols == info_data$direction_of_maximum_wind_gust) %>% pull(full_forms)
      output$fire_map_left <- renderUI({
        fluidRow(
          column(12, br()),
          column(12, br()),
          column(
            6,
            bs4InfoBox(
              width = 12,
              title = "Min temperature",
              gradientColor = "info",
              value = info_data$minimum_temperature_c,
              icon = "thermometer"
             )
          ),
          column(
            6,
            bs4InfoBox(
              width = 12,
              title = "Max temperature",
              gradientColor = "orange",
              value = info_data$maximum_temperature_c,
              icon = "thermometer"
             )
          ),
          column(
            6,
            bs4InfoBox(
              width = 12,
              title = "Rainfall",
              gradientColor = "primary",
              value = info_data$rainfall_mm,
              icon = "tint"
             )
          ),
          column(
            6,
            bs4InfoBox(
              width = 12,
              title = "Sunshine hours",
              gradientColor = "warning",
              value = info_data$sunshine_hours,
              icon = "sun"
             )
          ),
          column(
            6,
            bs4InfoBox(
              width = 12,
              title = "Max wind speed",
              gradientColor = "purple",
              value = info_data$speed_of_maximum_wind_gust_km_h,
              icon = "cloud"
             )
          ),
          column(
            6,
            bs4InfoBox(
              width = 12,
              title = "Wind direction",
              gradientColor = "purple",
              value = wind_direction,
              icon = "arrows-alt"
             )
          ),
          column(
            12,
            bs4InfoBox(
              width = 7,
              title = "Evaporation",
              gradientColor = "info",
              value = info_data$evaporation_mm,
              icon = "arrow-up"
             )
          )
        )
      })
      output$fire_map_right <- renderPlotly({
        plot_data <- structure(
          list(
            topic = structure(
              c(1:6),
              .Label = c("NSW<br>12.1M acres", "QLD<br>6.18M acres", "WA<br>4.2M acres", "VIC<br>2.97M acres", "SA<br>677k acres", "TAS<br>79.1k acres"),
              class = "factor"
            ),
            n = structure(
              c(1:6),
              .Label = c("12100000", "6180000", "4200000", "2970000", "677000", "79100"),
              class = "factor"
            )
          ),
          class = "data.frame",
          row.names = c(NA,-6L)
        )
        plot_ly(
          plot_data,
          labels = ~ topic,
          parents = NA,
          values = ~ n,
          type = "treemap",
          marker = list(colors = c("#7E2705", "#A83703", "#DE4F02", "#F37D2E", "#F5A05A", "#F8C48F"))
        )
      })
      output$fire_map_plot <- renderLeaflet({
        leaflet(plot_data) %>%
          addTiles() %>%
          addHeatmap(
            lng = ~longitude, lat = ~latitude, intensity = ~brightness,
            blur = 7, max = 0.5, radius = 7
          )
      })
      new_title_with_date <- paste0("Fire Detection Spots on ", current_day, "/", current_month, "/", current_year)
      output$fire_map_title <- renderText(new_title_with_date)
    }, error = function(err) {
      return()
    })
  })
  output$rainfall_stats <- renderPlotly({
    plot_ly(
      rainfall_over_decade,
      x = ~year,
      y = ~avg_rainfall,
      marker = list(color = "#77cf65"),
      type = "bar",
      name = "avg rainfall (mm)"
    ) %>%
      add_trace(y = ~diff_rainfall, type = "scatter",
                name = 'diff in rainfall (mm)', mode = 'lines',
                marker = list(color = "orange")) %>%
      layout(
        title = list(text = "Australian annual mean rainfall", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Rainfall in mm"),
        legend = list(y = 0.5, yanchor = "center")
      )
  })
  output$temperature_stats <- renderPlotly({
    plot_ly(
      temp_over_decade,
      x = ~year,
      y = ~diff_temp,
      color = ~diff_color,
      colors = c("#e33522", "#328be3"),
      showlegend = FALSE,
      type = "bar",
      name = "diff temperature (Celsius)"
    ) %>%
      layout(
        title = list(text = "Australian mean temperature anomaly", font = list(color = "#4f4f4f")),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Temperature change in Celsius"),
        legend = list(y = 0.5, yanchor = "center")
      )
  })
  info_data <- master_weather_data %>%
    filter(year == 2020) %>% filter(month == 1) %>%
    filter(day == "13")
  wind_direction <- wind_directions %>% filter(symbols == info_data$direction_of_maximum_wind_gust) %>% pull(full_forms)
  output$fire_map_left <- renderUI({
    fluidRow(
      column(12, br()),
      column(12, br()),
      column(
        6,
        bs4InfoBox(
          width = 12,
          title = "Min temperature",
          gradientColor = "info",
          value = info_data$minimum_temperature_c,
          icon = "thermometer"
         )
      ),
      column(
        6,
        bs4InfoBox(
          width = 12,
          title = "Max temperature",
          gradientColor = "orange",
          value = info_data$maximum_temperature_c,
          icon = "thermometer"
         )
      ),
      column(
        6,
        bs4InfoBox(
          width = 12,
          title = "Rainfall",
          gradientColor = "primary",
          value = info_data$rainfall_mm,
          icon = "tint"
         )
      ),
      column(
        6,
        bs4InfoBox(
          width = 12,
          title = "Sunshine hours",
          gradientColor = "warning",
          value = info_data$sunshine_hours,
          icon = "sun"
         )
      ),
      column(
        6,
        bs4InfoBox(
          width = 12,
          title = "Max wind speed",
          gradientColor = "purple",
          value = info_data$speed_of_maximum_wind_gust_km_h,
          icon = "cloud"
         )
      ),
      column(
        6,
        bs4InfoBox(
          width = 12,
          title = "Wind direction",
          gradientColor = "purple",
          value = wind_direction,
          icon = "arrows-alt"
         )
      ),
      column(
        12,
        bs4InfoBox(
          width = 7,
          title = "Evaporation",
          gradientColor = "info",
          value = info_data$evaporation_mm,
          icon = "arrow-up"
         )
      )
    )
  })
  output$fire_map_right <- renderPlotly({
    plot_data <- structure(
      list(
        topic = structure(
          c(1:6),
          .Label = c("NSW<br>12.1M acres", "QLD<br>6.18M acres", "WA<br>4.2M acres", "VIC<br>2.97M acres", "SA<br>677k acres", "TAS<br>79.1k acres"),
          class = "factor"
        ),
        n = structure(
          c(1:6),
          .Label = c("12100000", "6180000", "4200000", "2970000", "677000", "79100"),
          class = "factor"
        )
      ),
      class = "data.frame",
      row.names = c(NA,-6L)
    )
    plot_ly(
      plot_data,
      labels = ~ topic,
      parents = NA,
      values = ~ n,
      type = "treemap",
      marker = list(colors = c("#7E2705", "#A83703", "#DE4F02", "#F37D2E", "#F5A05A", "#F8C48F"))
    )
  })
}

shinyApp(ui = ui, server = server)
