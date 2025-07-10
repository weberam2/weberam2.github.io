library(jsonlite)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggimage)
library(scales)
library(shinyWidgets)
library(plotly)
library(ggrepel)

# To get this to run on your website, you need to run this command from this folder:
# shinylive::export(appdir = ".", destdir = "../Website/weberam2.github.io/docs/")
# but note, it will overwrite your index.html file

lat <- 49.246292
long <- -123.116226
zoom <- 5
climbs <- read.csv("Climbs.csv")
climbs$Longitude <- as.numeric(climbs$Longitude)
climbs$Latitude <- as.numeric(climbs$Latitude)
climbs$AlexClimbed <- as.factor(climbs$AlexClimbed)
iconURL <- "FontAwesome-Mountain-Sun-icon.png"
countrychoices = unique(climbs$Country)
statechoices = unique(climbs$State)
alexchoices = unique(climbs$AlexClimbed)
classicfifty = unique(climbs$Classics)

climbs$Grade <- factor(
  climbs$Grade,
  levels = c(
    "3rd",
    "4th",
    "5th",
    "5.2",
    "5.3",
    "5.4",
    "5.5",
    "5.6",
    "5.7",
    "5.8",
    "5.9",
    "5.10a",
    "5.10b",
    "5.10c",
    "5.10d",
    "5.11a",
    "5.11b",
    "5.11c",
    "5.11d"
  ),
  ordered = TRUE
)
climbingchoices = unique(climbs$Grade)

iconsize = 16
leafletIcon <- makeIcon(iconUrl = iconURL,
                        iconWidth = iconsize,
                        iconHeight = iconsize)

# Define UI
ui <- dashboardPage(
  # Application title
  dashboardHeader(
    title = "Climbs Around Vancouver",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput(
        "location",
        "Choose Country:",
        choices = countrychoices,
        selected = countrychoices,
        multiple = TRUE
      ),
      selectInput(
        "state",
        "Choose Province/State:",
        choices = statechoices,
        selected = statechoices,
        multiple = TRUE
      ),
      sliderTextInput(
        inputId = "gradeRange",
        label = "Choose Grade Range:",
        choices = levels(climbs$Grade),
        selected = c("3rd", "5.11d"),
        dragRange = TRUE
      ),
      selectInput(
        "alexdone",
        "Alex has done:",
        choices = alexchoices,
        selected = alexchoices,
        multiple = TRUE
      ),
      selectInput(
        "classicfifty",
        "Top 50:",
        choices = classicfifty,
        selected = classicfifty,
        multiple = TRUE
      ),
      width = 2
    )
  ),
  
  dashboardBody(
    fluidRow(
      box(
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = NULL,
        HTML("Click a mountain on the map to display information and a weather forecast (it may take a minute)<br>You can filter based on Country, State, and Grade using the sidebar on the left")
      )
    ),
    fluidRow(
      column(width = 12, box(
        textOutput("click_info"), width = NULL
      ))
    ),
    fluidRow(
      column(width = 6, box(
        leafletOutput(outputId = "leafletMap" ), width = NULL
      )), column(width = 6, box(
        plotOutput("weather_info"), width = NULL
      ))
    ))
)

# Define server logic required
server <- function(input, output) {
  #data
  data <- reactive({
    climbs %>% filter(
      AlexClimbed %in% input$alexdone &
        Classics %in% input$classicfifty &
        Country %in% input$location &
        State %in% input$state &
        Grade >= input$gradeRange[1] & Grade <= input$gradeRange[2]
    )
  })
  
  # leaflet map
  output$leafletMap <- renderLeaflet({
    leaflet(data = data()) %>%
      setView(lat = lat,
              lng = long,
              zoom = zoom) %>%
      addTiles() %>%
      addMarkers(
        ~ Longitude,
        ~ Latitude,
        icon = leafletIcon,
        popup = ~ paste0(
          "<b><a href=",
          MPLink,
          ">",
          Name,
          "</a></b>",
          ", ",
          Mountain,
          "<br>",
          Grade,
          ", ",
          Length,
          " meters, ",
          AlpineGrade,
          "<br>"
        )
      ) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  
  # store the click
  observeEvent(input$leafletMap_marker_click, {
    click <- input$leafletMap_marker_click
    
    # Path to icons
    icon_paths <- c(
      "daily_precipitation" = "rain_drop.png",
      "daily_temperature" = "thermometer.png",
      "daily_windspeed" = "wind.png"
    )
    
    # Get the selected mountain name
    selected_mountain <- data() %>%
      filter(Latitude == click$lat & Longitude == click$lng) %>%
      pull(Mountain)
    
    # # Call openmeteo API
    query_url <- paste0(
      'https://api.open-meteo.com/v1/forecast?latitude=',
      click$lat,
      '&longitude=',
      click$lng,
      '&daily=temperature_2m_max,precipitation_sum,wind_speed_10m_max'
    )
    
    #Call openmeteo
    weatherdata <- as.data.frame(jsonlite::fromJSON(query_url)) |>
      select(daily.time, daily.temperature_2m_max, daily.precipitation_sum, daily.wind_speed_10m_max) |>
      rename(date = daily.time, daily_precipitation_sum = daily.precipitation_sum, daily_temperature_2m_max = daily.temperature_2m_max, daily_windspeed_10m_max = daily.wind_speed_10m_max)
    weatherdata_long <- weatherdata %>% pivot_longer(-date, names_to = "Weather", values_to = "Values")
    
    # Add image paths to the data
    weatherdata_long <- weatherdata_long %>%
      mutate(
        image = case_when(
          Weather == "daily_precipitation_sum" ~ icon_paths["daily_precipitation"],
          Weather == "daily_temperature_2m_max" ~ icon_paths["daily_temperature"],
          Weather == "daily_windspeed_10m_max" ~ icon_paths["daily_windspeed"]
        )
      ) %>%
      mutate(
        nudge_x = case_when(
          Weather == "daily_precipitation_sum" ~ 0.1,
          Weather == "daily_temperature_2m_max" ~ -0.1,
          Weather == "daily_windspeed_10m_max" ~ 0
        ),
        nudge_y = ifelse(
          Weather == "daily_windspeed_10m_max", 0.3, 0)
      )
#    # write.csv(weatherdata_long, "weatherdata_long.csv")
#    # weatherdata_long <- read.csv("weatherdata_long.csv")
    
    # Display weather info
    output$weather_info <- renderPlot({
      ggplot(weatherdata_long, aes(as.Date(date), Values, colour = Weather)) +
        geom_line() +
        geom_image(aes(image = image), size = 0.05) +
        geom_text(
          aes(label = round(Values, 1), colour = Weather),
          vjust = -1.5,
          nudge_x = weatherdata_long$nudge_x,
          nudge_y = weatherdata_long$nudge_y,
          size = 4,
          check_overlap = TRUE
        ) +
        scale_color_manual(
          values = c(
            "daily_precipitation_sum" = "blue",
            "daily_temperature_2m_max" = "red",
            "daily_windspeed_10m_max" = "dodgerblue"
          )
        ) +
        theme_minimal() +
        labs(
          title = paste("Weather Forecast for", selected_mountain),
          x = "Date",
          y = "Values (Celcius, Kmh, mm)"
        ) +
        theme(
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold"),
          # Adjust title size
          axis.title.x = element_text(size = 14),
          # Adjust x-axis label size
          axis.title.y = element_text(size = 14),
          # Adjust y-axis label size
          axis.text.x = element_text(size = 12),
          # Adjust x-axis text size
          axis.text.y = element_text(size = 12)
        ) +
        scale_y_continuous(breaks = seq(
          min(weatherdata_long$Values),
          max(weatherdata_long$Values)+3,
          by = 1
        )) +
        expand_limits(y = c(min(weatherdata_long$Values), max(weatherdata_long$Values) + 3))+
        scale_x_date(date_labels = "%A\n %b %d", date_breaks  = "1 day")
    })
    # output$click_info <-renderText({
    #   paste("You clicked on:", weatherdata[1])
    # })
  })
}

# Run the application
shinyApp(ui = ui, server = server)