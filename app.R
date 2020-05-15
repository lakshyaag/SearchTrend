library(tidyverse)
library(gtrendsR)
library(purrr)
library(httr)
library(jsonlite)
library(htmltools)

library(leaflet)
library(sp)

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(waiter)

load('india_state_shape_simplfied.Rdata')
og_data <- india_shape@data

## Shiny app starts here

# UI code

header <- dashboardHeader(title = 'Search Trends in India', titleWidth = 300)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Interest by state', tabName = 'state', icon = icon('map'))
    ),
    disable = TRUE
)

body <- dashboardBody(
    use_waiter(), 
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    tabItem(tabName = 'state',
            fluidRow(
                box(width = 6,
                    title = "Search term to view trends by state",
                    status = 'primary',
                    solidHeader = T,
                    dateInput(inputId = 'date_trend', label = 'Select date', format = 'dd-M-yyyy'),
                    selectInput(inputId = 'search_term', label = 'Select search term (top 20 for selected date)', 
                                choices = NULL, multiple = F, selectize = T),
                    htmlOutput('how_to_interpret'),
                    htmlOutput('developer')
                ),
                
                uiOutput('map_box')
            )
    ),
    
    waiter_show_on_load(html = spin_ring(), color = '#5C7EA8')
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = 'purple')

# Server code

server <- function(input, output, session) {
    
    waiter_hide()
    
    search_trend_data <- reactive({
        date <- str_replace_all(as.character(input$date_trend), "-", "")

        url <- paste0("https://trends.google.com/trends/api/dailytrends?hl=en-US&tz=-330&ed=", date, "&geo=IN&ns=15")

        raw_search_trend_df <- GET(url) %>%
            content('text') %>%
            str_sub(start = 7) %>%
            fromJSON() %>%
            pluck('default', 'trendingSearchesDays','trendingSearches', 1) %>%
            pull(title) %>%
            select(-exploreLink)
    })
    
    observe({
        updateSelectInput(session, inputId = 'search_term', choices = search_trend_data()$query)
    })
    
    term_map_data <- reactive({
        search_trend_data() %>%
            filter(query == input$search_term) %>%
            mutate(region_interest = list(gtrends(keyword = query, geo = 'IN', time = 'now 7-d')$interest_by_region)) %>%
            pull(region_interest) %>%
            pluck(1) %>%
            select(1:3) %>%
            replace_na(list(hits = 0)) %>%
            mutate(location = case_when(location == "Delhi" ~ "NCT of Delhi",
                                        location == "Andaman and Nicobar Islands" ~ "Andaman and Nicobar",
                                        TRUE ~ location)) %>%
            mutate(popup = str_c(location, ': ', hits))
    })
    
    colorPal <- reactive({
        colorBin(palette = 'Greens', domain = term_map_data()$hits, bins = 4)
    })
    
    output$map_box <- renderUI({
        box(width = 6,
            id = 'map-container',
            status = 'success',
            solidHeader = T,
            title = paste0('Trends by state for term: ', input$search_term, ' on ', 
                           format(input$date_trend, format = "%d %b %Y")),
            leafletOutput('state_map') %>% withSpinner(type = 8)
        )
    })
    
    output$state_map <- renderLeaflet({
        
        india_shape@data <- og_data %>% 
            left_join(term_map_data(), by = c("NAME_1" = "location"))
        
        map <- leaflet(india_shape, height = '100%') %>%
            addProviderTiles('CartoDB.PositronNoLabels') %>%
            addPolygons(weight = 0.7, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.7, color = 'black',
                        fillColor = ~colorPal()(hits),
                        highlightOptions = highlightOptions(color = "white", weight = 1.5, bringToFront = TRUE),
                        label = ~popup,
                        labelOptions = labelOptions(style = list('font-size' = '15px', 'font-style' = 'bold')),
                        group = 'poly') %>%
            addLegend(pal = colorPal(),
                      group = 'poly',
                      values = ~hits,
                      title = "Popularity",
                      position = 'bottomright') %>%
            addEasyButton(easyButton(icon="fa-globe", title="Center on India",
                                     onClick=JS("function(btn, map){ map.setView([21.59, 78.96], 4); }")))
        
        
        map
    })
    
    output$how_to_interpret <- renderUI({
        HTML(
            paste0('<hr><h4><b>How to interpret the map?</b></h4>',
                   "Values are calculated on a scale from 0 to 100, where 100 is the location with the most popularity as a fraction of total searches in that location, ",
                   "a value of 50 indicates a location which is half as popular. A value of 0 indicates a location where there was not enough data for this term.",
                   '<br>',
                   '<h4>Data from ',
                   '<a href = "https://trends.google.com/trends/trendingsearches/daily?geo=IN" target = "_blank">Google Trends</a>',
                   ' for past 7 days from ',
                   '<em>', format(Sys.Date(), format = "%B %d, %Y"), '.</em></h4>',
                   'Code available on <a href = "https://github.com/lakshyaag/searchtrend" target = "_blank"> GitHub</a>'
            )
        )
    })
    
    output$developer <- renderUI({
        HTML(
            paste0('<hr>',
                   '<h4><b>Made by <a href = "https://github.com/lakshyaag/">Lakshya Agarwal</a></b></h4>'
            )
        )
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
