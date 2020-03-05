library(shiny)
library(leaflet)
library(rnaturalearth)
library(tidyverse)
library(rvest)

ui <- bootstrapPage(

    absolutePanel(
      top = 10, right = 10, width = 500, height = "auto",fixed = TRUE, style = "z-index:500; background: #FFFFFF;padding: 8px;border: 1px solid #CCC;",
      tags$h2("SARS-CoV-2: Fallzahlen in Deutschland"),
      tags$p("Hier sind ausschließlich Fälle aufgelistet, die dem RKI über den Meldeweg oder offizielle Quellen mitgeteilt wurden.
         Da es sich um eine sehr dynamische Situation handelt, kann es zu Abweichungen zwischen der RKI-Tabelle und Angaben anderer Stellen,
         etwa der betroffenen Bundesländer, kommen."),
      h3(textOutput("gesamt")),
      tags$hr(style="border-color: black;"),
      tags$p("Quelle: Robert Koch Institut"),
      p(textOutput("dateText")),
      tags$a("Autor: Stefan Reifenberg", href="https://twitter.com/Reyfenberg")
    ),
    leafletOutput("mymap", width = "100%", height = 1000)
)

server <- function(input, output, session) {
  
  autoInvalidate <- reactiveTimer(300000)
  
  observe({
    autoInvalidate()
  })
  
  output$dateText <- renderText({
    autoInvalidate()
    paste("Stand:", Sys.time())
  })
  
  data <- reactivePoll(
    intervalMillis = 300000,
    session,
    checkFunc = function(){
      Sys.time()
    },
    valueFunc = function(){
      url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
      corona <- url %>%
        xml2::read_html() %>%
        html_nodes(xpath='//*[@id="main"]/div[1]/table[1]') %>%
        html_table()
      corona <- corona[[1]]
    }
  )
  
  output$gesamt <- renderText({
    corona_data <- data()
    gesamt <- corona_data %>% 
      filter(Bundesland == "Gesamt")
    paste("Gesamtfälle:", gesamt$Fälle)
  })
 
  output$mymap <- renderLeaflet({
    
    corona_data <- data() 
    
    corona_ger <- corona_data %>% 
      slice(1:(n()-1)) %>% 
      rename(name = "Bundesland",
             Faelle = "Fälle") %>%
      mutate(name = case_when(
        name == "Schleswig Holstein" ~ "Schleswig-Holstein",
        TRUE ~ name)) 
    
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("viridis", domain = corona_ger$Faelle, bins = bins)
    
    ger_states <- ne_states(country = 'germany', returnclass = "sf")
    corona_ger_sf <- left_join(ger_states, corona_ger, by = "name")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Fälle: %g",
      corona_ger_sf$name, corona_ger_sf$Faelle
    ) %>% lapply(htmltools::HTML)
    
    leaflet(corona_ger_sf) %>%
      setView(11, 50, zoom = 6) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% 
      addPolygons(weight = 2,
                  fillColor = ~pal(Faelle),
                  fillOpacity = 0.7,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  })
}

shinyApp(ui, server)