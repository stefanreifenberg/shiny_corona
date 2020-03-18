library(shiny)
library(leaflet)
library(tidyverse)
library(rvest)
library(raster)
library(sf)
library(rgeos)

# population https://en.wikipedia.org/wiki/List_of_German_states_by_population
pop <- tibble(
          name = c("Baden-Württemberg","Bayern","Berlin",
                 "Brandenburg","Bremen","Hamburg","Hessen","Niedersachsen",
                 "Mecklenburg-Vorpommern","Nordrhein-Westfalen",
                 "Rheinland-Pfalz","Saarland","Sachsen","Sachsen-Anhalt",
                 "Schleswig-Holstein","Thüringen"),
          Pop = c(11069533,13076721,3644826,2511917,
                 682986,1841179,6265809,7982448,1609675,17932651,4084844,
                 990509,4077937,2208321,2896712,2143145)
)

# district map
dist_data <- read_csv("https://raw.githubusercontent.com/iceweasel1/COVID-19-Germany/master/germany_with_source.csv")
dist_data %>% 
  drop_na() %>% 
  str_remove_all("(district)")

dist <- str_remove_all(dist_data$District, "(district)")
dist <- gsub("[()]", "", dist)
dist <- str_trim(dist)

dist_data$District <- dist

# remove unknown District in Bavaria
dist_data <- dist_data %>% 
  filter(District != "N/A")


cor_district <- dist_data %>% 
  na.omit() %>% 
  group_by(District,Latitude,Longitude) %>% 
  summarise(cases=n())


ui <- bootstrapPage(

    absolutePanel(
      id = "controls", class = "panel panel-default",
      top = 120, left = 10, width = 300, height = "auto",fixed = TRUE, style = "z-index:500; background: #FFFFFF;padding: 8px;border: 1px solid #CCC;",
      HTML('<button data-toggle="collapse" data-target="#panel">Informationen</button>'),
      tags$div(id = 'panel',  class="collapse",
        tags$h2("SARS-CoV-2: Fallzahlen in Deutschland"),
        tags$p("Hier sind ausschließlich Fälle aufgelistet, die dem RKI über den Meldeweg oder offizielle Quellen mitgeteilt wurden.
         Da es sich um eine sehr dynamische Situation handelt, kann es zu Abweichungen zwischen der RKI-Tabelle und Angaben anderer Stellen,
         etwa der betroffenen Bundesländer, kommen."),
        h3(textOutput("gesamt")),
        tags$hr(style="border-color: black;"),
        tags$p("Quelle: Robert Koch Institut"),
        p(textOutput("dateText")),
        tags$p("Autor:"),
        tags$a("Stefan Reifenberg", href="https://twitter.com/Reyfenberg"))
    ),
    leafletOutput("mymap", width = "100%", height = 900)
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
      names(corona) <- NULL
      corona <- corona[-c(1), ]
      names(corona) = c("name", "Faelle", "Diff", "Faelle_rel","dead","krisen")
      corona <- as.tibble(corona)
    }
  )
  
  output$gesamt <- renderText({
    
    corona_data <- data() 
    gesamt <- corona_data %>%
      dplyr::select(name,Faelle,dead) %>% 
      filter(name == "Gesamt")
    paste("Gesamtfälle:", gesamt$Faelle, "Todesfälle:", gesamt$dead)
  })
 
  output$mymap <- renderLeaflet({
    #browser()
    corona_data <- data() 
    
    corona_ger <- corona_data %>% 
      slice(1:(n()-1)) %>%
      dplyr::select(name,Faelle)
    corona_ger$Faelle <- str_replace(corona_ger$Faelle, "\\.", "")
    corona_ger$Faelle <- as.numeric(corona_ger$Faelle)
    
    
    # join dataframes
    corona_ger <- inner_join(corona_ger, pop, by="name")
    corona_ger <- corona_ger %>% 
      mutate(per_k = (Faelle/Pop)*10000)
    
    pal_total <- colorNumeric(palette="viridis", domain = corona_ger$Faelle, na.color="transparent")
    
    pal_rel <- colorNumeric(palette="viridis", domain = corona_ger$per_k, na.color="transparent")
    
    DEU1 <- raster::getData("GADM", country="DEU", level=1)
    deu_states <- st_as_sf(DEU1)
    
    deu_states <- deu_states %>% 
      rename(name = "NAME_1")
    corona_ger_sf <- left_join(deu_states, corona_ger, by = "name")
    
    labels_total <- sprintf(
      "<strong>%s</strong><br/>Fälle (total): %g",
      corona_ger_sf$name, corona_ger_sf$Faelle
    ) %>% lapply(htmltools::HTML)
    
    labels_points <- sprintf(
      "<strong>%s</strong><br/>Fälle: %g",
      cor_district$District, cor_district$cases
    ) %>% lapply(htmltools::HTML)
    
    labels_rel <- sprintf(
      "<strong>%s</strong><br/>Fälle (pro 10.000): %s",
      corona_ger_sf$name, format(round(corona_ger_sf$per_k, 2), nsmall = 2)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(corona_ger_sf, cor_district, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(11, 50, zoom = 6) %>% 
            addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE) 
      ) %>% 
      # addCircleMarkers(
      #   group = "Einzelfälle",
      #   lng = cor_district$Longitude,
      #   lat = cor_district$Latitude,
      #   radius = sqrt(cor_district$cases),
      #   color = "#A04173",
      #   stroke = TRUE, fillOpacity = 0.9,
      #   label = labels_points,
      #   labelOptions = labelOptions(
      #     style = list("font-weight" = "normal", padding = "3px 8px"),
      #     textsize = "15px",
      #     direction = "auto")
      # ) %>% 
      addPolygons(group = "Fälle total",
                  weight = 2,
                  fillColor = ~pal_total(Faelle),
                  fillOpacity = 0.6,
                  label = labels_total,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addPolygons(group = "Fälle pro 10.000 Einwohner",
                  weight = 2,
                  fillColor = ~pal_rel(per_k),
                  fillOpacity = 0.6,
                  label = labels_rel,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLayersControl(
        position = "topleft",
        overlayGroups = c("Fälle total", "Fälle pro 10.000 Einwohner"), #, "Einzelfälle"),
        options = layersControlOptions(collapsed = FALSE) 
      ) %>% 
      hideGroup(c("Fälle pro 10.000 Einwohner")) #, "Einzelfälle"))
  })
}

shinyApp(ui, server)