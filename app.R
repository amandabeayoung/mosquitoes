library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
#library(shiny.semantic)
library(leaflet)
library(ggrepel)
library(rsconnect)
library(shinyBS)


##csv files
#setwd("C:/Users/Amanda B. Young/Documents/R/mosquitoes")
NEON_Mos<-read.csv("data/NEON_mosquitoes.csv")
All_MM<-read.csv("data/All_Mos.csv")
### Make bug tags for map
awesome <- makeAwesomeIcon(
  icon = "bug",
  iconColor = "black",
  markerColor = "blue",
  library = "fa"
)

df <- data.frame(
  question = c("Tinglit", "Inupiaq", "Athabaskan"),
  answer = c("How mosquitoes came to be", 
             "question2 answer", 
             "answer3")
)

ui <- fluidPage(
  navbarPage(title = "Alaskan Mosquitoes", theme = shinytheme("superhero"),
             tabPanel("Learning Objectives", fluid = TRUE,
                      withTags(
                        ul(
                          li("Describe the mosquito life cycle including aquatic and terrestrial developmental stages."),
                          li("Compare and contrast seasonal changes in mosquito biomass of local species with different life history strategies."),
                          li("Explore and analyze data of abiotic variables to help explain seasonal changes in mosquito biomass."),
                          li("Evaluate how a changing climate might affect Alaska mosquito populations in the future.")
                        )
                      )),
             tabPanel("About Mosquitoes", fluid = TRUE,
                      withTags(   
                        fluidRow(
                          column(7,
                                 h4("Types of Mosquitoes in Alaska"),
                                 h5(p("There are 4 genera of mosquitoes in Alaska:")),
                                 ul(
                                   li(i("Aedes")),
                                   ul(li("most common")),
                                   li(em("Anopheles")),
                                   ul(li("rare")),
                                   li(em("Culex")),
                                   ul(li("rare")),
                                   li(em("Culiseta")),
                                   ul(li("second most common"))),
                          ),
                          column(5,
                                 p("Mosquito Life Cycle"),
                                 imageOutput("home_img")
                          )
                        )
                      )
             ),
             navbarMenu("Methods", icon = icon("chart-bar"),  
                        tabPanel("Field Methods",fluid = TRUE,
                                 titlePanel("Field Methods"),
                                 withTags(
                                   column(12,
                                          ul(
                                            li("Sampling"),
                                            p("Each site has ten CO2 samplers. Samplers are set out every two weeks for two days when temperatures are above 4C"),
                                            p("CO2 samplers are filled with dry ice and attached to a battery operated fan. The mosquitoes are attracted to the CO2 released from the dry ice. The mosquitoes are then trapped inside a cup with a mesh bottom and lid until collected."),
                                            p(""),
                                            li("Sites"),
                                            ul(
                                              li("Utkiagvik"),
                                              li("Toolik Field Station"),
                                              li("Bonanza Creek"),
                                              li("Delta Juntion"),
                                              li("Healy"))))),
                                 column(10,
                                        class = "basic",
                                        a(class="ui blue ribbon label", "NEON Mosquito Plots"),
                                        leafletOutput("map")
                                 )),
                        tabPanel("Lab Methods", fluid = TRUE,
                                 titlePanel("Lab Methods"))
             ),
             tabPanel("Data Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel(tags$h4("Select Variables")),
                          checkboxGroupInput(inputId = "siteIDfinder",
                                             label = "NEON Study Site:",
                                             choices = c("Utqiagvik" = "BARR",
                                                         "Toolik Field Station" = "TOOL",
                                                         "Healy" = "HEAL",
                                                         "Delta Junction" = "DEJU",
                                                         "Bonanza Creek" = "BONA"),
                                             selected = c("TOOL", "BARR", "HEAL", "DEJU", "BONA")),
                          checkboxGroupInput(inputId = "genusfinder",
                                             label = "Select genus:",
                                             choices = c("Aedes", "Anopheles", "Culex","Culiseta"),
                                             selected = "Aedes"),
                          sliderInput(inputId = "range",
                                      label = "Years",
                                      min = 2017, max = 2022,sep="",
                                      value = c(2019,2022))
                        ),
                        mainPanel(
                          withSpinner(plotOutput(outputId = "test")),
                          tags$sub(tags$em("NEON (National Ecological Observatory Network). Mosquitoes sampled from CO2 traps, RELEASE-2022 (DP1.10043.001)")                          
                          )
                        )
                      )
             ),
             tabPanel("Historical Records",
                      withTags(
                        column(12,
                               p("ARCOTS has 3,084 records of the top four most common species  from NEON sampling."),
                               p("additional side panel with options for selecting Arctos data and figures")
                        ))),
             tabPanel("Native Alaskan",
                      withTags(
                        column(12,
                               p("research some Native Alaskan perspectives of mosquitoes"),
                               p("Alaskan Native Languages:"),
                               ul(
                                 li("Inupiaq =",em("kikugiak")),
                                 li("Gwitchin =",em("K'ii")),
                                 li("Koyukon Athabaskan =",em("Tl'eeyh"), p("(special charachters missing)"))
                               ),
                               p("Mosquito Origin Stories"),
                               bsCollapse(id = "collapseExample", open = "Tlingit",
                                          bsCollapsePanel("Tlingit", 
                                                          tags$a(href="https://www.firstpeople.us/FP-Html-Legends/How_Mosquitoes_Came_To_Be-Tlingit.html", "How Mosquitoes Came to be"),
                                                          "Transcribe text from website here but leave link "),
                                          bsCollapsePanel("Inupiaq", "Story in In a Hungry Country by Simon Paneak")
                               ),                               br(),
                               p("Ways to keep the mosquitos away"))))
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    m <- leaflet(NEON_Mos) %>% 
      addTiles(group= "OSM (default") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group="ESRI Imagery") 
    
    m <- m %>% fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    m <- m %>% addAwesomeMarkers(~Longitude, ~Latitude, icon = ~awesome,
                                 popup = ~as.character(plotID),
                                 label = ~as.character(plotID))
    m <- m %>%  addLayersControl(
      baseGroups = c("ESRI Imagery","OSM (default)"),
      options = layersControlOptions(collapsed = FALSE))
    m
  })
  
  output$home_img <- renderImage({
    
    list(src = "www/mosquito_lifecycle2.png",
         width = "100%")
    
  }, deleteFile = F)
  
  
  
  output$akdatum_img <- renderImage({
    
    list(src = "www/akdatum.jpg",
         width = "10%")
    
  }, deleteFile = F)
  
  
  
  NEON_mosquito <- reactive({
    req(input$siteIDfinder)
    req(input$genusfinder)
    req(input$range)
    filter(All_MM, family == "Culicidae") %>% 
      filter(siteID %in% input$siteIDfinder) %>%
      filter(genus %in% input$genusfinder) %>%
      filter(Year >= input$range[1], Year <= input$range[2]) 
  })
  
  
  output$value <- renderPrint({ input$genusfinder })
  
  # output$table2<-renderTable({NEON_mosquito2()
  # })
  
  output$test <- renderPlot({
    ggplot(data = NEON_mosquito(),
           aes(x = doy, 
               y = individualCount, 
               colour=genus,
               shape = siteID)) +
      geom_point()+
      labs(y = "Mosquito Count (individual)", x = "Day of Year") +
      lims(x=c(100,300), y=c(0,250))+
      theme_bw()
  })
  
}

shinyApp(ui = ui, server = server)
