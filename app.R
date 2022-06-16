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
#All_MM<-read.csv("data/All_Mos.csv")
All_ind<-read.csv("data/Individual_Mos_count.csv")

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
             tabPanel("Start Here!", fluid = TRUE,
                      withTags(
                        column(12,
                        h2("Bzzzz"),
                        p("- what would Alaska summers be without mosquitoes? Even though we might not enjoy their presence when trying to enjoy the outdoors, mosquitoes play an important role especially in arctic ecosystems. Forming dense clouds during the growing season, they help distribute plant pollen, provide a food source for migratory nesting birds and can impact the migration paths of caribou.  
                          This learning tool provides you with some basic information on mosquito biology. The data displayed here were collected by the National Ecological Observatory Network (NEON) at five sites along a latitudinal gradient in Alaska [link or button for “Methods” tab]. You can explore data on the abundance and timing of several mosquito species common to the Arctic and analyze connected temperature and soil moisture information [link or button for “Data exploration” tab].  
                          After completing the learning activities, you will be able to"),
                        ul(
                          li("describe the mosquito life cycle including aquatic and terrestrial developmental stages"),
                          li("compare and contrast seasonal changes in mosquito biomass of local species with different life history strategies"),
                          li("explore data of abiotic variables to help explain seasonal changes in mosquito biomass"),
                          li("evaluate how a changing climate might affect Alaska mosquito populations in the future")
                        )
                      ))),
             tabPanel("About Mosquitoes", fluid = TRUE,
                      withTags(   
                        fluidRow(
                          column(12,
                                 p("With their narrow wings and long thin legs, mosquitoes belong to the group of two-winged flies (",
                                 a(href="https://observer.globe.gov/documents/19589576/7b0bfffa-e66d-4587-98fb-fe24602e0d18", "GLOBE Observer",.noWS = "outside"),". There are more than 3,500 species of mosquitoes that vary in their food and habitat preferences, behavior patterns and host species they visit. All mosquito species require aquatic habitats with preferably stagnant water. The life cycle diagram shows that mosquitoes go through four life stages: egg, larva, pupa and winged adult. Once the female mosquito has secured enough protein via a blood meal, she deposits eggs on the water surface. The eggs hatch into larvae that feed and grow larger. The non-feeding pupa stage then develops into adults. Depending on the species, mosquitoes can overwinter as eggs, larvae or adults. You might have observed “fat, drunk” mosquitoes at the beginning of summer, which overwinter as adults and are the first to be active during the growing season. Other mosquito species overwinter as eggs and need to undergo several life-cycle stages before emerging as adults later in the season. If you would like to learn how to identify the larvae and adults, please take a look at this ",a(href="https://observer.globe.gov/documents/19589576/e4afb7bd-6f16-4825-90b6-5953cc9cec8d", "GLOBE mosquito identification (pdf).", .noWS = "outside"))),
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
                                              li(a(href="https://www.neonscience.org/field-sites/barr","BARR - Utkiagvik (formerly Barrow)")),
                                              li(a(href="https://www.neonscience.org/field-sites/took", "TOOK - Toolik Field Station")),
                                              li(a(href="https://www.neonscience.org/field-sites/bona","BONA - Bonanza Creek (at Caribou-Poker Creeks)")),
                                              li(a(href="https://www.neonscience.org/field-sites/deju","DEJU - Delta Juntion")),
                                              li(a(href="https://www.neonscience.org/field-sites/heal","HEAL - Healy")))))),
                                 column(10,
                                        class = "basic",
                                        a(class="ui blue ribbon label", "NEON Mosquito Plots"),
                                        leafletOutput("map")
                                 )),
                        tabPanel("Lab Methods", fluid = TRUE,
                                 titlePanel("Lab Methods"),
                                 withTags(
                                   p("Following collection, mosquito samples are sent to an external facility where they are sorted to remove bycatch and taxonomically identified (to species and sex, whenever possible). In the case of large field samples, a subsample of up to 200 individual mosquitoes is taxonomically identified but both total weights of the field collected sample and the subsample are provided to inform estimates of total abundance.")
                                 ))
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
                          withTags(
                            p("Each point on the graph represents how many mosquitoes were collected on a given day. On the left side of the page you can select data based on location and by species. Do you see any patterns?")
                          ),
                          withSpinner(plotOutput(outputId = "test")),
                          tags$sub(tags$em("NEON (National Ecological Observatory Network). Mosquitoes sampled from CO2 traps, RELEASE-2022 (DP1.10043.001)"),
                                   tags$p("Species identification occured on a subset of mosquitoes, based on the weight of the subset we extrapolated the number of individuals per species for this figure.")
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
  
  

  NEON_mosquito <- reactive({
    req(input$siteIDfinder)
    req(input$genusfinder)
    req(input$range)
    filter(All_ind) %>% 
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
               y = totalSpeciesCount, 
               colour=genus,
               shape = siteID)) +
      geom_point()+
      labs(y = "Mosquito Count ", x = "Day of Year") +
      lims(x=c(100,300))+
      theme_bw()
  })
  
}


shinyApp(ui = ui, server = server)


