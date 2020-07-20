targetPackages <- c('shiny', 'shinyjs', 'shinyWidgets', 'shinydashboard',
                    'dplyr', 'readr', 'DT', 'sf', 'leaflet', 'geojsonsf') 
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, repos = "http://cran.us.r-project.org")
for(package in targetPackages) library(package, character.only = T)

# configuration
site_name <- "Jichi Poly"
skin_type <- "purple"


# HTML header
htmlHeader <- tags$head(
  tags$link(rel="stylesheet", type="text/css", href="style.css"),
  tags$script(src="drag.js")
)

# UI header
header <- dashboardHeader(title=HTML(site_name), disable=FALSE, titleWidth=250)

# UI sidebar
sidebar <- dashboardSidebar(
  width=250,
  sidebarMenu(
    id="menu",
    menuItem(
      "1. Upload CSV File",
      tabName="dataTable",
      startExpanded=T
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'dataTable'",
      fileInput(
        "file", "Choose CSV File",
        accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
    ),
    menuItem(
      "2. Select Column of LG Code",
      tabName="code"
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'code'",
      htmlOutput("colname"),
      htmlOutput("submit")
    ),
    menuItem(
      "3. Confirm Result on Map",
      tabName="map"
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'map'",
      downloadButton("downloadJSON", "Download GeoJSON")
    )
  )
)

# UI body
body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(
      "dataTable",
      bootstrapPage(
        tags$style(type="text/css", "#table {min-height: calc(100vh - 80px) !important;}"),
        dataTableOutput("table")
      )
    ),
    tabItem(
      "map",
      bootstrapPage(
        tags$style(type="text/css", "#mapPlot {height: calc(100vh - 80px) !important;}"),
        leafletOutput("mapPlot")
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin=skin_type, htmlHeader)


# Server
server <- function(input, output, session) {
  
  # Initialize
  shinyjs::disable("downloadJSON")
  stData <- NULL
  
  # File Upload Event
  observeEvent(input$file, {
    
    # File type validation
    if (input$file$name %>% gsub("^.+\\.", "", .) != "csv") {
      
      # Warn if the file type is not match
      show_alert(
        title = "Error !!",
        text = "The selected file is invalid.",
        type = "error"
      )
    } else {
      
      # Read dataset
      data <- reactive(read.csv(input$file$datapath))
      output$table <- renderDataTable(data())
      output$colname <- renderUI(selectInput("jichicode", "JICHICODE", colnames(data())))
      output$submit <- renderUI(actionButton("submit", "View on map"))
      
      # Go to next tab
      updateTabItems(session, "menu", "code")
    }
  })
  
  # Code Column Select Event
  observeEvent(input$submit, {
    
    # Cast the column of code to character type
    data <- read.csv(input$file$datapath)
    jichi_code <- sprintf("%05d", data[,input$jichicode])
    data[,input$jichicode] <- jichi_code
    
    if (grep("^([0-3][0-9]|4[0-7])[0-9]{3}$", jichi_code) %>% length() == length(jichi_code)) {
      # Add geometry
      stData <<- jpn_cities2(data[,input$jichicode]) %>%
        dplyr::left_join(., data, by=c("city_code"=input$jichicode))
      
      # Plot dataset on map
      output$mapPlot <- renderLeaflet({
        leaflet() %>%
          addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png", group="basemap",
                   attribution='<a href="https://maps.gsi.go.jp/development/ichiran.html" target="_blank">地理院タイル</a>') %>%
          addMapPane("overlay", zIndex=600) %>%
          addPolygons(data=stData, option=pathOptions(pane="overlay"), stroke=T, color="#6666FF", weight=5,
                      fillOpacity=0.2, popup=leafpop::popupTable(st_drop_geometry(stData))) %>%
          addScaleBar(position="bottomleft")
      })
      
      # Go to next tab
      updateTabItems(session, "menu", "map")
      
    } else {
      # Warn if values in the selected column is not a digit of LG code
      show_alert(
        title = "Error !!",
        text = "The selected code is invalid.",
        type = "error"
      )
    }
  })
  
  # Open Map Event
  observeEvent(input$menu, {
    
    req(stData)
    if (!is.null(stData)) {
      
      # Make GeoJSON File
      json <- sf_geojson(stData)
      output$downloadJSON = downloadHandler(
        filename = function() {sprintf("%s.geojson", input$file$name %>% gsub("\\..+$", "", .))},
        content = function(f) {write(json, f)}
      )
      
      # Enable the download button
      shinyjs::enable("downloadJSON")
    }
  })
}

shinyApp(ui=ui, server=server)
