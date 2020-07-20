library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(readr)
library(DT)
library(sf)
library(geojsonsf)
library(leaflet)
library(mapview)
source("jpndistrict2.R")

# サイト設定
site_name <- "Jichi Poly"
skin_type <- "purple"

# スタイル設定
options(spinner.color="#605CA8", spinner.color.background="#ffffff", spinner.size=2)
spins <- c("circle","bounce","folding-cube","rotating-plane","cube-grid","fading-circle","double-bounce","dots","cube")

# 地図の設定


# HTMLヘッダ
htmlHeader <- tags$head(
  tags$link(rel="stylesheet", type="text/css", href="style.css"),
  tags$script(src="drag.js")
)

# UIヘッダ
header <- dashboardHeader(title=HTML(site_name), disable=FALSE, titleWidth=250)

# UIサイドバー
sidebar <- dashboardSidebar(
  width=250,
  sidebarMenu(
    id="menu",
    menuItem(
      "１．ファイルをアップロード",
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
      "２．JISCODE列を選択",
      tabName="code"
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'code'",
      htmlOutput("colname"),
      htmlOutput("submit")
    ),
    menuItem(
      "３．地図で確認",
      tabName="map"
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'map'",
      downloadButton("downloadJSON", "GeoJSONをDownload")
    )
  )
)

# UIボディ
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
  
  # 初期化
  shinyjs::disable("downloadJSON")
  stData <- NULL
  
  # ファイルをアップロードしたら
  observeEvent(input$file, {
    
    # データ読込
    data <- reactive(read.csv(input$file$datapath))
    output$table <- renderDataTable(data())
    output$colname <- renderUI(selectInput("jiscode", "JISCODE", colnames(data())))
    output$submit <- renderUI(actionButton("submit", "プロット"))
    
    # タブを切り替え
    updateTabItems(session, "menu", "code")
  })
  
  # JISCODE列を選択したら
  observeEvent(input$submit, {
    
    # JISCODE列をcharacter型にcast
    data <- read.csv(input$file$datapath)
    data[,input$jiscode] <- data[,input$jiscode] %>% as.character()
    jis_code <- data[,input$jiscode]
    
    if (grep("^([0-3][1-9]|4[1-7])[0-9]{3}$", jis_code) %>% length() == length(jis_code)) {
      # geometry付与
      stData <<- jpn_cities2(data[,input$jiscode]) %>%
        dplyr::left_join(., data, by=c("city_code"=input$jiscode))
      
      # 地図にプロット
      output$mapPlot <- renderLeaflet({
        leaflet() %>%
          addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png", group="背景地図",
                   attribution='<a href="https://maps.gsi.go.jp/development/ichiran.html" target="_blank">地理院タイル</a>') %>%
          addMapPane("overlay", zIndex=600) %>%
          addPolygons(data=stData, option=pathOptions(pane="overlay"), stroke=T, color="#6666FF", weight=5,
                      fillOpacity=0.2, popup=leafpop::popupTable(st_drop_geometry(stData))) %>%
          addScaleBar(position="bottomleft")
      })
      
      # タブを切り替え
      updateTabItems(session, "menu", "map")
      
    } else {
      # 選択列がJISCODE（※01～47＋3桁）でなければアラート
      show_alert(
        title = "Error !!",
        text = "選択したコードが正しくありません。",
        type = "error"
      )
    }
  })
  
  # mapタブに切り替わったら
  observeEvent(input$menu, {
    
    req(stData)
    if (!is.null(stData)) {
      
      # GeoJSONを作成
      json <- sf_geojson(stData)
      output$downloadJSON = downloadHandler(
        filename = function() {sprintf("%s.geojson", input$file$name %>% gsub("\\..+$", "", .))},
        content = function(f) {write(json, f)}
      )
      
      # ダウンロードボタンを有効化
      shinyjs::enable("downloadJSON")
    }
  })
}

shinyApp(ui=ui, server=server)
