#Global.R------------------
#Library Needed

#Data Preparation
library(dplyr)
library(tidyr)
library(openxlsx)
library(RColorBrewer)
#Shiny Dashboard
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
#Map
library(leaflet)
library(mapview)
#Graph
library(rCharts)
library(highcharter)

#Open File
sales <- read.xlsx("Dataset sales bedak bayi Jawa Timur.v3.xlsx")
dtable <- read.xlsx("Dataset wilayah Jawa Timur.xlsx")
lokasi <- read.xlsx("Lokasi.xlsx")
market <- read.xlsx("Market Share.xlsx")

#Coloring
color_pal <- colorNumeric(palette = c("red", "orange", "yellow3","green3"), domain = lokasi$Rataan_Sales_mio, reverse = F)


#server.R------------------
server <- function(input, output, session) {
  
#Plot Map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
                        data = lokasi,
                        lng = ~longitude, lat = ~latitude,
                        radius = ~Rataan_Populasi_Bayi/10000,
                        color = ~color_pal(Rataan_Sales_mio),
                        stroke = T, fillOpacity = 0.8,
                        label=~as.character(Wilayah), 
                        layerId=~as.character(Wilayah),
                        popup = popupTable(lokasi))
      
  })
  
#Plot Chart
  #Common Usage
  click_marker <- eventReactive(input$map_marker_click, {
    
    x <- input$map_marker_click
         return(x$id)
  })
  
  data_for_chart <- reactive({
    sales2 <- sales[sales$Wilayah == click_marker(),]
    return(sales2)
  })
  
  #Sales Chart
  output$salesnum <- renderHighchart({
    
    sales2 <- sales[sales$Wilayah == click_marker(),]
    hchart(sales2, "column", hcaes(x = tahun, y = sales_mio, group = Wilayah))
    })
    
  output$salesgrow <- renderHighchart({
    
    sales2 <- sales[sales$Wilayah == click_marker(),]
    hchart(sales2, "line", hcaes(x = tahun, y = sales_growth, group = Wilayah))
  })
  
  #Market Share Chart
  output$marketchart <- renderChart({
        market2 <- market[market$Wilayah == click_marker(),]
        marketout <- nPlot(
                          market_share ~ Tahun,
                          group = "Produk",
                          data = market2,
                          type = "multiBarChart",
                          dom = "marketchart",
                          width = 350
                          )
                          marketout$chart(margin = list(left = 50))
                          #marketout$yAxis(axisLabel = "% Market Share", width = 5)
                          
                          marketout$xAxis(axisLabel = "Tahun", width = 80)
                          marketout$chart(reduceXTicks = F)
                          marketout
  })
  
  
  #Populasi Bayi Chart
  output$bayinum <- renderHighchart({
    
    sales2 <- sales[sales$Wilayah == click_marker(),]
    hchart(sales2, "column", hcaes(x = tahun, y = populasi_bayi, group = Wilayah))
  })
  
  output$bayigrow <- renderHighchart({
    
    sales2 <- sales[sales$Wilayah == click_marker(),]
    hchart(sales2, "line", hcaes(x = tahun, y = bayi_growth, group = Wilayah))
  })
  
  
  #Pengeluaran per Kapita Chart
  output$kapitanum <- renderHighchart({
    
    sales2 <- sales[sales$Wilayah == click_marker(),]
    hchart(sales2, "column", hcaes(x = tahun, y = pengeluaran_kapita_mio, group = Wilayah))
  })
  
  output$kapitagrow <- renderHighchart({
    
    sales2 <- sales[sales$Wilayah == click_marker(),]
    hchart(sales2, "line", hcaes(x = tahun, y = pengeluaran_kapita_growth, group = Wilayah))
  })
    
  
  #Data Table Explorer
  output$data = DT::renderDataTable({
                   dtable})
  
  
#closing server parantheses
}


#ui.R
#------------------
ui <- fluidPage(
                useShinyjs(),
                navbarPage(title = "TEMPO SCAN Sales Analytics Dashboard - Bedak Bayi",
             
             # Pick a bootstrap theme from https://rstudio.github.io/shinythemes/
             theme = shinytheme("united"),
             
             # Interactive Map panel -------------------------------------------------- 
             tabPanel("Interactive Map", #tags$style(type = "text/css", "#map {height: calc(120vh - 100px) !important;}"),
             fluidRow(
                       
                       column(12, 
                              fluidRow(
                                       h4("Peta Sales Jawa Timur", align="left")),
                              fluidRow(
                                           box(
                                           width = 12,
                                           leafletOutput("map"))
                                           
                                           ),
                              fluidRow(
                                        column(12,
                                                  fluidRow(#tags$head(tags$style(HTML('.box {margin: 1px;}'))),
                                                           column(3,                                         
                                                                    h4("Sales Bedak Bayi per Wilayah", align="left"),
                                                                    tabBox(
                                                                           width = 12,
                                                                           id = "tabset1", height = "100px",
                                                                           tabPanel("Jumlah (Rp mio)", highchartOutput('salesnum')),
                                                                           tabPanel("Growth (%)", highchartOutput('salesgrow'))
                                                                                )     
                                                                    ),
                                                           column(3, 
                                                                  h4("Market Share Est. (%)", align="left"),
                                                                  box(
                                                                    width = 12,
                                                                    p(showOutput("marketchart", "nvd3"), align = "right")
                                                                  )
                                                           ),
                                                           column(3, 
                                                                    h4("Populasi Bayi per Wilayah", align="left"),
                                                                    tabBox(
                                                                           width = 12,
                                                                           id = "tabset3", height = "100px",
                                                                           tabPanel("Jumlah (Jiwa)", highchartOutput('bayinum')),
                                                                           tabPanel("Growth (%)", highchartOutput('bayigrow'))
                                                                      )
                                                                     ),
                                                           column(3, 
                                                                  h4("Pengeluaran per Kapita Penduduk", align="left"),
                                                                  tabBox(
                                                                    width = 12,
                                                                    id = "tabset4", height = "100px",
                                                                    tabPanel("Jumlah (Rp mio)", highchartOutput('kapitanum')),
                                                                    tabPanel("Growth (%)", highchartOutput('kapitagrow'))
                                                                  )
                                                           )
                                                              )
                                        )
                                      )
                              
                              
                              
                              )
                       )
                    
             ),
             
             #Tab Data Explorer------------------------
             tabPanel("Data Explorer",
                      DT::dataTableOutput("data")
             )
             
             
#closing navbar parantheses               
                )

#closing ui parantheses
)

shinyApp(ui = ui, server = server)