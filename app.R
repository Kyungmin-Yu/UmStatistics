library(data.table)
library(shiny)
library(tidyverse)
library(lubridate)
library(AnomalyDetection)
# setwd("C:/Users/ykm25/Desktop/공모전/bigcontest2021/shiny")

fw = fread('dong_fw.csv')

ui = fluidPage(
  titlePanel("음식물 배출량 이상치 탐지"),

  sidebarLayout(
    sidebarPanel(
    helpText("제주시 행정동별 음식물 배출량의 이상치 탐지"),

    selectInput("dong",
                label = "행정동을 선택하세요",
                choices = fw$emd_nm %>% unique(),
                selected = "건입동"),

   dateRangeInput("Date",
                label = "날짜를 선택하세요",
                start = "2018-01-01",
                end = "2021-06-30",
                min = "2018-01-01",
                max = "2021-06-30")
   
  ),
  mainPanel(h1(textOutput("selected_dong")),
            
            plotOutput('plot1'),
            
            h3("<이상 탐지일> ",br(),br(), strong(textOutput("anom_date")))
  )
  
  )
  
  
  
  )


# Define server logic ----
server <- function(input, output) {
  
  output$selected_dong = renderText({
    paste0(input$dong , "의 ", input$Date[1], " - ", input$Date[2], " 기간 이상치 탐지 결과" )
  })
  
  res = reactive({
    fw %>% 
      as_tibble() %>% 
      filter(emd_nm==input$dong & input$Date[1] <= base_date & base_date <= input$Date[2]) %>% 
      select(-emd_nm)%>%
      AnomalyDetectionTs(max_anoms=0.1, direction='neg', plot=TRUE)
    
  })
  
  output$plot1 = renderPlot({
    anoms <- res()$anoms
    anoms <- data.frame(anoms)
    
    if(is.null(anoms$timestamp) == T) {
    fw %>% 
      as_tibble() %>% 
      filter(emd_nm==input$dong & input$Date[1] <= base_date & base_date <= input$Date[2]) %>% 
      ggplot(aes(x=base_date, y=em_g))+
      geom_line(colour='RoyalBlue')
      
      } else{
      anoms$timestamp <- as.POSIXct(anoms$timestamp)
      fw %>% 
      as_tibble() %>% 
      filter(emd_nm==input$dong & input$Date[1] <= base_date & base_date <=   input$Date[2]) %>% 
    ggplot(aes(x=base_date, y=em_g))+
      geom_line(colour='RoyalBlue') +
      geom_point(data=anoms, aes(x=timestamp, y=anoms), shape=21, size=3, colour="firebrick")}
    
  })
  
  output$anom_date = renderText({
    if(is.null(res()$anoms$timestamp) == T) {" 없습니다."
      }else{paste(res()$anoms$timestamp %>% as.character(), collapse=", ")}
  })
  
}
# Run the app ----
shinyApp(ui = ui, server = server)
