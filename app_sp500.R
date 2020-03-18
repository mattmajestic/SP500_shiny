library(pacman)
pacman::p_load(shiny,BatchGetSymbols,shinydashboard,dplyr,plotly,tidyquant,xts,tibble,dygraphs,DT)


first.date <- Sys.Date()-2000
sp <- GetSP500Stocks()
ovv <- "OVV"
# sp_list <- BatchGetSymbols(tickers = sp$Tickers,
#                            first.date = first.date,
#                            last.date = Sys.Date(), 
#                            do.cache=TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Majestic Stocks"),
  dashboardSidebar(),
  dashboardBody(
            box(selectInput("sp",label = "Select Stock",choices = sp$Tickers),
                numericInput("days",label = "Days Back",min = 30,max = 1000,value = 90),
                h4("Company Name"),
                textOutput("company"),
                br(),
                h4("Sector"),
                textOutput("sector"),
                br(),
                h4("Headquarters"),
                textOutput("hq"),
                br(),
                h4("Yesterday's Closing Price"),
                textOutput("current"),
                br(),
                h4("Last 30 Day AVG"),
                textOutput("days30"),
                br(),
                h4("Last 30 Day Median"),
                textOutput("days30_med"),
                #radioButtons()
                width = 3),
            box(plotlyOutput("line"),
                br(),
                h3("Sector Stocks"),
                dygraphOutput("dygraph"),
                DTOutput("dt"),
                width = 9))
    )


server <- function(input,output,session){
  
  observeEvent(c(input$sp,input$days),{
    output$company <- renderText({
      comp <- as.character(sp %>% dplyr::filter(Tickers == input$sp) %>% dplyr::select(Company))
      comp})
   output$sector <- renderText({
     sect <- as.character(sp %>% dplyr::filter(Tickers == input$sp) %>% dplyr::select(GICS.Sector))
     sect})
   output$hq <- renderText({
     hq <- as.character(sp %>% dplyr::filter(Tickers == input$sp) %>% dplyr::select(HQ.Location))
     hq})
   startDate <- Sys.Date() - input$days
   ticker_df <- sp_list$df.tickers %>% dplyr::filter(ticker == input$sp) %>% 
     dplyr::filter(ref.date > startDate) %>% dplyr::mutate(DailyAVG = (price.high + price.low)/2)
    output$line <- renderPlotly({plot_ly(ticker_df,x=~ref.date,y=~price.close,type = "scatter",mode = "lines",name = "Closing Price") %>%
      add_trace(y =~DailyAVG,name = "High/LowAVG") })
    output$dt <- renderDataTable(datatable(ticker_df,rownames = FALSE))
    output$current <- renderText({
      current <- sp_list$df.tickers %>% filter(ticker == input$sp) %>% arrange(desc(ref.date)) %>% slice(1) %>% dplyr::select(price.close)
      paste0("$",round(current$price.close,2))})
    output$days30 <- renderText({
      days30 <- sp_list$df.tickers %>% filter(ticker == input$sp) %>% arrange(desc(ref.date)) %>% 
        slice(1:30) %>% dplyr::select(price.close) %>% summarise(AVG30 = mean(price.close))
      paste0("$",round(days30$AVG30,2))})
    output$days30_med <- renderText({
      days30_med <- sp_list$df.tickers %>% filter(ticker == input$sp) %>% arrange(desc(ref.date)) %>% 
        slice(1:30) %>% dplyr::select(price.close) %>% summarise(MED30 = median(price.close))
      paste0("$",round(days30_med$MED30,2))})
    output$dygraph <- renderDygraph({
      sect <- as.character(sp %>% dplyr::filter(Tickers == input$sp) %>% dplyr::select(GICS.Sector))
      sp_sector_select <- sp %>% dplyr::filter(GICS.Sector  == sect) %>% dplyr::select(Tickers)
      sp_tib <- as_tibble(sp_list$df.tickers %>% 
                            dplyr::filter(ticker == sp_sector_select$Tickers) %>%
                            dplyr::arrange(desc(ref.date)) %>% 
                            dplyr::slice(1:input$days))
      sp_xts <- xts(sp_tib %>% dplyr::select(price.close,ref.date),order.by = sp_tib$ref.date)
      dygraph(sp_xts) %>% dyRangeSelector()
    })
  })
}

shinyApp(ui,server)
