server <- function(input,output,session){
  
  observeEvent(c(input$sp,input$days),{
    output$company <- renderText({
      comp <<- as.character(sp %>% dplyr::filter(Tickers == input$sp) %>% dplyr::select(Company))
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
      current <<- sp_list$df.tickers %>% filter(ticker == input$sp) %>% arrange(desc(ref.date)) %>% slice(1) %>% dplyr::select(price.close)
      paste0("$",round(current$price.close,2))})
    output$days30 <- renderText({
      days30 <<- sp_list$df.tickers %>% filter(ticker == input$sp) %>% arrange(desc(ref.date)) %>% 
        slice(1:30) %>% dplyr::select(price.close) %>% summarise(AVG30 = mean(price.close))
      paste0("$",round(days30$AVG30,2))})
    output$days30_med <- renderText({
      days30_med <<- sp_list$df.tickers %>% filter(ticker == input$sp) %>% arrange(desc(ref.date)) %>% 
        slice(1:30) %>% dplyr::select(price.close) %>% summarise(MED30 = median(price.close))
      paste0("$",round(days30_med$MED30,2))})
    output$dygraph <- renderDygraph({
      sect <- as.character(sp %>% dplyr::filter(Tickers == input$sp) %>% dplyr::select(GICS.Sector))
      sp_sector_select <<- sp %>% dplyr::filter(GICS.Sector  == sect) %>% dplyr::select(Tickers)
      sp_tib <- as_tibble(sp_list$df.tickers %>% 
                            dplyr::filter(ticker == sp_sector_select$Tickers) %>%
                            dplyr::arrange(desc(ref.date)) %>% 
                            dplyr::slice(1:input$days))
      sp_xts <- xts(sp_tib %>% dplyr::select(price.close,ref.date),order.by = sp_tib$ref.date)
      dygraph(sp_xts) %>% dyRangeSelector()
    })})
  
  output$ovv_df <- renderDT({datatable(ovv_list$df.tickers,rownames = FALSE)})
  output$ovv_dygraph <- renderDygraph({
    ovv_tib <- as_tibble(ovv_list$df.tickers)
    ovv_xts <- xts(ovv_tib %>% dplyr::select(price.close,ref.date),order.by = ovv_tib$ref.date)
    dygraph(ovv_xts) %>% dyRangeSelector()
  })
  
  observeEvent(input$sim_run,{
    result <- vector("numeric", input$sims)
    for (run in 1:input$sims){
      income <- sample(input$income[1]:input$income[2],1,replace = T)
      expense <- sample(input$expense[1]:input$expense[2],1,replace = T)
      margin <- income - expense
      result[run] <- margin
      print(margin)
    }
    resultAVG <- mean(result)
    annual_growth <- vector("numeric", input$years)
    years_project <- resultAVG * 12 * input$years + input$assets 
    output$life <- renderValueBox({ 
      valueBox(scales::dollar(years_project[2]),subtitle = "Projected")})
  })
  
}