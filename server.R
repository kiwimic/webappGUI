
## 0.0.1 wymagane pakiety####
library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(RSQLite)
library(lubridate)
library(data.table)

## 0.1.0 Serwer ####
shinyServer(function(input, output, session) {
   
  
  ## 0.1.1 Scatter plot Wartość w zł vs Udział % wskaźnika psedoefedryny ####
  output$pseudoPlot <- renderPlotly({
    

  d1 <- input$dateRange[1]
  d2 <- input$dateRange[2]  
  p3 <- input$Proc
  p4 <- input$Wart
    
    dataToPlot <- YM_ALL_WSK_PSEUDO %>%
      mutate(YMD = ymd(paste0(YM, "-01"))) %>%
      filter(YMD >=d1, YMD <= d2) %>%
      group_by(CKK) %>%
      summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T),
                WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
      mutate(WSK_PSEUDO = WCSN_PSEUDO/WCSN_ALL,
             WSK_PSEUDO = ifelse(WSK_PSEUDO > 1, 1, WSK_PSEUDO),
             WSK_PSEUDO = ifelse(WSK_PSEUDO < 0, 0, WSK_PSEUDO)) %>%
      filter(WSK_PSEUDO >= input$Proc/100) %>%
      filter(WCSN_PSEUDO >= input$Wart * 1000) %>%
      left_join(select(BAZA_CKK, -GPS, -NIP), by = c("CKK"="ID")) %>%
      mutate(LP = 1:n())
      
    
    

 
    plot_ly(dataToPlot, x = ~WCSN_PSEUDO, y = ~WSK_PSEUDO, type = 'scatter', mode = 'markers',
            hoverinfo = 'text', source = "scatterZaznacz",
            text = ~paste('CKK: ', CKK,
                          '</br>',
                          '</br> Wartość sprzedaży pseudoefedryna w zł: ',paste0(round(WCSN_PSEUDO/1000,1),"tys"),
                          '</br> Udział %: ', percent(WSK_PSEUDO),
                          '</br> Płatnik: ', PLATNIK,
                          '</br> Miasto: ', MIEJSCOWOSC,
                          '</br> Ulica: ', ULICA)) %>% 
      layout(dragmode = "select",
             xaxis = list(title = "Wartość w zł", range = c(0,2 * 1000 * 1000)),
             yaxis = list(title = "Wskaźnik %", range = c(0,1.05), tickformat = "%"),
             shapes = list(hline(input$Proc/100, color = "red"), vline(input$Wart*1000, color = "red"))) %>%
      config(displayModeBar = F)
    
  })
  ## 0.1.2 ####
  output$pseudoPlotYM <- renderPlotly({
    
    
  
    dataToPlot <- YM_PSEUDO_PLOT2
    
    plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_PSEUDO, color = ~WSK, type = "bar", 
            hoverinfo = 'text',
            text = ~paste('CKK: ', CKK,
                          '</br>',
                          '</br> Wartość sprzedaży pseudoefedryna w zł: ',paste0(round(WCSN_PSEUDO/1000,1),"tys"),
                          '</br> Udział %: ', percent(PROC),
                          '</br> Płatnik: ', PLATNIK,
                          '</br> Miasto: ', MIEJSCOWOSC,
                          '</br> Ulica: ', ULICA)) %>%
      layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
       config(displayModeBar = F)
      
  })
  
  ## 0.1.3 ####
  output$pseudoPlotYM_CKK <- renderPlotly({
    
    if (length(input$in6) > 10) {
      dataToPlot <- YM_ALL_WSK_PSEUDO %>%
        mutate(WCSN_ALL_MINUS_PSEUDO = WCSN_ALL - WCSN_PSEUDO) %>%
        group_by(YMD) %>%
        summarise(WCSN_ALL_MINUS_PSEUDO = sum(WCSN_ALL_MINUS_PSEUDO, na.rm = T),
                  WCSN_PSEUDO = sum(WCSN_PSEUDO))
    } else {
      dataToPlot <- YM_ALL_WSK_PSEUDO %>%
        mutate(WCSN_ALL_MINUS_PSEUDO = WCSN_ALL - WCSN_PSEUDO) %>%
        filter(CKK %in% input$in6) %>%
        group_by(YMD) %>%
        summarise(WCSN_ALL_MINUS_PSEUDO = sum(WCSN_ALL_MINUS_PSEUDO, na.rm = T),
                  WCSN_PSEUDO = sum(WCSN_PSEUDO))
    }
    
    
    
    
    plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_ALL_MINUS_PSEUDO, type = 'bar', name = 'Sprzedaż niepseudoefedryny') %>%
      add_trace(y = ~WCSN_PSEUDO, name = 'Sprzedaż pseudoefedryny') %>%
      layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
      config(displayModeBar = F)
    
  
  })
  
  ## 0.1.4 ####
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  ## 0.1.5 ####
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  ## 0.1.6 ####
  output$brush <- renderDataTable({
    d <- event_data("plotly_selected", source = "scatterZaznacz")
    if (is.null(d)) {
      cat("Click and drag events (i.e., select/lasso) appear here (double-click to clear)") 
      } else { 
       
       d1 <- input$dateRange[1]
        d2 <- input$dateRange[2]  
        
        dataToPlot <- YM_ALL_WSK_PSEUDO %>%
          mutate(YMD = ymd(paste0(YM, "-01"))) %>%
          filter(YMD >=d1, YMD <= d2) %>%
          group_by(CKK) %>%
          summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T),
                    WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
          mutate(WSK_PSEUDO = WCSN_PSEUDO/WCSN_ALL,
                 WSK_PSEUDO = ifelse(WSK_PSEUDO > 1, 1, WSK_PSEUDO),
                 WSK_PSEUDO = ifelse(WSK_PSEUDO < 0, 0, WSK_PSEUDO)) %>%
          filter(WSK_PSEUDO >= input$Proc/100) %>%
          filter(WCSN_PSEUDO >= input$Wart * 1000) %>%
          left_join(select(BAZA_CKK, -GPS, -NIP), by = c("CKK"="ID")) %>%
          mutate(LP = (1:n())-1) %>%
          filter(LP %in% d$pointNumber)
        }
  })
  
  ## 0.1.7 ####
  output$select2 <- renderPlotly({
    d <- event_data("plotly_selected", source = "scatterZaznacz")
    if (is.null(d)) {
      cat("Click and drag events (i.e., select/lasso) appear here (double-click to clear)") 
    } else { 
      
      d1 <- input$dateRange[1]
      d2 <- input$dateRange[2]  
      
      dataToPlot_temp <- YM_ALL_WSK_PSEUDO %>%
        mutate(YMD = ymd(paste0(YM, "-01"))) %>%
        filter(YMD >=d1, YMD <= d2) %>%
        group_by(CKK) %>%
        summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T),
                  WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
        mutate(WSK_PSEUDO = WCSN_PSEUDO/WCSN_ALL,
               WSK_PSEUDO = ifelse(WSK_PSEUDO > 1, 1, WSK_PSEUDO),
               WSK_PSEUDO = ifelse(WSK_PSEUDO < 0, 0, WSK_PSEUDO)) %>%
        filter(WSK_PSEUDO >= input$Proc/100) %>%
        filter(WCSN_PSEUDO >= input$Wart * 1000) %>%
        left_join(select(BAZA_CKK, -GPS, -NIP), by = c("CKK"="ID")) %>%
        mutate(LP = (1:n())-1) %>%
        filter(LP %in% d$pointNumber) %>%
        select(CKK) 
      
      dataToPlot <- YM_ALL_WSK_PSEUDO %>%
        filter(CKK %in% dataToPlot_temp$CKK) %>%
        mutate(WCSN_ALL_MINUS_PSEUDO = WCSN_ALL - WCSN_PSEUDO) %>%
        group_by(YMD) %>%
        summarise(WCSN_ALL_MINUS_PSEUDO = sum(WCSN_ALL_MINUS_PSEUDO, na.rm = T),
                  WCSN_PSEUDO = sum(WCSN_PSEUDO))

    plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_ALL_MINUS_PSEUDO, type = 'bar', name = 'Sprzedaż niepseudoefedryny') %>%
      add_trace(y = ~WCSN_PSEUDO, name = 'Sprzedaż pseudoefedryny') %>%
      layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
      config(displayModeBar = F)
    
      
    }
  })
  

})
