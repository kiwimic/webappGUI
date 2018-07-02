#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(RSQLite)
library(lubridate)
library(data.table)

# myDB_YM <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus_YM.sqlite")
# myDB <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus.sqlite") 
# YM_ALL_WSK <- dbSendQuery(myDB_YM, "SELECT * FROM WSK_YM") %>% fetch()
# BAZA_CKK <- dbSendQuery(myDB, "SELECT * FROM tab3") %>% fetch()


# YM_ALL_WSK_PSEUDO <- YM_ALL_WSK %>%
  # filter(!is.na(WCSN_PSEUDO)) %>%
  # filter(WCSN_PSEUDO > 0) %>%
  # mutate(YMD = ymd(paste0(YM, "-01")))

# YM_PSEUDO_PLOT2 <- YM_ALL_WSK_PSEUDO %>%
  # mutate(YMD = ymd(paste0(YM, "-01"))) %>%
  #filter(YMD >=d1, YMD <= d2) %>%
  # group_by(CKK, YMD) %>%
  # summarise(WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
  # group_by(YMD) %>%
  # mutate(PROC = round(WCSN_PSEUDO/sum(WCSN_PSEUDO),4)) %>%
  # mutate(WSK = ifelse(PROC >= 0.04, CKK, "Poniżej 4%")) %>%
  # group_by(YMD, WSK) %>%
  # summarise(WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
  # ungroup() %>%
  # group_by(YMD) %>%
  # mutate(PROC = round(WCSN_PSEUDO/sum(WCSN_PSEUDO),4)) %>%
  # arrange(YMD, desc(PROC)) %>%
  # mutate(CKK = as.numeric(WSK)) %>%
  # left_join(select(BAZA_CKK, -GPS, -NIP), by = c("CKK"="ID"))

# CKKdoWyboru <<- YM_ALL_WSK_PSEUDO %>%
  # group_by(CKK) %>%
  # summarise(WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
  # arrange(desc(WCSN_PSEUDO)) %>%
  # filter(WCSN_PSEUDO > 20000) %>%
  # slice(1:50)

# vline <- function(x = 0, color = "red") {
  # list(
    # type = "line", 
    # y0 = 0, 
    # y1 = 1, 
    # yref = "paper",
    # x0 = x, 
    # x1 = x, 
    # line = list(color = color)
  # )
# }

# hline <- function(y = 0, color = "blue") {
  # list(
    # type = "line", 
    # x0 = 0, 
    # x1 = 1, 
    # xref = "paper",
    # y0 = y, 
    # y1 = y, 
    # line = list(color = color)
  # )
# }
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$pseudoPlot <- renderPlotly({
    

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
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
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
