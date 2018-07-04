
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
  output$pseudo_scatter_plot <- renderPlotly({
    dataToPlot <- DaneDoScatter(
      dane = YM_ALL_WSK,
      input_data_start = input$dateRange_pseudo[1],
      input_data_koniec = input$dateRange_pseudo[2],
      input_proc = input$Proc_pseudo,
      input_wart = input$Wart_pseudo,
      Wart_COL = "WCSN_PSEUDO",
      WSK_COL = "WSK_PSEUDO"
    )
    
    ScatterPlotly(dane = dataToPlot,
                  input_proc = input$Proc_pseudo,
                  input_wart = input$Wart_pseudo,
                  Wart_COL = "WCSN_PSEUDO",
                  WSK_COL = "WSK_PSEUDO",
                  fragmentOpisu = "pseudoefedryny",
                  source = "pseudo_scatter")
    
    
    # plot_ly(
    #   dataToPlot,
    #   x = ~ WCSN_PSEUDO,
    #   y = ~ WSK_PSEUDO,
    #   type = 'scatter',
    #   mode = 'markers',
    #   hoverinfo = 'text',
    #   source = "pseudo_scatter",
    #   text = ~ paste(
    #     'CKK: ',
    #     CKK,
    #     '</br>',
    #     '</br> Wartość sprzedaży pseudoefedryna w zł: ',
    #     paste0(round(WCSN_PSEUDO / 1000, 1), "tys"),
    #     '</br> Udział %: ',
    #     percent(WSK_PSEUDO),
    #     '</br> Płatnik: ',
    #     PLATNIK,
    #     '</br> Miasto: ',
    #     MIEJSCOWOSC,
    #     '</br> Ulica: ',
    #     ULICA
    #   )
    # ) %>%
    #   layout(
    #     dragmode = "select",
    #     xaxis = list(title = "Wartość w zł", range = c(0, 2 * 1000 * 1000)),
    #     yaxis = list(
    #       title = "Wskaźnik %",
    #       range = c(0, 1.05),
    #       tickformat = "%"
    #     ),
    #     shapes = list(
    #       hline(input$Proc / 100, color = "red"),
    #       vline(input$Wart * 1000, color = "red")
    #     )
    #   ) %>%
    #   config(displayModeBar = F)
    
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

  ## 0.1.5 ####

  ## 0.1.6 dt_pseudo_select####
  output$dt_pseudo_select <- renderDataTable({
    pseudo_d_dt <- event_data("plotly_selected", source = "pseudo_scatter")
    if (is.null(pseudo_d_dt)) {
      tibble(x = "empty", y = "empty") 
      } else { 
       
        dataToPlot <- DaneDoScatter(
          dane = YM_ALL_WSK_PSEUDO,
          input_data_start = input$dateRange_pseudo[1],
          input_data_koniec = input$dateRange_pseudo[2],
          input_proc = input$Proc_pseudo,
          input_wart = input$Wart_pseudo,
          Wart_COL = "WCSN_PSEUDO",
          WSK_COL = "WSK_PSEUDO"
        )
          dataToPlot %>%
          filter(LP %in% pseudo_d_dt$pointNumber)
        #pseudo_d_dt
        }
  })
  
  ## 0.1.7 ym_pseudo_plot_select ####
  output$ym_pseudo_plot_select <- renderPlotly({
    pseudo_d_ym <- event_data("plotly_selected", source = "pseudo_scatter")
    if (is.null(pseudo_d_ym)) {
      plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
    } else { 
      
      dataToPlot_temp <- DaneDoScatter(
        dane = YM_ALL_WSK,
        input_data_start = input$dateRange_pseudo[1],
        input_data_koniec = input$dateRange_pseudo[2],
        input_proc = input$Proc_pseudo,
        input_wart = input$Wart_pseudo,
        Wart_COL = "WCSN_PSEUDO",
        WSK_COL = "WSK_PSEUDO"
      )
      dataToPlot_temp <- dataToPlot_temp %>%
        filter(LP %in% pseudo_d_ym$pointNumber) %>%
        select(CKK) 
      
      dataToPlot <- YM_ALL_WSK %>%
        filter(CKK %in% dataToPlot_temp$CKK) %>%
        mutate(WCSN_ALL_MINUS_PSEUDO = WCSN_ALL - WCSN_PSEUDO) %>%
        group_by(YMD) %>%
        summarise(WCSN_ALL_MINUS_PSEUDO = sum(WCSN_ALL_MINUS_PSEUDO, na.rm = T),
                  WCSN_PSEUDO = sum(WCSN_PSEUDO))
      
      plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_ALL_MINUS_PSEUDO,
              type = 'bar', name = 'Sprzedaż niepseudoefedryny') %>%
        add_trace(y = ~WCSN_PSEUDO, name = 'Sprzedaż pseudoefedryny') %>%
        layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
        config(displayModeBar = F)
    }
  })
  
  
  ## 0.1.8 Scatter plot Wartość w zl vs Udział % wskaźnika deficytów ####
  output$def_scatter_plot <- renderPlotly({
    dataToPlot <- DaneDoScatter(
      dane = YM_ALL_WSK,
      input_data_start = input$dateRange_def[1],
      input_data_koniec = input$dateRange_def[2],
      input_proc = input$Proc_def,
      input_wart = input$Wart_def,
      Wart_COL = "WCSN_DEF",
      WSK_COL = "WSK_DEF"
    )
    
    
    ScatterPlotly(dane = dataToPlot,
                  input_proc = input$Proc_pseudo,
                  input_wart = input$Wart_pseudo,
                  Wart_COL = "WCSN_DEF",
                  WSK_COL = "WSK_DEF",
                  fragmentOpisu = "deficytów",
                  source = "def_scatter")
    
    # plot_ly(
    #   dataToPlot,
    #   x = ~ WCSN_DEF,
    #   y = ~ WSK_DEF,
    #   type = 'scatter',
    #   mode = 'markers',
    #   hoverinfo = 'text',
    #   source = "def_scatter",
    #   text = ~ paste(
    #     'CKK: ',
    #     CKK,
    #     '</br>',
    #     '</br> Wartość sprzedaży deficytów w zł: ',
    #     paste0(round(WCSN_DEF / 1000, 1), "tys"),
    #     '</br> Udział %: ',
    #     percent(WSK_DEF),
    #     '</br> Płatnik: ',
    #     PLATNIK,
    #     '</br> Miasto: ',
    #     MIEJSCOWOSC,
    #     '</br> Ulica: ',
    #     ULICA
    #   )
    # ) %>%
    #   layout(
    #     dragmode = "select",
    #     xaxis = list(title = "Wartość w zł", range = c(0, 2 * 1000 * 1000)),
    #     yaxis = list(
    #       title = "Wskaźnik %",
    #       range = c(0, 1.05),
    #       tickformat = "%"
    #     ),
    #     shapes = list(
    #       hline(input$Proc / 100, color = "red"),
    #       vline(input$Wart * 1000, color = "red")
    #     )
    #   ) %>%
    #   config(displayModeBar = F)
    
  })
  
  ## 0.1.7 dt_def_select ####
  output$dt_def_select <- renderDataTable({
    def_d_dt <- event_data("plotly_selected", source = "def_scatter")
    if (is.null(def_d_dt)) {
      tibble(x = "empty")
    } else { 
      
      dataToPlot <- DaneDoScatter(
        dane = YM_ALL_WSK,
        input_data_start = input$dateRange_def[1],
        input_data_koniec = input$dateRange_def[2],
        input_proc = input$Proc_def,
        input_wart = input$Wart_def,
        Wart_COL = "WCSN_DEF",
        WSK_COL = "WSK_DEF"
      )
        dataToPlot %>%
        filter(LP %in% def_d_dt$pointNumber)
    }
  })
  
  
  
  
  
  
  
  ## 0.1.7 ym_def_plot_select ####
  output$ym_def_plot_select <- renderPlotly({
    def_d_YM <- event_data("plotly_selected", source = "def_scatter")
    if (is.null(def_d_YM)) {
     
    } else { 
      
      dataToPlot_temp <- DaneDoScatter(
        dane = YM_ALL_WSK,
        input_data_start = input$dateRange_def[1],
        input_data_koniec = input$dateRange_def[2],
        input_proc = input$Proc_def,
        input_wart = input$Wart_def,
        Wart_COL = "WCSN_DEF",
        WSK_COL = "WSK_DEF"
      )
      dataToPlot_temp <- dataToPlot_temp %>%
        filter(LP %in% def_d_YM$pointNumber) %>%
        select(CKK) 
      
      dataToPlot <- YM_ALL_WSK %>%
        filter(CKK %in% dataToPlot_temp$CKK) %>%
        mutate(WCSN_ALL_MINUS_DEF = WCSN_ALL - WCSN_DEF) %>%
        group_by(YMD) %>%
        summarise(WCSN_ALL_MINUS_DEF = sum(WCSN_ALL_MINUS_DEF, na.rm = T),
                  WCSN_DEF = sum(WCSN_DEF))
      
      plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_ALL_MINUS_DEF, type = 'bar', name = 'Sprzedaż niedeficytow') %>%
        add_trace(y = ~WCSN_DEF, name = 'Sprzedaż deficytów') %>%
        layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
        config(displayModeBar = F)
      
      
    }
  })
})
