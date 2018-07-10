
## 0.0.1 wymagane pakiety####
library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(RSQLite)
library(lubridate)
library(data.table)
library(leaflet)
library(writexl)

## 0.1.0 Serwer ####
shinyServer(function(input, output, session) {
   
  
  
  ## 0.1.1 Dane do scatter psedo jako reactive, by nie liczyć kilkukrotnie ####
  dataToPlot_pseudo <- reactive({
    
    input$goButton_pseudo
    
    dataToPlot <- isolate(DaneDoScatter(
      dane = YM_ALL_WSK,
      input_data_start = input$dateRange_pseudo[1],
      input_data_koniec = input$dateRange_pseudo[2],
      input_proc = input$Proc_pseudo,
      input_wart = input$Wart_pseudo,
      Wart_COL = "WCSN_PSEUDO",
      WSK_COL = "WSK_PSEUDO"
    ))
  })
  
  ## 0.1.2 Dane do scatter ref jako reactive, by nie liczyć kilkukrotnie ####
  dataToPlot_ref <- reactive({
    input$goButton_ref
    
    dataToPlot <- isolate(DaneDoScatter(
      dane = YM_ALL_WSK,
      input_data_start = input$dateRange_ref[1],
      input_data_koniec = input$dateRange_ref[2],
      input_proc = input$Proc_ref,
      input_wart = input$Wart_ref,
      Wart_COL = "WCSN_REF",
      WSK_COL = "WSK_REF"
    ))
  })
  ## 0.1.2 Dane do scatter def jako reactive, by nie liczyć kilkukrotnie ####
  dataToPlot_def <- reactive({
    
    input$goButton_def
    
    dataToPlot <- isolate(DaneDoScatter(
      dane = YM_ALL_WSK,
      input_data_start = input$dateRange_def[1],
      input_data_koniec = input$dateRange_def[2],
      input_proc = input$Proc_def,
      input_wart = input$Wart_def,
      Wart_COL = "WCSN_DEF",
      WSK_COL = "WSK_DEF"
    ))
  })
  
  ## 0.1.1 Scatter plot Wartość w zł vs Udział % wskaźnika psedoefedryny ####
  output$pseudo_scatter_plot <- renderPlotly({
    
    input$goButton_pseudo
    
    # dataToPlot <- isolate(DaneDoScatter(
    #   dane = YM_ALL_WSK,
    #   input_data_start = input$dateRange_pseudo[1],
    #   input_data_koniec = input$dateRange_pseudo[2],
    #   input_proc = input$Proc_pseudo,
    #   input_wart = input$Wart_pseudo,
    #   Wart_COL = "WCSN_PSEUDO",
    #   WSK_COL = "WSK_PSEUDO"
    # ))
    
    ret <- isolate(ScatterPlotly(dane = dataToPlot_pseudo(),
                  input_proc = input$Proc_pseudo,
                  input_wart = input$Wart_pseudo,
                  Wart_COL = "WCSN_PSEUDO",
                  WSK_COL = "WSK_PSEUDO",
                  fragmentOpisu = "pseudoefedryny",
                  source = "pseudo_scatter"))
    return(ret)
    
    
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
  ## 0.1.2 all_ym_bar####
  output$all_ym_bar <- renderPlotly({
    
    
    
   YM_ALL_WSK_grouped <- YM_ALL_WSK %>%
      group_by(YMD) %>%
      summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T))
    
    
    plot_ly(data =YM_ALL_WSK_grouped, 
            x = ~YMD,
            y = ~WCSN_ALL,
            type = "bar",
            text = ~paste0(round(WCSN_ALL/(1000*1000)), "mln zł."),
            textposition = 'auto') %>%
      layout(barmode = 'stack')
      
    
    
    
  })
  
  # output$all_ym_boxplot_pseudo <- renderPlotly({
  #   
  #   YM_ALL_WSK %>%
  #   mutate(WSK_PSEUDO = ifelse(is.na(WSK_PSEUDO), 0, WSK_PSEUDO)) %>%
  #   mutate(WSK_PSEUDO = ifelse(WSK_PSEUDO > 1, 1, WSK_PSEUDO),
  #          WSK_PSEUDO = ifelse(WSK_PSEUDO < 0, 0, WSK_PSEUDO)) -> dane
  # 
  #   lista <- vector("list", length = length(unique(dane$YM)))
  #   k <- 1
  #   for (i in unique(dane$YM)) {
  #     temp_data <- dane %>%
  #       filter(YM == i)    
  #     
  #     temp <- tibble(YM = i,
  #                    values = seq(0, 1, 0.01), 
  #                    quants = quantile(temp_data$WSK_PSEUDO, probs = seq(0, 1, 0.01)))
  #     lista[[k]] <- temp 
  #     k <- k+1
  #   }
  #   
  #    plot_ly(y = ~WSK_PSEUDO, type = "histogram",  histnorm = "probability")
  #   
  # })
  # 
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
  

  ## 0.1.5 ####

  ## 0.1.6 dt_pseudo_select####
  output$dt_pseudo_select <- renderDataTable({
    pseudo_d_dt <- event_data("plotly_selected", source = "pseudo_scatter")
    if (is.null(pseudo_d_dt)) {
      tibble(x = "empty", y = "empty") 
      } else { 
       
        # dataToPlot <- DaneDoScatter(
        #   dane = YM_ALL_WSK,
        #   input_data_start = input$dateRange_pseudo[1],
        #   input_data_koniec = input$dateRange_pseudo[2],
        #   input_proc = input$Proc_pseudo,
        #   input_wart = input$Wart_pseudo,
        #   Wart_COL = "WCSN_PSEUDO",
        #   WSK_COL = "WSK_PSEUDO"
        # )
          dataToPlot_pseudo() %>%
          filter(LP %in% pseudo_d_dt$pointNumber)
        #pseudo_d_dt
        }
  })
  
  ## 0.1.7 ym_pseudo_plot_select ####
  output$ym_pseudo_plot_select <- renderPlotly({
    
    ym_select_plotly(dataToPlot = dataToPlot_pseudo(),
                     source = "pseudo_scatter",
                     points = NA,
                     Wart_COL = "WCSN_PSEUDO",
                     name1 = "Sprzedaż pozostała",
                     name2 = "Sprzedaż pseudoefedryny"
                    )
    
    # pseudo_d_ym <- event_data("plotly_selected", source = "pseudo_scatter")
    # if (is.null(pseudo_d_ym)) {
    #   plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
    # } else { 
    #   
    #   # dataToPlot_temp <- DaneDoScatter(
    #   #   dane = YM_ALL_WSK,
    #   #   input_data_start = input$dateRange_pseudo[1],
    #   #   input_data_koniec = input$dateRange_pseudo[2],
    #   #   input_proc = input$Proc_pseudo,
    #   #   input_wart = input$Wart_pseudo,
    #   #   Wart_COL = "WCSN_PSEUDO",
    #   #   WSK_COL = "WSK_PSEUDO"
    #   # )
    #   dataToPlot_temp <- dataToPlot_pseudo() %>%
    #     filter(LP %in% pseudo_d_ym$pointNumber) %>%
    #     select(CKK) 
    #   
    #   dataToPlot <- YM_ALL_WSK %>%
    #     filter(CKK %in% dataToPlot_temp$CKK) %>%
    #     mutate(WCSN_ALL_MINUS_PSEUDO = WCSN_ALL - WCSN_PSEUDO) %>%
    #     group_by(YMD) %>%
    #     summarise(WCSN_ALL_MINUS_PSEUDO = sum(WCSN_ALL_MINUS_PSEUDO, na.rm = T),
    #               WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T))
    #   
    #   plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_ALL_MINUS_PSEUDO, type = 'bar', name = 'Sprzedaż pozostała') %>%
    #     add_trace(y = ~WCSN_PSEUDO, name = 'Sprzedaż pseudoefedryny') %>%
    #     layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
    #     config(displayModeBar = F)
    # }
  })
  
  
  ## 0.1.8 Scatter plot Wartość w zl vs Udział % wskaźnika deficytów ####
  output$def_scatter_plot <- renderPlotly({
    
    input$goButton_def
    
    
     # dataToPlot <- isolate(DaneDoScatter(
     #  dane = YM_ALL_WSK,
     #  input_data_start = input$dateRange_def[1],
     #  input_data_koniec = input$dateRange_def[2],
     #  input_proc = input$Proc_def,
     #  input_wart = input$Wart_def,
     #  Wart_COL = "WCSN_DEF",
     #  WSK_COL = "WSK_DEF"))
    
    
    ret <- isolate(
      ScatterPlotly(dane = dataToPlot_def(),
                  input_proc = input$Proc_def,
                  input_wart = input$Wart_def,
                  Wart_COL = "WCSN_DEF",
                  WSK_COL = "WSK_DEF",
                  fragmentOpisu = "deficytów",
                  source = "def_scatter")
    )
    
    return(ret)
    
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
      
      # dataToPlot <- DaneDoScatter(
      #   dane = YM_ALL_WSK,
      #   input_data_start = input$dateRange_def[1],
      #   input_data_koniec = input$dateRange_def[2],
      #   input_proc = input$Proc_def,
      #   input_wart = input$Wart_def,
      #   Wart_COL = "WCSN_DEF",
      #   WSK_COL = "WSK_DEF"
      # )
        dataToPlot_def() %>%
        filter(LP %in% def_d_dt$pointNumber)
    }
  })
  
  
  
  
  
  
  
  ## 0.1.7 ym_def_plot_select ####
  output$ym_def_plot_select <- renderPlotly({
    def_d_YM <- event_data("plotly_selected", source = "def_scatter")
    if (is.null(def_d_YM)) {
      plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
    } else { 
      
      # dataToPlot_temp <- DaneDoScatter(
      #   dane = YM_ALL_WSK,
      #   input_data_start = input$dateRange_def[1],
      #   input_data_koniec = input$dateRange_def[2],
      #   input_proc = input$Proc_def,
      #   input_wart = input$Wart_def,
      #   Wart_COL = "WCSN_DEF",
      #   WSK_COL = "WSK_DEF"
      # )
      dataToPlot_temp <- dataToPlot_def() %>%
        filter(LP %in% def_d_YM$pointNumber) %>%
        select(CKK) 
      
      dataToPlot <- YM_ALL_WSK %>%
        filter(CKK %in% dataToPlot_temp$CKK) %>%
        mutate(WCSN_DEF = ifelse(is.na(WCSN_DEF), 0, WCSN_DEF)) %>%
        mutate(WCSN_ALL_MINUS_DEF = WCSN_ALL - WCSN_DEF) %>%
        group_by(YMD) %>%
        summarise(WCSN_ALL_MINUS_DEF = sum(WCSN_ALL_MINUS_DEF, na.rm = T),
                  WCSN_DEF = sum(WCSN_DEF, na.rm = T))
      
      plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_ALL_MINUS_DEF, type = 'bar', name = 'Sprzedaż pozostała') %>%
        add_trace(y = ~WCSN_DEF, name = 'Sprzedaż deficytów') %>%
        layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
        config(displayModeBar = F)
      
      
    }
  })
  
  
  ## 0.1.10 Scatter plot Wartość w zł vs Udział % wskaźnika ref ####
  output$ref_scatter_plot <- renderPlotly({
   
    input$goButton_ref
    
    #  dataToPlot <- isolate(DaneDoScatter(
    #   dane = YM_ALL_WSK,
    #   input_data_start = input$dateRange_ref[1],
    #   input_data_koniec = input$dateRange_ref[2],
    #   input_proc = input$Proc_ref,
    #   input_wart = input$Wart_ref,
    #   Wart_COL = "WCSN_REF",
    #   WSK_COL = "WSK_REF"
    # ))
    
    ret <- isolate(ScatterPlotly(dane = dataToPlot_ref(),
                  input_proc = input$Proc_ref,
                  input_wart = input$Wart_ref,
                  Wart_COL = "WCSN_REF",
                  WSK_COL = "WSK_REF",
                  fragmentOpisu = "refundacji",
                  source = "ref_scatter")
                  )
    
    return(ret)
    
    
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
  
  
  ## 0.1.11 dt_ref_select####
  output$dt_ref_select <- renderDataTable({
    ref_d_dt <- event_data("plotly_selected", source = "ref_scatter")
    if (is.null(ref_d_dt)) {
      tibble(x = "empty", y = "empty") 
    } else { 
      
      # dataToPlot <- DaneDoScatter(
      #   dane = YM_ALL_WSK,
      #   input_data_start = input$dateRange_ref[1],
      #   input_data_koniec = input$dateRange_ref[2],
      #   input_proc = input$Proc_ref,
      #   input_wart = input$Wart_ref,
      #   Wart_COL = "WCSN_REF",
      #   WSK_COL = "WSK_REF"
      # )
      dataToPlot_ref() %>%
        filter(LP %in% ref_d_dt$pointNumber)
      #pseudo_d_dt
    }
  })
  
  
  ## 0.1.12 ym_ref_plot_select ####
  output$ym_ref_plot_select <- renderPlotly({
    ref_d_YM <- event_data("plotly_selected", source = "ref_scatter")
    if (is.null(ref_d_YM)) {
      plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
    } else { 
      
      # dataToPlot_temp <- DaneDoScatter(
      #   dane = YM_ALL_WSK,
      #   input_data_start = input$dateRange_ref[1],
      #   input_data_koniec = input$dateRange_ref[2],
      #   input_proc = input$Proc_ref,
      #   input_wart = input$Wart_ref,
      #   Wart_COL = "WCSN_REF",
      #   WSK_COL = "WSK_REF"
      # )
      dataToPlot_temp <- dataToPlot_ref() %>%
        filter(LP %in% ref_d_YM$pointNumber) %>%
        select(CKK) 
      
      dataToPlot <- YM_ALL_WSK %>%
        filter(CKK %in% dataToPlot_temp$CKK) %>%
        mutate(WCSN_REF = ifelse(is.na(WCSN_REF), 0, WCSN_REF)) %>%
        mutate(WCSN_ALL_MINUS_REF = WCSN_ALL - WCSN_REF) %>%
        group_by(YMD) %>%
        summarise(WCSN_ALL_MINUS_REF = sum(WCSN_ALL_MINUS_REF, na.rm = T),
                  WCSN_REF = sum(WCSN_REF, na.rm = T))
      
      plot_ly(dataToPlot, x = ~YMD, y = ~WCSN_ALL_MINUS_REF, type = 'bar', name = 'Sprzedaż pozostała') %>%
        add_trace(y = ~WCSN_REF, name = 'Sprzedaż refundowanych') %>%
        layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
        config(displayModeBar = F)
      
      
    }
  })
  
## 0.1.8 Kanibalizacja mapa#####  
 output$kanibalizm_mapa <-  renderLeaflet({
   
   input$goButton_kanibalizm
   Apteka_dane <- isolate(read.csv2(paste0("C:\\Users\\msiwik\\Desktop\\FOLDER R\\Analiza_Prepeparatow\\Dane\\GPS\\",
                                    input$kanibalizm_ckk, ".csv")))
   
   AptekaCentrum <- isolate(as.numeric(input$kanibalizm_ckk))
   centr_LNG <- isolate(Mam_GPS_temp$lng[Mam_GPS_temp$ID==AptekaCentrum])
   centr_LAT <- isolate(Mam_GPS_temp$lat[Mam_GPS_temp$ID==AptekaCentrum])
   
   ret <- isolate(Apteka_dane %>%
     filter(Dist_in_meters <= input$kanibalizm_km * 1000) %>%
     left_join(select(Mam_GPS_temp, ID, lng, lat), by = c("ID_2"="ID")) %>%
   leaflet() %>%
     addTiles() %>%
     setView(lng = centr_LNG, lat = centr_LAT, zoom = 8) %>%
     addMarkers(lng =  ~lng,
                lat = ~lat) %>%
     addCircleMarkers(centr_LNG, centr_LAT))
   
   # ~paste0("CKK apteki: ", CKK,
   #         "<br>",
   #         "Miasto: ",MIEJSCOWOSC,
   #         "<br>",
   #         "Ulica: ", ULICA,
   #         "<br>",
   #         "Udział pseudoefedryny w zakupach: ", percent(WSK_PSEUDOEFEDRYNA),
   #         "<br>",
   #         "Wartość zakupu pseudoefedryny: ", paste0(round(PSEUDOEFEDRYNA_WCSN/1000), "tys. zł."),
   #         "<br>",
   #         "Za okres: ", "od 2018-01-01 do 2018-05-31")
   return(ret)
 })
 
 output$kanibalizm_dt <- renderDataTable({
   
   Apteka_dane <- read.csv2(paste0("C:\\Users\\msiwik\\Desktop\\FOLDER R\\Analiza_Prepeparatow\\Dane\\GPS\\",
                                   input$kanibalizm_ckk, ".csv"))
   
   AptekaCentrum <- as.numeric(input$kanibalizm_ckk)
   
   Apteka_dane %>%
     filter(Dist_in_meters <= input$kanibalizm_km * 1000) %>%
     left_join(select(Mam_GPS_temp, ID, lng, lat), by = c("ID_2"="ID")) %>%
     select(ID_2) -> WybraneApteki
   
 YM_ALL_WSK %>%
     filter(CKK %in% WybraneApteki$ID_2) %>%
     mutate(YMD = ymd(paste0(YM, "-01"))) %>%
     filter(YMD >= ymd(input$kanibalizm_daterange[1]),
            YMD <= ymd(input$kanibalizm_daterange[2])) %>%
     group_by(CKK) %>%
     summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T)) %>%
     arrange(desc(WCSN_ALL)) %>%
     mutate(Udzial_proc = percent(WCSN_ALL/sum(WCSN_ALL))) %>%
     left_join(select(BAZA_CKK, ID, PLATNIK, NIP, MIEJSCOWOSC, ULICA, STATUS, RODZAJ_PODMIOTU), by = c("CKK"="ID"))
    
   
 })
 
 daneRaportExcel <- reactive({
     
   input$goButton_download_excel
   
   ret1 <- isolate({
     
     d1 <- as.numeric(som(input$dateRange_excel[1])) * 86400 
     d2 <- as.numeric(eom(input$dateRange_excel[2])) * 86400
     
     
     d1 <- as.POSIXct(d1, origin = "1970-01-01 00:00:00 UTC")
     d2 <- as.POSIXct(d2, origin = "1970-01-01 00:00:00 UTC")
     
     
     hour(d1) <- 0
     minute(d1) <- 0
     second(d1) <- 0
     
     hour(d2) <- 23
     minute(d2) <- 59
     second(d2) <- 59
     
     d1 <- as.numeric(d1)
     d2 <- as.numeric(d2)
     
     input$goButton_download_excel
     CKK_apteki <- isolate(as.numeric(input$ckk_raport_download))
     
     DaneDuzaBaza_tab1 <- tbl(myDB, "tab1") %>%
       filter(CKK == CKK_apteki) %>%
       filter(DATA_ZAFAKTUROWANIA >= d1, DATA_ZAFAKTUROWANIA <= d2) %>%
       group_by(CKK, CKT, DATA_ZAFAKTUROWANIA) %>%
       summarise(WCSN = sum(WCSN, na.rm = T),
                 WCSN_PO_RABATACH = sum(WCSN_PO_RABATACH, na.rm = T),
                 ILOSC = sum(ILOSC, na.rm = T),
                 LICZBA_WIERSZY = n()) %>%
       collect() %>%
       left_join(select(BAZA_CKT, ID, NAZWA_OFERTOWA, Opis_caly), by = c("CKT"="ID")) %>%
       ungroup() %>%
       mutate(DATA_ZAFAKTUROWANIA = as.POSIXct(DATA_ZAFAKTUROWANIA, origin = "1970-01-01 00:00:00 UTC"),
              YM = str_sub(as.character(DATA_ZAFAKTUROWANIA),1, 7))
     
     
     DanePSEDO <- DaneDuzaBaza_tab1 %>%
       semi_join(BAZA_PSEUDO, by = c("CKT"="ID"))
     
     DaneDEF <- DaneDuzaBaza_tab1 %>%
       inner_join(BAZA_DEF, by = c("CKT"="ID"))
     
     
     DaneREF <- DaneDuzaBaza_tab1 %>%
       inner_join(BAZA_REF, by = c("CKT"="ID")) %>%
       mutate(START = as.POSIXct.Date(START, origin = "1970-01-01 00:00:00 UTC"),
              KONIEC = as.POSIXct.Date(KONIEC, origin = "1970-01-01 00:00:00 UTC")) %>%
       mutate(WCSN_REF = ifelse(DATA_ZAFAKTUROWANIA>= START & DATA_ZAFAKTUROWANIA <= KONIEC, WCSN, 0))
     
     DaneDEF2 <- DaneDEF %>%
       mutate(START = as.POSIXct.Date(START, origin = "1970-01-01 00:00:00 UTC"),
              KONIEC = as.POSIXct.Date(KONIEC, origin = "1970-01-01 00:00:00 UTC")) %>%
       mutate(WCSN_DEF = ifelse(DATA_ZAFAKTUROWANIA>= START & DATA_ZAFAKTUROWANIA <= KONIEC, WCSN, 0))
     
     Legenda <- tibble()
     Podsum <- tibble()
     
     ret <- list(Legenda = Legenda,
                 Podsum = Podsum,
                 Dane_zrodlowe = DaneDuzaBaza_tab1,
                 Pseudoefedryna = DanePSEDO,
                 Deficyty = DaneDEF,
                 Deficyty_czas_lista = DaneDEF2,
                 Refundacja = DaneREF)
     
     return(ret)
     
   })
   
   
   # for (i in 1:5) {
   #   Sys.sleep(1)
   # }
   # return(mtcars)
 })
 
 output$downloadData <- renderUI({
   req(daneRaportExcel())
   downloadButton("raport_excel_apteka", label = 'Pobierz raport!')
 })
 
  output$raport_excel_apteka <- downloadHandler(
      filename = function() {
        
        CKK_apteki <- as.numeric(input$ckk_raport_download)
        
        filename <-  paste0(CKK_apteki,"_",
                            paste(stringr::str_extract_all(Sys.time(), pattern = "[0-9]", simplify = T), collapse = ""),
                            ".xlsx")
        return(filename)
      },
      content = function(filename) {
        write_xlsx(daneRaportExcel(), path = filename)
      }
    )  
  
  output$download_pseudo_widok <- downloadHandler(
    
    
    
    # For PDF output, change this to "report.pdf"
    filename = paste0("Raport",
                      paste(stringr::str_extract_all(Sys.Date(), pattern = "[0-9]", simplify = T),
                            collapse = ""), ".html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "raport_html.Rmd")
      file.copy("raport_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
       paramsALL <- list(input_wart = input$Wart_pseudo,
                      input_proc = input$Proc_pseudo,
                      d1 = input$dateRange_pseudo[1], 
                      d2 = input$dateRange_pseudo[2],
                      points = event_data("plotly_selected", source = "pseudo_scatter")$pointNumber)
     
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = paramsALL,
                        envir = new.env(parent = globalenv()))
      }
     )
 
})
