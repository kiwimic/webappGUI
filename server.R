

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
  updateSelectizeInput(
    session = session,
    inputId = "ckk_raport_download",
    choices = c(Choose = '', unique(BAZA_CKK$ID)),
    server = TRUE
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "ckp_raport_download",
    choices = c(Choose = '', unique(BAZA_CKK$PLATNIK)),
    server = TRUE
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "ckk_kanibalizm",
    choices = c(Choose = '', unique(Mam_GPS_temp$ID)),
    server = TRUE
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "ckp_def_wybor",
    choices = c(Choose = '', unique(BAZA_CKK$PLATNIK)),
    server = TRUE
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "ckp_ref_wybor",
    choices = c(Choose = '', unique(BAZA_CKK$PLATNIK)),
    server = TRUE
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "ckp_pseudo_wybor",
    choices = c(Choose = '', unique(BAZA_CKK$PLATNIK)),
    server = TRUE
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "ckp_custom_wybor",
    choices = c(Choose = '', unique(BAZA_CKK$PLATNIK)),
    server = TRUE
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "ckt_custom_multi",
    choices = c(Choose = '', unique(BAZA_CKT$ID)),
    server = TRUE
  )
  
  ###CKP_DEF##
  CKP_def <- reactive({
    
    input$goButton_def
    
    CKK <- isolate(ifelse(
      is.na(as.numeric(input$ckp_def_wybor)),
      NA,
      as.numeric(input$ckp_def_wybor)
    ))
    return(CKK)
  })
  
  ###CKP_REF##
  CKP_ref <- reactive({
    
    input$goButton_ref
    
    CKK <- isolate(ifelse(
      is.na(as.numeric(input$ckp_ref_wybor)),
      NA,
      as.numeric(input$ckp_ref_wybor)
    ))
    return(CKK)
  })
  
  ###CKP_PSEUDO##
  CKP_pseudo <- reactive({
    
    input$goButton_pseudo
    
    CKK <- isolate(ifelse(
      is.na(as.numeric(input$ckp_pseudo_wybor)),
      NA,
      as.numeric(input$ckp_pseudo_wybor)
    ))
    return(CKK)
  })
  
  ###CKP_custom##
  CKP_custom <- reactive({
    
    input$goButton_custom
    
    CKK <- isolate(ifelse(
      is.na(as.numeric(input$ckp_custom_wybor)),
      NA,
      as.numeric(input$ckp_custom_wybor)
    ))
    return(CKK)
  })
  

  dataToStats <- reactive({
    input$goButton_stats
    
    ret <- isolate(
      data_table_stats(dataToShow = YM_ALL_WSK,
                       input_data_start = input$dateRange_stats[1],
                       input_data_koniec = input$dateRange_stats[2])
    )
    
    return(ret)
  })
  
  
   output$dt_stats <- renderDataTable({
    
       dataToStats()
      
   })
   
   CKT_custom <- reactive({
     
     input$goButton_custom
     
     CKT <- isolate(ifelse(
       is.na(as.numeric(input$ckt_custom_multi)),
       NA,
       as.numeric(input$ckt_custom_multi)
     ))
     return(CKT)
   })
   
   ## Query do do danych do scatter do CUSTOM :)
  # customCKTdata <- reactive({
  #   
  # })
     
   ## 0.1.1 Dane do scatter custom jako reactive, by nie liczyć kilkukrotnie ####
   dataToPlot_custom_fetch <- reactive({
     input$goButton_custom
     
     customCKTdata <- isolate(
       PobierzCustomCKTzBAZY(Platnik = CKP_custom(),
                             CKK = NA,
                             input_data_start = input$dateRange_custom[1],
                             input_data_koniec = input$dateRange_custom[2],
                             wybraneCKT = CKT_custom())
     )
   })
   
   dataToPlot_custom <- reactive({
     input$goButton_custom
     # 
     # customCKTdata <- isolate(
     #   PobierzCustomCKTzBAZY(Platnik = CKP_custom(),
     #                         CKK = NA,
     #                         input_data_start = input$dateRange_custom[1],
     #                         input_data_koniec = input$dateRange_custom[2],
     #                         wybraneCKT = CKT_custom())
     #                          )
     
     dataToPlot <- isolate(
       DaneDoScatter(
         dane = dataToPlot_custom_fetch(),
         Platnik = CKP_custom(),
         CKK = NA,
         input_data_start = input$dateRange_custom[1],
         input_data_koniec = input$dateRange_custom[2],
         input_proc = input$Proc_custom,
         input_wart = input$Wart_custom,
         Wart_COL = "WCSN_CUSTOM",
         WSK_COL = "WSK_CUSTOM"
       )
     )
   })
   
   
    
  ## 0.1.1 Dane do scatter psedo jako reactive, by nie liczyć kilkukrotnie ####
  dataToPlot_pseudo <- reactive({
    input$goButton_pseudo
    
    dataToPlot <- isolate(
      DaneDoScatter(
        dane = YM_ALL_WSK,
        Platnik = CKP_pseudo(),
        CKK = NA,
        input_data_start = input$dateRange_pseudo[1],
        input_data_koniec = input$dateRange_pseudo[2],
        input_proc = input$Proc_pseudo,
        input_wart = input$Wart_pseudo,
        Wart_COL = "WCSN_PSEUDO",
        WSK_COL = "WSK_PSEUDO"
      )
    )
  })
  
  ## 0.1.2 Dane do scatter ref jako reactive, by nie liczyć kilkukrotnie ####
  dataToPlot_ref <- reactive({
    input$goButton_ref
    
    dataToPlot <- isolate(
      DaneDoScatter(
        dane = YM_ALL_WSK,
        Platnik = CKP_ref(),
        CKK = NA,
        input_data_start = input$dateRange_ref[1],
        input_data_koniec = input$dateRange_ref[2],
        input_proc = input$Proc_ref,
        input_wart = input$Wart_ref,
        Wart_COL = "WCSN_REF",
        WSK_COL = "WSK_REF"
      )
    )
  })
  ## 0.1.2 Dane do scatter def jako reactive, by nie liczyć kilkukrotnie ####
  dataToPlot_def <- reactive({
    input$goButton_def
    
    dataToPlot <- isolate(
      DaneDoScatter(
        dane = YM_ALL_WSK,
        Platnik = CKP_def(),
        CKK = NA,
        input_data_start = input$dateRange_def[1],
        input_data_koniec = input$dateRange_def[2],
        input_proc = input$Proc_def,
        input_wart = input$Wart_def,
        Wart_COL = "WCSN_DEF",
        WSK_COL = "WSK_DEF"
      )
    )
  })
  
  ## 0.1.1 Scatter plot PSEUDOEFEDRYNA zł vs Udział % wskaźnika psedoefedryny ####
  output$pseudo_scatter_plot <- renderPlotly({
    input$goButton_pseudo
    
    ret <- isolate(
      ScatterPlotly(
        dane = dataToPlot_pseudo(),
        input_proc = input$Proc_pseudo,
        input_wart = input$Wart_pseudo,
        Wart_COL = "WCSN_PSEUDO",
        WSK_COL = "WSK_PSEUDO",
        fragmentOpisu = "pseudoefedryny",
        source = "pseudo_scatter"
      )
    )
    return(ret)
    
  })
   
   ## Scatter plot CUSTOM zł vs udział wskaźnika
   output$custom_scatter_plot <- renderPlotly({
     input$goButton_custom
     
     ret <- isolate(
       ScatterPlotly(
         dane = dataToPlot_custom(),
         input_proc = input$Proc_custom,
         input_wart = input$Wart_custom,
         Wart_COL = "WCSN_CUSTOM",
         WSK_COL = "WSK_CUSTOM",
         fragmentOpisu = "wybranych CKT",
         source = "custom_scatter"
       )
     )
     return(ret)
     
   }) 
   
  ## 0.1.2 all_ym_bar####
  output$all_ym_bar <- renderPlotly({
    YM_ALL_WSK_grouped <- YM_ALL_WSK %>%
      group_by(YMD) %>%
      summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T))
    
    
    plot_ly(
      data = YM_ALL_WSK_grouped,
      x = ~ YMD,
      y = ~ WCSN_ALL,
      type = "bar",
      text = ~ paste0(round(WCSN_ALL / (1000 * 1000)), "mln zł."),
      textposition = 'auto'
    ) %>%
      layout(barmode = 'stack')
    
   
  })
  
  ## 0.1.3 Podstawowe statsy DATATABLE####
  # output$dt_stats <- renderDataTable({
  #   data_table_stats(
  #     dataToShow = dataToStats(),
  #     input_data_start = input$dateRange_stats[1],
  #     input_data_koniec = input$dateRange_stats[2]
  #     )
  #   
  # })
  # 
  ## 0.1.3 ####
  output$pseudoPlotYM_CKK <- renderPlotly({
    if (length(input$in6) > 10) {
      dataToPlot <- YM_ALL_WSK_PSEUDO %>%
        mutate(WCSN_ALL_MINUS_PSEUDO = WCSN_ALL - WCSN_PSEUDO) %>%
        group_by(YMD) %>%
        summarise(
          WCSN_ALL_MINUS_PSEUDO = sum(WCSN_ALL_MINUS_PSEUDO, na.rm = T),
          WCSN_PSEUDO = sum(WCSN_PSEUDO)
        )
    } else {
      dataToPlot <- YM_ALL_WSK_PSEUDO %>%
        mutate(WCSN_ALL_MINUS_PSEUDO = WCSN_ALL - WCSN_PSEUDO) %>%
        filter(CKK %in% input$in6) %>%
        group_by(YMD) %>%
        summarise(
          WCSN_ALL_MINUS_PSEUDO = sum(WCSN_ALL_MINUS_PSEUDO, na.rm = T),
          WCSN_PSEUDO = sum(WCSN_PSEUDO)
        )
    }
    
    
    
    
    plot_ly(
      dataToPlot,
      x = ~ YMD,
      y = ~ WCSN_ALL_MINUS_PSEUDO,
      type = 'bar',
      name = 'Sprzedaż niepseudoefedryny'
    ) %>%
      add_trace(y = ~ WCSN_PSEUDO, name = 'Sprzedaż pseudoefedryny') %>%
      layout(yaxis = list(title = 'Wartość w zł'),
             barmode = 'stack') %>%
      config(displayModeBar = F)
    
    
  })
  
  
  ## 0.1.5 ####
  
  ## 0.1.6 dt_pseudo_select####
  output$dt_pseudo_select <- renderDataTable({
    data_table_plotly(
      dataToShow = dataToPlot_pseudo(),
      source = "pseudo_scatter",
      points = NA,
      Wart_COL = "WCSN_PSEUDO",
      WSK_COL = "WSK_PSEUDO"
    )
    
  })
  
  ## 0.1.7 ym_pseudo_plot_select ####
  output$ym_pseudo_plot_select <- renderPlotly({
    ym_select_plotly(
      dataToPlot = dataToPlot_pseudo(),
      source = "pseudo_scatter",
      points = NA,
      Wart_COL = "WCSN_PSEUDO",
      name1 = "Sprzedaż pozostała",
      name2 = "Sprzedaż pseudoefedryny"
    )
    
  })
  
  
  ## 0.1.8 Scatter plot Wartość w zl vs Udział % wskaźnika deficytów ####
  output$def_scatter_plot <- renderPlotly({
    input$goButton_def
    
    
    ret <- isolate(
      ScatterPlotly(
        dane = dataToPlot_def(),
        input_proc = input$Proc_def,
        input_wart = input$Wart_def,
        Wart_COL = "WCSN_DEF",
        WSK_COL = "WSK_DEF",
        fragmentOpisu = "deficytów",
        source = "def_scatter"
      )
    )
    
    return(ret)
    
    
  })
  
  ## 0.1.7 dt_def_select ####
  output$dt_def_select <- renderDataTable({
    data_table_plotly(
      dataToShow = dataToPlot_def(),
      source = "def_scatter",
      points = NA,
      Wart_COL = "WCSN_DEF",
      WSK_COL = "WSK_DEF"
    )
  })
   
   
   ## 0.1.7 dt_custom_select ####
   output$dt_custom_select <- renderDataTable({
     data_table_plotly(
       dataToShow = dataToPlot_custom(),
       source = "custom_scatter",
       points = NA,
       Wart_COL = "WCSN_CUSTOM",
       WSK_COL = "WSK_CUSTOM"
     )
   })
   
  
  
  
  
  
  
  
  ## 0.1.7 ym_def_plot_select ####
  output$ym_def_plot_select <- renderPlotly({
    ym_select_plotly(
      dataToPlot = dataToPlot_def(),
      source = "def_scatter",
      points = NA,
      Wart_COL = "WCSN_DEF",
      name1 = "Sprzedaż pozostała",
      name2 = "Sprzedaż deficytów"
    )
    
  })
  
  
  ## 0.1.10 Scatter plot Wartość w zł vs Udział % wskaźnika ref ####
  output$ref_scatter_plot <- renderPlotly({
    input$goButton_ref
    
    ret <- isolate(
      ScatterPlotly(
        dane = dataToPlot_ref(),
        input_proc = input$Proc_ref,
        input_wart = input$Wart_ref,
        Wart_COL = "WCSN_REF",
        WSK_COL = "WSK_REF",
        fragmentOpisu = "refundacji",
        source = "ref_scatter"
      )
    )
    
    return(ret)
    
    
    
  })
  
  
  ## 0.1.11 dt_ref_select####
  output$dt_ref_select <- renderDataTable({
    data_table_plotly(
      dataToShow = dataToPlot_ref(),
      source = "ref_scatter",
      points = NA,
      Wart_COL = "WCSN_REF",
      WSK_COL = "WSK_REF"
    )
    
    
  })
  
  
  ## 0.1.12 ym_ref_plot_select ####
  output$ym_ref_plot_select <- renderPlotly({
    ym_select_plotly(
      dataToPlot = dataToPlot_ref(),
      source = "ref_scatter",
      points = NA,
      Wart_COL = "WCSN_REF",
      name1 = "Sprzedaż pozostała",
      name2 = "Sprzedaż refundacji"
    )
    
  })
  
   ## 0.1.12 ym_custom_plot_select ####
   output$ym_custom_plot_select <- renderPlotly({
     ym_select_plotly(
       dataToPlot = dataToPlot_custom(),
       source = "custom_scatter",
       points = NA,
       Wart_COL = "WCSN_CUSTOM",
       name1 = "Sprzedaż pozostała",
       name2 = "Sprzedaż wybranych preparatów",
       customdata = dataToPlot_custom_fetch()
     )
     
   }) 
   
   
  ##CKK do kanibalizacji####
  Kanibalizacja_CKK <- reactive({
    
    input$goButton_kanibalizm
    
    CKK <- isolate(ifelse(
      is.na(as.numeric(input$ckk_kanibalizm)),
      16571,
      as.numeric(input$ckk_kanibalizm)
      ))
    return(CKK)
  })
   
   

  ## 0.1.8 Kanibalizacja mapa dane do niej ####
  KanibalizacjaRynku_dane <- reactive({
    #input$goButton_kanibalizm
    
    ret <- WystawNajblizszeCKKdlaPunktu(
      data = Mam_GPS_temp,
      ID = Kanibalizacja_CKK(),
      distInMeters = input$kanibalizm_km * 1000
    )
    
    return(ret)
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected",
          input$ckk_kanibalizm,
          "\nClass to:",
          class(input$ckk_kanibalizm),
          "\nA kanibalizacja_CKK() to: ", Kanibalizacja_CKK())
  })
  
  ## 0.1.8 Kanibalizacja mapa#####
  output$kanibalizm_mapa <-  renderLeaflet({
    
    #input$goButton_kanibalizm

    Apteka_dane <- KanibalizacjaRynku_dane() %>%
      left_join(RodzajPodmiotu, by = "ID")

   AptekaCentrum <- Kanibalizacja_CKK()
    centr_LNG <- Apteka_dane$lng[Apteka_dane$ID == AptekaCentrum]
    centr_LAT <- Apteka_dane$lat[Apteka_dane$ID == AptekaCentrum]
    
    ret <- isolate(
      Apteka_dane %>%
        #filter(Dist_in_meters <= input$kanibalizm_km * 1000) %>%
        #left_join(select(Mam_GPS_temp, ID, lng, lat), by = c("ID_2" = "ID")) %>%
        leaflet() %>%
        addTiles() %>%
        setView(
          lng = centr_LNG,
          lat = centr_LAT,
          zoom = 8
        ) %>%
        addMarkers(lng =  ~ lng,
                   lat = ~ lat,
                   popup = ~paste0("CKK apteki: ", ID,
                                   "<br>",
                                   "Płatnik:", PLATNIK,
                                   "<br>",
                                   "Miasto: ",MIEJSCOWOSC,
                                   "<br>",
                                   "Ulica: ", ULICA,
                                   "<br>",
                                   "Odległość od wybranej apteki: ", paste0(
                                     round(Dist_in_meters/1000,1), " km"),
                                   "<br>",
                                   "<br>",
                                   "Za okres: ", paste0("od ",input$kanibalizm_daterange[1],
                                                        " do ", input$kanibalizm_daterange[2]))) %>%
        addCircleMarkers(centr_LNG, centr_LAT)
    )
    return(ret)
  })
  
  output$kanibalizm_dt <- renderDataTable({
    
     input$goButton_kanibalizm
    
    Apteka_dane <- isolate(KanibalizacjaRynku_dane())
     
    Wartosci <- isolate(YM_ALL_WSK %>%
    filter(CKK %in% Apteka_dane$ID) %>%
        mutate(YMD = ymd(paste0(YM, "-01"))) %>%
         filter(
           YMD >= ymd(input$kanibalizm_daterange[1]),
           YMD <= ymd(input$kanibalizm_daterange[2])
         ) %>%
         group_by(CKK) %>%
         summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T)) %>%
         arrange(desc(WCSN_ALL)) %>%
         mutate(Udzial_proc = percent(WCSN_ALL / sum(WCSN_ALL))))
    
    Apteka_dane <- isolate(Apteka_dane %>%
                             left_join(Wartosci, by = c("ID"="CKK")) %>%
                             filter(WCSN_ALL > 0) %>%
                             arrange(desc(WCSN_ALL)) %>%
                             mutate(WCSN_ALL = PLN(round(WCSN_ALL))) %>%
                             left_join(select(BAZA_CKK, ID, Podmiot), by = c("ID"="ID")) %>%
                             select(ID, PLATNIK, Udzial_proc, WCSN_ALL, Podmiot))
      
    ret <- Apteka_dane
    return(ret)
    
    # 
    # Apteka_dane <- Mam_GPS_temp
    # 
    # 
    # AptekaCentrum <- isolate(as.numeric(input$ckk_kanibalizm))
    # 
    # if (is.null(AptekaCentrum)) {
    #   AptekaCentrum <- 16517
    # }
    # centr_LNG <-
    #   isolate(Mam_GPS_temp$lng[Mam_GPS_temp$ID == AptekaCentrum])
    # centr_LAT <-
    #   isolate(Mam_GPS_temp$lat[Mam_GPS_temp$ID == AptekaCentrum])
    # 
    # temp <-
    #   isolate(ObliczOdleglosciOdPunktu(data = Mam_GPS_temp, id = AptekaCentrum))
    # 
    # 
    # isolate(Apteka_dane %>%
    #   left_join(select(temp, ID_2, Dist_in_meters), by = c("ID" = "ID_2"))) -> Apteka_dane 
    # 
    # 
    # 
    # isolate(Apteka_dane %>%
    #   filter(Dist_in_meters <= input$kanibalizm_km * 1000) %>%
    #   select(ID)) -> WybraneApteki
    # 
    # isolate(YM_ALL_WSK %>%
    #   filter(CKK %in% WybraneApteki$ID) %>%
    #   mutate(YMD = ymd(paste0(YM, "-01"))) %>%
    #   filter(
    #     YMD >= ymd(input$kanibalizm_daterange[1]),
    #     YMD <= ymd(input$kanibalizm_daterange[2])
    #   ) %>%
    #   group_by(CKK) %>%
    #   summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T)) %>%
    #   arrange(desc(WCSN_ALL)) %>%
    #   mutate(Udzial_proc = percent(WCSN_ALL / sum(WCSN_ALL))) %>%
    #   left_join(
    #     select(
    #       BAZA_CKK,
    #       ID,
    #       PLATNIK,
    #       Podmiot
    #     ),
    #     by = c("CKK" = "ID")
    #   )) -> ret
    # 
 
    
  })
  
  daneRaportExcel <- reactive({
    input$goButton_download_excel
    
    ret1 <- isolate({
      
                  ExportRaportExcelDlaAptekiLubCKP(input_data_start = input$dateRange_excel[1],
                                                   input_data_koniec = input$dateRange_excel[2],
                                                   CKK = input$ckk_raport_download,
                                                   Platnik = input$ckp_raport_download)
      
    })
      # d1 <- as.numeric(som(input$dateRange_excel[1])) * 86400
      # d2 <- as.numeric(eom(input$dateRange_excel[2])) * 86400
      # 
      # 
      # d1 <- as.POSIXct(d1, origin = "1970-01-01 00:00:00 UTC")
      # d2 <- as.POSIXct(d2, origin = "1970-01-01 00:00:00 UTC")
      # 
      # 
      # hour(d1) <- 0
      # minute(d1) <- 0
      # second(d1) <- 0
      # 
      # hour(d2) <- 23
      # minute(d2) <- 59
      # second(d2) <- 59
      # 
      # d1 <- as.numeric(d1)
      # d2 <- as.numeric(d2)
      # 
      # input$goButton_download_excel
      # CKK_apteki <- isolate(as.numeric(input$ckk_raport_download))
      # 
      # DaneDuzaBaza_tab1 <- tbl(myDB, "tab1") %>%
      #   filter(CKK == CKK_apteki) %>%
      #   filter(DATA_ZAFAKTUROWANIA >= d1, DATA_ZAFAKTUROWANIA <= d2) %>%
      #   group_by(CKK, CKT, DATA_ZAFAKTUROWANIA) %>%
      #   summarise(
      #     WCSN = sum(WCSN, na.rm = T),
      #     WCSN_PO_RABATACH = sum(WCSN_PO_RABATACH, na.rm = T),
      #     ILOSC = sum(ILOSC, na.rm = T),
      #     LICZBA_WIERSZY = n()
      #   ) %>%
      #   collect() %>%
      #   left_join(select(BAZA_CKT, ID, NAZWA_OFERTOWA, Opis_caly),
      #             by = c("CKT" = "ID")) %>%
      #   ungroup() %>%
      #   mutate(
      #     DATA_ZAFAKTUROWANIA = as.POSIXct(DATA_ZAFAKTUROWANIA, origin = "1970-01-01 00:00:00 UTC"),
      #     YM = str_sub(as.character(DATA_ZAFAKTUROWANIA), 1, 7)
      #   )
      # 
      # 
      # DanePSEDO <- DaneDuzaBaza_tab1 %>%
      #   semi_join(BAZA_PSEUDO, by = c("CKT" = "ID"))
      # 
      # DaneDEF <- DaneDuzaBaza_tab1 %>%
      #   inner_join(BAZA_DEF, by = c("CKT" = "ID"))
      # 
      # 
      # DaneREF <- DaneDuzaBaza_tab1 %>%
      #   inner_join(BAZA_REF, by = c("CKT" = "ID")) %>%
      #   mutate(
      #     START = as.POSIXct.Date(START, origin = "1970-01-01 00:00:00 UTC"),
      #     KONIEC = as.POSIXct.Date(KONIEC, origin = "1970-01-01 00:00:00 UTC")
      #   ) %>%
      #   mutate(WCSN_REF = ifelse(
      #     DATA_ZAFAKTUROWANIA >= START &
      #       DATA_ZAFAKTUROWANIA <= KONIEC,
      #     WCSN,
      #     0
      #   ))
      # 
      # DaneDEF2 <- DaneDEF %>%
      #   mutate(
      #     START = as.POSIXct.Date(START, origin = "1970-01-01 00:00:00 UTC"),
      #     KONIEC = as.POSIXct.Date(KONIEC, origin = "1970-01-01 00:00:00 UTC")
      #   ) %>%
      #   mutate(WCSN_DEF = ifelse(
      #     DATA_ZAFAKTUROWANIA >= START &
      #       DATA_ZAFAKTUROWANIA <= KONIEC,
      #     WCSN,
      #     0
      #   ))
      # 
      # Legenda <- tibble()
      # Podsum <- tibble()
      # 
      # ret <- list(
      #   Legenda = Legenda,
      #   Podsum = Podsum,
      #   Dane_zrodlowe = DaneDuzaBaza_tab1,
      #   Pseudoefedryna = DanePSEDO,
      #   Deficyty = DaneDEF,
      #   Deficyty_czas_lista = DaneDEF2,
      #   Refundacja = DaneREF
      # )
      # 
      # return(ret)
      
   
    
    
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
      CKP_apteki <- as.numeric(input$ckk_raport_download)
      
      front <- ifelse(is.na(CKK_apteki), CKP_apteki, CKK_apteki)
      
      filename <-  paste0("Raport",
                          "_",
                          paste(
                            stringr::str_extract_all(Sys.time(), pattern = "[0-9]", simplify = T),
                            collapse = ""
                          ),
                          ".xlsx")
      return(filename)
    },
    content = function(filename) {
      write_xlsx(daneRaportExcel(), path = filename)
    }
  )
  
  
  ##Download_pseudo_widok ####
  output$download_pseudo_widok <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(
      "Raport_pseudo_",
      paste(
        stringr::str_extract_all(Sys.Date(), pattern = "[0-9]", simplify = T),
        collapse = ""
      ),
      ".html"
    ),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "raport_pseudo_html.Rmd")
      file.copy("raport_pseudo_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      paramsALL <- list(
        input_wart = input$Wart_pseudo,
        input_proc = input$Proc_pseudo,
        d1 = input$dateRange_pseudo[1],
        d2 = input$dateRange_pseudo[2],
        CKP = CKP_pseudo(),
        CKK = 0,
        points = event_data("plotly_selected", source = "pseudo_scatter")$pointNumber
      )
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = paramsALL,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  ##Download_def_widok ####
  output$download_def_widok <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(
      "Raport_def_",
      paste(
        stringr::str_extract_all(Sys.Date(), pattern = "[0-9]", simplify = T),
        collapse = ""
      ),
      ".html"
    ),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "raport_def_html.Rmd")
      file.copy("raport_def_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      paramsALL <- list(
        input_wart = input$Wart_def,
        input_proc = input$Proc_def,
        d1 = input$dateRange_def[1],
        d2 = input$dateRange_def[2],
        CKP = CKP_def(),
        points = event_data("plotly_selected", source = "def_scatter")$pointNumber
      )
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = paramsALL,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  ##Download_ref_widok ####
  output$download_ref_widok <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(
      "Raport_ref_",
      paste(
        stringr::str_extract_all(Sys.Date(), pattern = "[0-9]", simplify = T),
        collapse = ""
      ),
      ".html"
    ),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "raport_ref_html.Rmd")
      file.copy("raport_ref_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      paramsALL <- list(
        input_wart = input$Wart_ref,
        input_proc = input$Proc_ref,
        d1 = input$dateRange_ref[1],
        d2 = input$dateRange_ref[2],
        CKP = CKP_ref(),
        points = event_data("plotly_selected", source = "ref_scatter")$pointNumber
      )
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = paramsALL,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
})
