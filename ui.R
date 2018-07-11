#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
library(data.table)

dashboardPage(
  skin = "green",
  ## 0 1.1 Header ####
  dashboardHeader(title = "Analiza sprzedaży PGF SA"),
  
  ## 0.1.2 Sidebar####
  dashboardSidebar(sidebarMenu(
    ## 0.1.2.1 Zakładki sidebar####
    menuItem("Podstawowe statystyki", tabName = "podst_stats"),
    menuItem("Przegląd pseudoefedryny", tabName = "pseudoefedryna"),
    menuItem("Przeglad deficytów", tabName = "deficyty"),
    menuItem("Przegląd refundowanych", tabName = "refundacja"),
    menuItem("Przegląd wybranych CKT", tabName = "custom_ckk"),
    menuItem("Kanibalizacja rynku", tabName = "kanibalizm"),
    menuItem("Export danych do excel", tabName = "export_excel"),
    menuItem("Generowanie raportów HTML", tabName = "ymplot")
  )),
  ## 0.1.3 Body #####
  dashboardBody(
    tabItems(
      tabItem("export_excel",
              box(width = 4,
                  selectizeInput(
                    inputId = 'ckk_raport_download', label = 'Wybierz/wpisz CKK apteki',
                    choices = NULL,
                    selected = 16571),
                  dateRangeInput(
                    'dateRange_excel',
                    label = 'Wybierz okres: RRRR-MM-DD',
                    start = ymd("2016-06-01"),
                    end = Sys.Date()
                  ),
                  actionButton("goButton_download_excel", "Przygotuj raport!"),
                  h3("Uwaga raport może generować się nawet do kilku minut!\n,
                     Po przygotowaniu raportu pojawi się przycisk pobierz."),
                  uiOutput("downloadData")
                  ),
              box(width = 6, 
                  h1("Układ raportów:"),
                  h4("1. Całe dane w danym zakresie dat w podziale na CKK, YM, CKT, WCSN, ILOSC"),
                  h4("2. Tylko pseudoefedryna w danym zakresie dat w podziale na CKK, YM, CKT, WCSN, ILOSC"),
                  h4("3. Tylko deficyty w danym zakresie dat w podziale na CKK, YM, CKT, WCSN, ILOSC"),
                  h4("4. Tylko deficyty w czasie trwania listy w danym zakresie dat w podziale na CKK, YM, CKT, WCSN, ILOSC"),
                  h4("5. Tylko refundacja dat w podziale na CKK, YM, CKT, WCSN, ILOSC"))
              ),
    
            
      tabItem(
      ## 0.1.3.1 Zawartość przegląd pseudoefedryny####
      "pseudoefedryna",
      fluidRow(
        box(
          width = 4,
          sliderInput(
            "Proc_pseudo",
            "Wskaźnik udziału pseudoefedryny w zakupach:",
            min = 0,
            max = 100,
            value = 5
          ),
          sliderInput(
            "Wart_pseudo",
            "Wartość zakupu pseudoefedryny w tys zł:",
            min = 1,
            max = 1000,
            value = 5
          ),
          dateRangeInput(
            'dateRange_pseudo',
            label = 'Wybierz okres: RRRR-MM-DD',
            start = ymd("2016-06-01"),
            end = Sys.Date()
          ),
          actionButton("goButton_pseudo", "Filtruj!"),
          downloadButton("download_pseudo_widok", "Pobierz widok")
        ),
        box(
          width = 8,
          title = "Apteki kupujące pseudoefedrynę",
          plotlyOutput("pseudo_scatter_plot")
        ),
        box(width = 12,
            title = "Ramka danych z zaznaczenia"),
        dataTableOutput("dt_pseudo_select")
      ),
      box(width = 12,
          title = "Wykres YM z zaznaczenia"),
      plotlyOutput("ym_pseudo_plot_select")
    ), ##</0.1.3.1####
    tabItem(
      ## 0.1.3.1 Zawartość przegląd pseudoefedryny####
      "refundacja",
      fluidRow(
        box(
          width = 4,
          sliderInput(
            "Proc_ref",
            "Wskaźnik udziału refundacji w zakupach:",
            min = 0,
            max = 100,
            value = 50
          ),
          sliderInput(
            "Wart_ref",
            "Wartość zakupu refundacji w tys zł:",
            min = 1,
            max = 1000,
            value = 50
          ),
          dateRangeInput(
            'dateRange_ref',
            label = 'Wybierz okres: RRRR-MM-DD',
            start = ymd("2016-06-01"),
            end = Sys.Date()
          ),
          actionButton("goButton_ref", "Filtruj!"),
          downloadButton("download_ref_widok", "Pobierz widok")
        ),
        box(
          width = 8,
          title = "Apteki kupujące preparaty refundowane",
          plotlyOutput("ref_scatter_plot")
        ),
        box(width = 12,
            title = "Ramka danych z zaznaczenia"),
        dataTableOutput("dt_ref_select")
      ),
      box(width = 12,
          title = "Wykres YM z zaznaczenia"),
      plotlyOutput("ym_ref_plot_select")
    ),
    tabItem(
      ## 0.1.3.1 Zawartość przegląd pseudoefedryny####
      "custom_ckk",
      fluidRow(
        box(
          width = 4,
          sliderInput(
            "Proc_custom",
            "Wskaźnik udziału wybranych preparatów w zakupach:",
            min = 0,
            max = 100,
            value = 50
          ),
          sliderInput(
            "Wart_custom",
            "Wartość wybranych preparatów w tys zł:",
            min = 1,
            max = 1000,
            value = 50
          ),
          dateRangeInput(
            'dateRange_custom',
            label = 'Wybierz okres: RRRR-MM-DD',
            start = ymd("2016-06-01"),
            end = Sys.Date()
          ),
          actionButton("goButton_custom", "Filtruj!"),
          downloadButton("download_custom_widok", "Pobierz widok")
        ),
        box(
          width = 8,
          title = "Apteki kupujące preparaty refundowane",
          plotlyOutput("custom_scatter_plot")
        ),
        box(width = 12,
            title = "Ramka danych z zaznaczenia"),
        dataTableOutput("dt_custom_select")
      ),
      box(width = 12,
          title = "Wykres YM z zaznaczenia"),
      plotlyOutput("ym_custom_plot_select")
    ),
    tabItem(
      "podst_stats",
      box(
        width = 12,
        title = "Rozkład miesięczny sprzedaży PGF SA",
        plotlyOutput("all_ym_bar")
      ),
      box(
        width = 12,
        title = "Podstawowe statystyki w okresie - Tabela:",
        dateRangeInput(
          'dateRange_stats',
          label = 'Wybierz okres: RRRR-MM-DD',
          start = ymd("2016-06-01"),
          end = Sys.Date()
        ),
        actionButton("goButton_stats", "Filtruj!"),
        dataTableOutput("dt_stats")
      )
    ), ##Dopracować ta liste :)####
    tabItem(
      "kanibalizm",
      fluidRow(
      box(width = 4,
          selectizeInput(
            inputId = 'ckk_kanibalizm', label = 'Wybierz/wpisz CKK apteki',
            choices = NULL,
            selected = 16571),
            #Do debugowania
            #textOutput("selected_var"),
                    sliderInput(
                      "kanibalizm_km",
                      "Wskaż apteki w odległości x km od wybranej apteki:",
                      min = 0,
                      max = 100,
                      value = 2
                    ),
                    br(),
                    br(),
                    dateRangeInput(
                      'kanibalizm_daterange',
                      label = 'Wybierz okres: RRRR-MM-DD od i do',
                      start = ymd("2016-06-01"),
                      end = Sys.Date()
                    ),
          actionButton("goButton_kanibalizm", "Filtruj!")
        ),
      box(
        width = 8,
        title = "Mapa otoczenia apteki",
        leafletOutput("kanibalizm_mapa")
      )),
      box(
        width = 12,
        title = "Apteki z otoczenia",
        dataTableOutput("kanibalizm_dt")
      )
    
     
    ),
    tabItem(
      "ymplot",
      box(
        width = 12,
        title = "Przegląd pseudoefedryny apteki okresy",
        selectInput(
          'in6',
          'Options',
          CKKdoWyboru$CKK,
          multiple = TRUE,
          selectize = TRUE
        ),
        plotlyOutput("pseudoPlotYM_CKK")
      )
     
    ),
    tabItem(
      ## 0.1.3.7 Zawartość przegląd deficytówy####
      "deficyty",
      fluidRow(
        box(
          width = 4,
          sliderInput(
            "Proc_def",
            "Wskaźnik udziału deficytów w zakupach:",
            min = 0,
            max = 100,
            value = 20
          ),
          sliderInput(
            "Wart_def",
            "Wartość zakupu deficytów w tys zł:",
            min = 1,
            max = 1000,
            value = 20
          ),
          dateRangeInput(
            'dateRange_def',
            label = 'Wybierz okres: RRRR-MM-DD',
            start = ymd("2016-06-01"),
            end = Sys.Date()
          ),
          actionButton("goButton_def", "Filtruj!"),
          downloadButton("download_def_widok", "Pobierz widok")
      ),
        box(
          width = 8,
          title = "Apteki kupujące deficyty",
          plotlyOutput("def_scatter_plot")
        ),
        box(width = 12,
            title = "Ramka danych"),
        dataTableOutput("dt_def_select")
      ),
      box(width = 12,
          title = "Wykres YM dla zaznaczonych CKK"),
      plotlyOutput("ym_def_plot_select")
    )
  )
  
)
)
