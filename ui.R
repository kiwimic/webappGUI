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
  dashboardHeader(title = "Analiza sprzedaży PGF SA"),
  dashboardSidebar(sidebarMenu(
    menuItem("Podstawowe statystyki", tabName = "ymplot"),
    menuItem("Przegląd pseudoefedryny", tabName = "pseudo_przeglad"),
    menuItem("Przeglad deficytów", tabName = "ymplot"),
    menuItem("Przegląd refundowanych", tabName = "ymplot"),
    menuItem("Przegląd wyrbanych CKT", tabName = "ymplot"),
    menuItem("Export danych do excel", tabName = "ymplot"),
    menuItem("Generowanie raportów HTML", tabName = "ymplot")
  )),
  dashboardBody(
    tabItems(tabItem(
      ##tabItem pseudoefedryna####
      "pseudoefedryna",
      
      fluidRow(
        box(
          width = 4,
          sliderInput(
            "Proc",
            "Wskaźnik udziału pseudoefedryny w zakupach:",
            min = 0,
            max = 100,
            value = 5
          ),
          sliderInput(
            "Wart",
            "Wartość zakupu pseudoefedryny w tys zł:",
            min = 1,
            max = 1000,
            value = 5
          ),
          dateRangeInput(
            'dateRange',
            label = 'Wybierz okres: RRRR-MM-DD',
            start = ymd("2016-06-01"),
            end = Sys.Date()
          )
          
        ),
        
        box(
          width = 8,
          title = "Apteki kupujące pseudoefedrynę",
          plotlyOutput("pseudoPlot")
        ),
        box(width = 12,
            title = "Wyniki zaznaczeń"),
               dataTableOutput("brush")
        ),
      box(width = 12,
          title = "Wyniki zaznaczeń2"),
          plotlyOutput("select2")
    ),
    tabItem(
      "pseudo_przeglad",
      box(
        width = 12,
        title = "Apteki kupujące pseudoefedrynę",
        plotlyOutput("pseudoPlotYM")
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
     
    )
  )
  
)
)
