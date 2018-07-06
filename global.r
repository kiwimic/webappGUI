
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(RSQLite)
library(lubridate)
library(data.table)
library(rlang) ## rlang::sym
library(tidyr)
library(leaflet)
library(writexl)
library(stringr)


### Dorobić funkcje do YM ####

myDB_YM <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus_YM.sqlite")
myDB <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus.sqlite") 
YM_ALL_WSK <- dbSendQuery(myDB_YM, "SELECT * FROM WSK_YM") %>% fetch() %>% mutate(YMD = ymd(paste0(YM, "-01")))
BAZA_CKK <- dbSendQuery(myDB, "SELECT * FROM tab3") %>% fetch()
Mam_GPS_temp <- readxl::read_excel("C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Mam_GPS_temp.xlsx")
BAZA_CKT <- dbSendQuery(myDB, "SELECT * FROM tab2") %>% fetch
BAZA_PSEUDO <- dbSendQuery(myDB, "SELECT * FROM tab7_PSEUDOEFEDRYNA") %>% fetch
BAZA_REF <- dbSendQuery(myDB, "SELECT * FROM tab5_REF") %>% fetch
BAZA_DEF <- dbSendQuery(myDB, "SELECT * FROM tab6_DEF") %>% fetch
### Takie zapytanie to około 30 sekund na moim komputerze
# Sys.time()
# tbl(myDB, "tab1") %>%
#   filter(CKT %in% c(140452,
#                     119519,
#                     209498,
#                     237820,
#                     209120,
#                     204810,
#                     112932,
#                     11222,
#                     138378,
#                     154642
#                     
#   )) %>%
#   group_by(CKK, CKT, DATA_ZAFAKTUROWANIA) %>%
#   summarise(WCSN = sum(WCSN, na.rm = T)) %>%
#   collect()
# Sys.time()  


# tbl(myDB, "tab1") %>%
#      filter(CKK == 123812) %>%
#    group_by(CKK, CKT, DATA_ZAFAKTUROWANIA) %>%
#    summarise(WCSN = sum(WCSN, na.rm = T)) %>%
#    collect() %>%
#    mutate(YMD_HMS = as.POSIXct(DATA_ZAFAKTUROWANIA, origin = "1970-01-01 00:00:00 UTC"))-> test_123812

Apteka_16571 <- read.csv2("C:\\Users\\msiwik\\Desktop\\FOLDER R\\Analiza_Prepeparatow\\Dane\\GPS\\16571.csv")

YM_ALL_WSK_PSEUDO <- YM_ALL_WSK %>%
  filter(!is.na(WCSN_PSEUDO)) %>%
  filter(WCSN_PSEUDO > 0) %>%
  mutate(YMD = ymd(paste0(YM, "-01")))

YM_PSEUDO_PLOT2 <- YM_ALL_WSK_PSEUDO %>%
  mutate(YMD = ymd(paste0(YM, "-01"))) %>%
  group_by(CKK, YMD) %>%
  summarise(WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
  group_by(YMD) %>%
  mutate(PROC = round(WCSN_PSEUDO/sum(WCSN_PSEUDO),4)) %>%
  mutate(WSK = ifelse(PROC >= 0.04, CKK, "Poniżej 4%")) %>%
  group_by(YMD, WSK) %>%
  summarise(WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
  ungroup() %>%
  group_by(YMD) %>%
  mutate(PROC = round(WCSN_PSEUDO/sum(WCSN_PSEUDO),4)) %>%
  arrange(YMD, desc(PROC)) %>%
  mutate(CKK = as.numeric(WSK)) %>%
  left_join(select(BAZA_CKK, -GPS, -NIP), by = c("CKK"="ID"))

CKKdoWyboru <<- YM_ALL_WSK_PSEUDO %>%
  group_by(CKK) %>%
  summarise(WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T)) %>%
  arrange(desc(WCSN_PSEUDO)) %>%
  filter(WCSN_PSEUDO > 20000) %>%
  slice(1:50)

## Rysowanie poziomych lini na wykresie plotly ####
vline <- function(x = 0, color = "red") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

## Rysowanie pionowych lini na wykresie plotly ####
hline <- function(y = 0, color = "blue") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}

filter_col <- function(df, col_name_as_string, val){
  col_name <- rlang::sym(col_name_as_string)
  df %>% filter((!!col_name) == val)
}

#DaneDoScatter(dane = YM_ALL_WSK,
#              input_data_start = "2018-01-01",
#              input_data_koniec = "2018-11-01",
#              Wart_COL = "WCSN_DEF",
#              WSK_COL = "WSK_DEF",
#              input_proc = 5,
#              input_wart = 10
#              ) -> test


##Podstawowe statystyki########
YM_ALL_WSK_grouped <- YM_ALL_WSK %>%
  group_by(YMD) %>%
  summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T),
            WCSN_DEF = sum(WCSN_DEF, na.rm = T),
            WCSN_PSEUDO = sum(WCSN_PSEUDO, na.rm = T),
            WCSN_ALL_DIFF = WCSN_ALL - WCSN_DEF- WCSN_PSEUDO) %>%
  gather(Kat, Wart, WCSN_DEF:WCSN_ALL_DIFF) %>%
  group_by(YMD) %>%
  mutate(PROC = Wart/sum(Wart),
         PROC_label = percent(PROC))

plot_ly(data =YM_ALL_WSK_grouped, 
        x = ~YMD,
        y = ~Wart,
        color = ~Kat,
        type = "bar",
        text = ~PROC_label,
        textposition = 'auto') %>%
  layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack')



## Scatter plot plotly #####
DaneDoScatter <- function(dane,
                          WSK, 
                          input_data_start,
                          input_data_koniec,
                          input_proc = 40,
                          input_wart = 50,
                          Wart_COL = "WCSN_PSEUDO",
                          WSK_COL = "WSK_PSEUDO") {
  
  
  sym_Wart_COL <- rlang::sym(Wart_COL)
  sym_WSK_COL <- rlang::sym(WSK_COL)
  
  
  dataToPlot <- dane %>%
    filter(!is.na(!!sym_Wart_COL)) %>%
    mutate(YMD = ymd(paste0(YM, "-01"))) %>%
    filter(YMD >= ymd(input_data_start),
           YMD <= ymd(input_data_koniec)) %>%
    group_by(CKK) %>%
    summarise(WCSN_ALL = sum(WCSN_ALL, na.rm = T),
              (!!sym_Wart_COL) := sum((!!sym_Wart_COL), na.rm = T)) %>%
   mutate((!!sym_WSK_COL) := (!!sym_Wart_COL)/WCSN_ALL) %>%
   mutate((!!sym_WSK_COL) := ifelse((!!sym_WSK_COL) > 1, 1, (!!sym_WSK_COL)),
          (!!sym_WSK_COL) := ifelse((!!sym_WSK_COL) < 0, 0, (!!sym_WSK_COL))) %>%
  filter((!!sym_WSK_COL) >= input_proc/100) %>%
  filter((!!sym_Wart_COL) >= input_wart  * 1000) %>%
  left_join(select(BAZA_CKK, -GPS, -NIP), by = c("CKK"="ID")) %>%
  ungroup() %>%
  arrange(desc((!!sym_Wart_COL))) %>%
  mutate(LP = (1:n())-1) -> ret
  
  return(ret)
  # d1 <- input$dateRange[1]
  # d2 <- input$dateRange[2]  
  # p3 <- input$Proc
  # p4 <- input$Wart
}

## Wykres dla danych z ScatterPlolty####
ScatterPlotly <- function(dane,
                          fragmentOpisu = "pseudoefedryny",
                          input_proc = 40,
                          input_wart = 50,
                          Wart_COL = "WCSN_PSEUDO",
                          WSK_COL = "WSK_PSEUDO",
                          source = "pseudo_scatter") {
  sym_Wart_COL <- rlang::sym(Wart_COL)
  #sym_Wart_COL <- enquo(sym_Wart_COL)
  sym_WSK_COL <- rlang::sym(WSK_COL)
  #sym_WSK_COL <- enquo(sym_WSK_COL)
  stringZopisem <- paste0("</br> Wartość sprzedaży ", fragmentOpisu, " w zł: ")
  
plot_ly(
    dane,
    x = dane[[Wart_COL]],
    y = dane[[WSK_COL]],
    type = 'scatter',
    mode = 'markers',
    hoverinfo = 'text',
    source = source,
    text = ~ paste(
      'CKK: ',
      CKK,
      '</br>',
      stringZopisem,
      paste0(round(dane[[Wart_COL]] / 1000, 1), "tys"),
      '</br> Udział %: ',
      percent(dane[[WSK_COL]]),
      '</br> Płatnik: ',
      PLATNIK,
      '</br> Miasto: ',
      MIEJSCOWOSC,
      '</br> Ulica: ',
      ULICA,
      '</br> LP: ', dane[['LP']])
) %>%
  layout(
    dragmode = "select",
    xaxis = list(title = "Wartość w zł"),
    yaxis = list(
      title = "Wskaźnik %",
      range = c(0, 1.05),
      tickformat = "%"
    )#,
    #shapes = list(
    #  hline(input_proc / 100, color = "red"),
    #  vline(input_wart * 1000, color = "red")
    #)
  ) %>%
  config(displayModeBar = F)
}
# 
# ScatterPlotly(dane = test,
#               input_proc = 10,
#               input_wart = 20,
#               WSK_COL = "WSK_DEF",
#               Wart_COL = "WCSN_DEF",
#               fragmentOpisu = "deficytów")
# 
KanibalizacjaRynku_dane <- function() {
  
}
  
KanibalizacjaRynku_mapka <- function() {
  
}

ExportRaportExcelDlaApteki <- function() {
  
  
  writexl::write_xlsx(mtcars, path = )
}


