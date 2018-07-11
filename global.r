library(geosphere)
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
RodzajPodmiotu <- readxl::read_excel("C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/RodzajPodmiotuPGF.xlsx")

BAZA_CKK <- dbSendQuery(myDB, "SELECT * FROM tab3") %>% fetch()
BAZA_CKK %>%
  left_join(RodzajPodmiotu, by = c("RODZAJ_PODMIOTU"="ID")) %>%
  select(-RODZAJ_PODMIOTU) -> BAZA_CKK
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

## Start of month fun####
som <- function(x) {
  x <- lubridate::ymd(x)
  as.Date(format(x, "%Y-%m-01"))
}

## end of month fun#####
eom <- function(x) {
  som(som(x) + 35) - 1
}


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

### Test na 1 do PLN
isMoreThanOne <- Vectorize(function(x) {
  if (all(x > 1)) {
    ret <- T
  } else {
    ret <- F
  }
  return(ret)
}, vectorize.args = "x")


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

## Wykres YM dla danych zaznaczonych z ScatterPlotly ####
ym_select_plotly <- function(dataToPlot = dataTooPlot_pseudo(),
                             source = "pseudo_scatter",
                             points = NA,
                             Wart_COL = "WCSN_PSEUDO",
                             name1 = "Sprzedaż pozostała",
                             name2 = "Sprzedaż pseudoefedryny") {
 
   sym_Wart_col <- rlang::sym(Wart_COL)
   
 if (sum(is.na(points))>0) {
   points <- event_data("plotly_selected", source = source)
   points <- points$pointNumber
 }
   
   
  if (is.null(points)) {
   p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
  } else { 
    
  
  dataToPlot_temp <- dataToPlot %>%
    filter(LP %in% points) %>%
    select(CKK) 
  
  dataToPlot <- YM_ALL_WSK %>%
    filter(CKK %in% dataToPlot_temp$CKK) %>%
    mutate(WCSN_ALL_DIFF = WCSN_ALL - (!!sym_Wart_col)) %>%
    group_by(YMD) %>%
    summarise(WCSN_ALL_DIFF = sum(WCSN_ALL_DIFF, na.rm = T),
              (!!sym_Wart_col) := sum((!!sym_Wart_col), na.rm = T))
  
  p <- plot_ly(
      dataToPlot,
      x = ~ YMD,
      y = ~ WCSN_ALL_DIFF,
      type = 'bar',
      name = name1
    ) %>%
    add_trace(y = ~dataToPlot[[Wart_COL]], name = name2) %>%
    layout(yaxis = list(title = 'Wartość w zł'), barmode = 'stack') %>%
    config(displayModeBar = F)
  }
  return(p)
  
}

PLN <- dollar_format(suffix = "zł", prefix = "", big.mark = ",")

## Datatable output dla danych zaznaczonych z ScatterPlotly ####
data_table_plotly <- function(dataToShow = dataToPlot_pseudo(),
                             source = "pseudo_scatter",
                             points = NA,
                             Wart_COL = "WCSN_PSEUDO",
                             WSK_COL = "WSK_PSEUDO"
                             ) {
  
  sym_Wart_col <- rlang::sym(Wart_COL)
  sym_WSK_col <- rlang::sym(WSK_COL)
  
  if (sum(is.na(points))>0) {
    points <- event_data("plotly_selected", source = source)
    points <- points$pointNumber
  }
  
  if (is.null(points)) {
   p <- tibble(Zaznaczone = "Brak zaznaczonych punktów", Instrukcja = "Zaznacz punkty na wykresie powyżej.") 
  } else { 
    
    dataToShow %>%
      filter(LP %in% points) %>%
      mutate(
        WCSN_ALL = PLN(round(WCSN_ALL)),
        (!!sym_Wart_col) := PLN(round((!!sym_Wart_col))),
        (!!sym_WSK_col) := percent((!!sym_WSK_col))
             ) %>%
      select(PLATNIK, CKK, everything(), -LP) -> p
    
  
  }
  return(p)
  
}

## Statystyki data table WCSN_ALL > 0!!!!##### 
data_table_stats <- function(
  dataToShow = YM_ALL_WSK,
  input_data_start =  "2016-06-01",
  input_data_koniec = "2018-06-30"
) {
  
  dataToShow %>%
    mutate(YMD = ymd(paste0(YM, "-01"))) %>%
    filter(YMD >= ymd(input_data_start),
           YMD <= ymd(input_data_koniec)) %>%
    group_by(CKK) %>%
    summarise_if(is.numeric, sum, na.rm = T) %>%
    filter(WCSN_ALL > 0) %>%
    mutate(WSK_PSEUDO = WCSN_PSEUDO/WCSN_ALL) %>%
    mutate(WSK_DEF = WCSN_DEF/WCSN_ALL) %>%
    mutate(WSK_REF = WCSN_REF/WCSN_ALL) %>%
    mutate(WSK_PSEUDO = ifelse(is.na(WSK_PSEUDO), 0, WSK_PSEUDO)) %>%
    mutate(WSK_PSEUDO = ifelse(WSK_PSEUDO > 1, 1, WSK_PSEUDO),
           WSK_PSEUDO = ifelse(WSK_PSEUDO < 0, 0, WSK_PSEUDO)) %>%
    mutate(WSK_DEF = ifelse(is.na(WSK_DEF), 0, WSK_DEF)) %>%
    mutate(WSK_DEF = ifelse(WSK_DEF > 1, 1, WSK_DEF),
           WSK_DEF = ifelse(WSK_DEF < 0, 0, WSK_DEF)) %>%
    mutate(WSK_REF = ifelse(is.na(WSK_REF), 0, WSK_REF)) %>%
    mutate(WSK_REF = ifelse(WSK_REF > 1, 1, WSK_REF),
           WSK_REF = ifelse(WSK_REF < 0, 0, WSK_REF)) -> data
  
  data %>%
    summarise_if(is.numeric, mean, na.rm = T) %>%
    mutate(Metric = "Mean: ",
           Opis = "Średnia") %>%
    select(Metric, everything()) -> Mean
  
  data %>%
    summarise_if(is.numeric, min, na.rm = T) %>%
    mutate(Metric = "Min: ",
           Opis = "Minimum") %>%
    select(Metric, everything()) -> Min
  
  data %>%
    summarise_if(is.numeric, max, na.rm = T) %>%
    mutate(Metric = "Max: ",
           Opis = "Maksimum") %>%
    select(Metric, everything()) -> Max
  
  data %>%
    summarise_if(is.numeric, median, na.rm = T) %>%
    mutate(Metric = "Median: ",
           Opis = "Mediana") %>%
    select(Metric, everything()) -> Median
  
  data %>%
    summarise_if(is.numeric, quantile, probs = 0.025, na.rm = T) %>%
    mutate(Metric = "Q025: ",
           Opis = "Kwantyl 2.5%") %>%
    select(Metric, everything()) -> Q025
  
  data %>%
    summarise_if(is.numeric, quantile, probs = 0.25, na.rm = T) %>%
    mutate(Metric = "Q25: ",
           Opis = "Kwantyl 25%") %>%
    select(Metric, everything()) -> Q25
  
  data %>%
    summarise_if(is.numeric, quantile, probs = 0.75, na.rm = T) %>%
    mutate(Metric = "Q75: ",
           Opis = "Kwantyl 75%") %>%
    select(Metric, everything()) -> Q75
  
  data %>%
    summarise_if(is.numeric, quantile, probs = 0.975, na.rm = T) %>%
    mutate(Metric = "Q975: ",
           Opis = "Kwantyl 97.5%") %>%
    select(Metric, everything()) -> Q975
  
  data %>%
    summarise_if(is.numeric, quantile, probs = 0.99, na.rm = T) %>%
    mutate(Metric = "Q99: ",
           Opis = "Kwantyl 99%") %>%
    select(Metric, everything()) -> Q99
  
  ret <- bind_rows(Mean, Min, Max, Median, Q025, Q25, Q75, Q975, Q99) %>%
    mutate_at(vars(matches("^WSK")), percent) %>%
    mutate_at(vars(matches("^WCSN")), PLN) %>%
    select(Metric, Opis, everything()) %>%
    select(-CKK)
   
  
  return(ret)
  
 #return(iris)
}



ObliczOdleglosciOdPunktu <- function(data, id) {
  lat_id <- data[data$ID == id,]$lat
  lng_id <- data[data$ID == id,]$lng
  temp_dist <- vector(mode = "numeric", length = nrow(data))
  ret <- tibble(ID = id,
                ID_2 = data$ID,
                Dist_in_meters = NA)
  ST <- Sys.time()
  for (i in 1:nrow(data)) {
    temp_dist[i] <-
      distHaversine(p1 = c(lat_id, lng_id),
                    p2 = c(data$lat[i], data$lng[i]))
    
  }
  ret$Dist_in_meters <- temp_dist
  return(ret)
}

data <- Mam_GPS_temp
ID <- 16571
distInMeters <- 1000
WystawNajblizszeCKKdlaPunktu <- function(data, ID, distInMeters) {
  
  if (is.null(ID)) {
    ID <- 16571
  }
  
  lat_id <- data[data$ID == ID,]$lat
  lng_id <- data[data$ID == ID,]$lng
  
  IloscSekundGEO <- distInMeters/10
  
  lat_lim <- c(lat_id -  IloscSekundGEO/3600, lat_id +  IloscSekundGEO/3600)
  lng_lim <- c(lng_id -  IloscSekundGEO/3600, lng_id +  IloscSekundGEO/3600)
  
  data %>%
    filter(lng <= lng_lim[2], lng >= lng_lim[1]) %>%
    filter(lat <= lat_lim[2], lat >= lat_lim[1]) -> temp

  disty <- ObliczOdleglosciOdPunktu(temp, ID)
  
  
  temp %>%
    left_join(select(disty, ID_2, Dist_in_meters), by = c("ID"="ID_2")) %>%
    filter(Dist_in_meters < distInMeters) -> ret
  
  return(ret)  
}



WystawNajblizszeCKKdlaPunktu(data = Mam_GPS_temp, ID = 16571, distInMeters = 1000)
