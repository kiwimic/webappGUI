
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(RSQLite)
library(lubridate)
library(rlang) ## rlang::sym
library(data.table)

myDB_YM <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus_YM.sqlite")
myDB <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus.sqlite") 
YM_ALL_WSK <- dbSendQuery(myDB_YM, "SELECT * FROM WSK_YM") %>% fetch() %>% mutate(YMD = ymd(paste0(YM, "-01")))
BAZA_CKK <- dbSendQuery(myDB, "SELECT * FROM tab3") %>% fetch()

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


tbl(myDB, "tab1") %>%
     filter(CKK == 123812) %>%
   group_by(CKK, CKT, DATA_ZAFAKTUROWANIA) %>%
   summarise(WCSN = sum(WCSN, na.rm = T)) %>%
   collect() %>%
   mutate(YMD_HMS = as.POSIXct(DATA_ZAFAKTUROWANIA, origin = "1970-01-01 00:00:00 UTC"))-> test_123812


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

DaneDoScatter(dane = YM_ALL_WSK,
              input_data_start = "2018-01-01",
              input_data_koniec = "2018-11-01",
              Wart_COL = "WCSN_DEF",
              WSK_COL = "WSK_DEF"
              ) -> test
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
  mutate(LP = (1:n())-1) -> ret
  
  return(ret)
  # d1 <- input$dateRange[1]
  # d2 <- input$dateRange[2]  
  # p3 <- input$Proc
  # p4 <- input$Wart
}


ScatterPlotly <- function() {
  
}
