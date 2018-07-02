
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(RSQLite)
library(lubridate)
library(data.table)


myDB_YM <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus_YM.sqlite")
myDB <- dbConnect(RSQLite::SQLite(), "C:/Users/msiwik/Desktop/FOLDER R/Analiza_Prepeparatow/Dane/Consensus.sqlite") 
YM_ALL_WSK <- dbSendQuery(myDB_YM, "SELECT * FROM WSK_YM") %>% fetch()
BAZA_CKK <- dbSendQuery(myDB, "SELECT * FROM tab3") %>% fetch()


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