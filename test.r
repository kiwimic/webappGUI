df <- data.frame( 
  color = c("blue", "black", "blue", "blue", "black"), 
  letters =c("C", "C", "C", "D", "E"),
  value = 1:5)


filter_col <- function(df, col_name_as_string, col_name_2, val){
  col_name <- rlang::sym(col_name_as_string)
  col_name2 <- rlang::sym(col_name_2)
  df %>% filter((!!col_name) == val) %>%
    mutate((!!col_name2) := ifelse(!!col_name2 < 3, !!col_name2 * 5, !!col_name2))
}
filter_col(df, 'letters','value', 'C')

filter(df, color == "blue")

var <- quo(color)
filter(df, UQ(var) == "blue")
#   color value
#1  blue     1
#2  blue     3
#3  blue     4
#Due to operator precedence, we may require () to wrap around !!
  
  filter(df, (!!var) == "blue")
#   color value
#1  blue     1
#2  blue     3
#3  blue     4
  
BAZA_REF %>%
  semi_join(BAZA_DEF, by = "ID") -> def_w_ref

BAZA_DEF %>%
  anti_join(BAZA_REF, by = "ID") -> def_nie_ref
  
  
testFUN <- function(nameOfVar, value) {
  var <- enquo(nameOfVar)
  var2 <- quo_name(nameOfVar)
  value_enqo <- enquo(value)
  value_quo <- quo(value)
 
  df %>%
    filter((!! var) == (!!value_enqo)) -> temp
  
  df %>%
    filter(var2 == (!!value_enqo)) -> temp2
  
  df %>%
    filter((!! var) == (!! value_quo)) -> temp3
  

  return(list(temp, temp2, temp3))   
}

testFUN2 <- function(nameOfVar, value) {
  var_quo_name <- quo_name(nameOfVar)
  var_quo <- quo(nameOfVar)
  var_sym <- sym(nameOfVar)
   
   # 
   # df %>%
   #  filter((!! var_quo) == value) -> temp
   # 
   df %>%
    filter((!!var_sym) == value) -> temp2
  # 
  # df %>%
  #   filter((!! var) == value) -> temp3
  # 
  
 return(temp2)
}

TEST <- "color"
testFUN2("color", "blue")

my_summarise <- function(df, group_var) {
  quo_group_var <- quo(group_var)
  print(quo_group_var)
  
  df %>%
    group_by(!! quo_group_var) %>%
    summarise(a = mean(a))
}


YM_DEF <- tbl(myDB_YM, "YM_DEF")
YM_WSK <- tbl(myDB_YM, "YM_ALL_CKT") %>% collect()
YM_DEF <- YM_DEF %>% collect()

YM_DEF %>%
  group_by(YM) %>%
  summarise(WCSN_def = sum(WCSN)) -> temp

YM_DEF %>%
  filter(grepl(YM, pattern = "2018")) %>%
  group_by(CKK) %>%
  summarise(WCSN_def = sum(WCSN, na.rm = T)) %>%
  arrange(desc(WCSN_def)) -> temp2

YM_WSK %>%
  group_by(YM) %>%
  summarise(WCSN_all = sum(WCSN)) %>%
  full_join(temp, by = "YM") %>%
  mutate(WSK = WCSN_def/WCSN_all)-> SPR

ZapiszOdleglosciOdPunktu <- function(data, id, dir, type) {
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
  print(Sys.time() - ST)
  ret$Dist_in_meters <- temp_dist
  
  if (type == ".csv") {
    write.csv2(ret, file = paste0(dir, "/", id, ".csv"))
  } else {
    save(ret, file = paste0(dir, "/", id, ".Rdata"))
  }
}

k <- 0
for (id in unique(Mam_GPS$ID)) {
  if(!file.exists(paste0("Dane/GPS/", id, ".csv"))) {
    ZapiszOdleglosciOdPunktu(data = Mam_GPS, id = id, dir = "Dane/GPS/", type = ".csv")
    k <- k+1
  } else {
    print(paste0("Plik ", id, " istnieje."))
  }
  
  print(k)
}




DaneDuzaBaza_tab1 <- tbl(myDB, "tab1") %>%
  filter(CKK ==  16540) %>%
  group_by(CKK, CKT, DATA_ZAFAKTUROWANIA) %>%
  summarise(WCSN = sum(WCSN, na.rm = T),
            WCSN_PO_RABATACH = sum(WCSN_PO_RABATACH, na.rm = T),
            ILOSC = sum(ILOSC, na.rm = T),
            LICZBA_WIERSZY = n()) %>%
  collect() %>%
  ungroup() %>%
  mutate(DATA_ZAFAKTUROWANIA = as.POSIXct(DATA_ZAFAKTUROWANIA, origin = "1970-01-01 00:00:00 UTC"),
         YM = str_sub(as.character(DATA_ZAFAKTUROWANIA),1, 7))




YM_ALL_WSK %>%
  mutate(WSK_PSEUDO = ifelse(is.na(WSK_PSEUDO), 0, WSK_PSEUDO)) %>%
  mutate(WSK_PSEUDO = ifelse(WSK_PSEUDO > 1, 1, WSK_PSEUDO),
         WSK_PSEUDO = ifelse(WSK_PSEUDO < 0, 0, WSK_PSEUDO)) -> dane

lista <- vector("list", length = length(unique(dane$YM)))
k <- 1
for (i in unique(dane$YM)) {
  temp_data <- dane %>%
    filter(YM == i)    
  
  temp <- tibble(YM = i,
                 values = seq(0, 1, 0.01), 
                 quants = quantile(temp_data$WSK_PSEUDO, probs = seq(0, 1, 0.01)))
  lista[[k]] <- temp 
  k <- k+1
}

test <- bind_rows(lista)

plot_ly(test, x = ~values, y = ~quants, color = ~YM)
