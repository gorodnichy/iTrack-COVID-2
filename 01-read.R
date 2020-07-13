# R101
# 01-read.R
# https://github.com/IVI-M/R-Ottawa/new/master/r101
# Last updated: 22 May 2020

# 0. General libraries and functions ----

# print(sessionInfo())
# source("000-common.R")

if (T) { 
  options(scipen=999); #remove scientific notation
  library(data.table)
  options(datatable.print.class=TRUE)
  library(magrittr)
  library(lubridate)
  library(ggplot2)
  library(plotly)
  library(DT)
}

STR_TOTAL <- "...COMBINED..."

# 0.1 My functions ----

covid.reduceToTopNCitiesToday <- function(dt0, N=5) {
  
  dateMax <- dt0$date %>% max
  dt <- dt0[date==dateMax][order(-confirmed)][1: min(N, nrow(dt))];
  dt
  topNcities <- dt$city ; 
  topNcities
  dt <- dt0[ city %in% topNcities];
  dt
}


readGeo <- function() {
  dt <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") 
  dt[c(1,3)]
  dt[c(1:4)]
  dt[.N]
  dt[c(1:2, (.N-1):.N), 1:3]
  cols <- c("UID"  , "iso2" ,   "iso3"  ,   "code3", "FIPS", "Combined_Key")
  dt [ ,(cols):=NULL] ;  
  dt %>% setnames(c("city", "state", "country", "lat", "lng", "population")) 
  
  dt
  dt[ state == "", state:=STR_TOTAL]
  dt[ city == "", city:=STR_TOTAL]
  
  setcolorder(dt, c( "country" , "state" , "city" , "lat", "lng" , "population"  ) )
  dt
}

readCovidUS <- function() {
  # read confirmed data
  dt <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", stringsAsFactors = T )
  dt %>% names
  dt[.N]
  dt[.N, 1:12]
  dt[c(1:2, (.N-1):.N), 1:12]
  cols <- c ( 1:5, 8:11 ) ;
  dt [ ,(cols):=NULL] ;   
  dtUSc <- dt %>% melt(id=1:2);   
  dtUSc[c(1,.N)]
  setnames(dtUSc, c("city", "state", "date", "confirmed"));dtUSc [c(1,.N)]
  setkeyv (dtUSc, c("date", "state", "city") )
  dtUSc
  
  # read deaths data  
  dt <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", stringsAsFactors = T )
  # ...  
  cols <- c ( 1:5, 8:12 ) ;
  dt [ ,(cols):=NULL] ;   
  dtUSd <- dt %>% melt(id=1:2);   
  setnames(dtUSd, c("city", "state", "date", "deaths"))
  setkeyv (dtUSd, c("date", "state", "city") )
  dtUSd
  # merge two data sets;  
  
  dtUS <- merge(dtUSc, dtUSd, by =  c("city", "state", "date") )
  dtUS
  # OR
  #  dtUS <- dtUSc [dtUSd]
  
  dtUS [, date := as.character(date) %>% mdy ]
  
  dtUS$country <- "US"
  dtUS$recovered <- NA
  dtUS[ city == "", city:=STR_TOTAL]
  setcolorder(dtUS, c("date", "country", "state", "city", "confirmed", "deaths", "recovered" ))
  
  
  dtUS %>% summary
  
  dateMax <- dtUS$date %>% max()
  dtUS$state %>% unique
  dtUS [state=='New York']$city %>% unique()
  
  dtUS[date==dateMax]
  dtUS[confirmed > 10000]
  dtUS[confirmed > 100000 & state=='New York']
  
  
  return (dtUS)
}



readCovidOttawa <- function() {
  
  library(readxl)
  
  # read_excel reads both xls and xlsx files
  read_excel("my-old-spreadsheet.xls")
  read_excel("my-new-spreadsheet.xlsx")
  
  # Specify sheet with a number or name
  read_excel("my-spreadsheet.xls", sheet = "data")
  read_excel("my-spreadsheet.xls", sheet = 2)
  dtOt <- read_excel("https://can01.safelinks.protection.outlook.com/?url=https%3A%2F%2Fwww.arcgis.com%2Fsharing%2Frest%2Fcontent%2Fitems%2F235c68c04008424bbf2dc69ee8cdd941%2Fdata&data=02%7C01%7Ccatherine.millar%40ottawa.ca%7C14abab7de8bd4e54ce5a08d7fccfdb96%7Cdfcc033ddf874c6ea1b88eaa73f1b72e%7C0%7C0%7C637255841974360463&sdata=O6qYdDFJOguJx1VionMUXu4%2FmiLQvyMy68Gq7nR8MsE%3D&reserved=0,sheet = 1")  
  
  dtOt <- fread("https://can01.safelinks.protection.outlook.com/?url=https%3A%2F%2Fwww.arcgis.com%2Fsharing%2Frest%2Fcontent%2Fitems%2F235c68c04008424bbf2dc69ee8cdd941%2Fdata&data=02%7C01%7Ccatherine.millar%40ottawa.ca%7C14abab7de8bd4e54ce5a08d7fccfdb96%7Cdfcc033ddf874c6ea1b88eaa73f1b72e%7C0%7C0%7C637255841974360463&sdata=O6qYdDFJOguJx1VionMUXu4%2FmiLQvyMy68Gq7nR8MsE%3D&reserved=0")  
  
  
  str <- "https://can01.safelinks.protection.outlook.com/?url=https%3A%2F%2Fwww.arcgis.com%2Fsharing%2Frest%2Fcontent%2Fitems%2F235c68c04008424bbf2dc69ee8cdd941%2Fdata&data=02%7C01%7Ccatherine.millar%40ottawa.ca%7C14abab7de8bd4e54ce5a08d7fccfdb96%7Cdfcc033ddf874c6ea1b88eaa73f1b72e%7C0%7C0%7C637255841974360463&sdata=O6qYdDFJOguJx1VionMUXu4%2FmiLQvyMy68Gq7nR8MsE%3D&reserved=0"
  
  str1 <-"https://ago-item-storage.s3.us-east-1.amazonaws.com/235c68c04008424bbf2dc69ee8cdd941/COVID19_MapPublic_DataTables_EN.xlsx?X-Amz-Security-Token=IQoJb3JpZ2luX2VjENL%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIQCkK4wPlpygqlJM1qTJjN2Uqeu7iYDbaw2O4GNooqS4IAIgPDcatIrg5%2Fr1SBo3LyG82rWHbnJTLq7LvR29GRuSTFIqtAMIOxAAGgw2MDQ3NTgxMDI2NjUiDNw%2BhBeixG1dPYZkliqRA7A6LqeOz4lhXxzpAdtJipv4AYHxy6NmxS1S0AaqXFZC48%2BWdUMq%2F997U2pehvW%2FRfIQHuPn8F%2Bclh7glo2OAhRQ9%2FbWRxuGwm88AwiOCsK3MgizfqOYvpJubfClpkEMsuvhd%2B%2FfA41ATNsrc9wY6W3EpVqozj1P5iQOYomq34lWbOcwaGvaoo1sJ%2FKpcLkftw9nKyw94MqzOmMGVyNLKJv%2FcICXmo2uDwvd3syAdOFuPnc1Ncj25ox32%2FAD8Ht4kBc%2BngOZQsxybg%2F9icu61vYp2IkDkViECu%2F7tNQG8vl34XH80lHiiPnnw9Ut4bsS1FWELNGMcnpV2gKjK270Qpdp5hxZn6bU%2FS6E%2F25RL9YRomNZzobw0Jx23jQoKPBlgJ0FQ4wF4vev5L%2FzcUu4m8%2BqRu7IP3hk5JYg02l5qRCH%2FKZoUZgvKXnUeFLaMffPCT5%2B0nemlZcYWq11W%2BSPkS3F8ujLkVqh%2B64zA7ZZsO8G6XzbZGGS1TcG%2Fn3LpdUo4Qij9C8beXA4SmGVsT%2BvTsTbMMa10fYFOusBfQcUqUo6NIJzVUoOSj17uLf%2Bz%2F3Ol%2Bx8X0EFxruwoiYvfVPFl4lefj0MZ4Ds%2BaocYzTwMURC0qQ8QVqA62ue28igbxhEayEpNA4hcD5Y2%2BPiKVVnyXzI9UYt0oJDKuocL11D9cEj1BCJ7sqtUXxs%2B2W9mghGUrxt3PcoVixa3VluegeFgJGGs9De30HLxn9UKXYC1VPuDQyghT2drzTUfY4icBMWp3Q48wCo1B4m8VxY7txS2iGhHsfnFxJekx%2BN%2F2%2BzkQahgYP2sH7a9M8PJfbHGbR3UlfkZQNXM3Y%2F3nkL2neKSDO%2BVBgn8Q%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20200601T025631Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAYZTTEKKEQHJ4SM6Q%2F20200601%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=5e1dd4f5e57dbfd173a51d389f5172d22d8c07f95b67b305756fa1cf75795f69"
  
  
  str2 <- "https://docs.google.com/spreadsheets/d/1qhNWZIIrJyMkHvL8bSnsgF64B0-_YxDTypdPsJC55b0/edit?usp=sharing"
  
  require(gdata)
  df = read.xls (str1, sheet = 1, header = TRUE)
  
  read_excel(str1, sheet = 1)
}

readCovidOntario <- function() {
  
  str <- "https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv"
  
  
  dt <-fread (str)
  
  
  dt %>% summary
}


readAll <- function() {
  
}


TEST_ME <- function() {
  
  #  1.0 Reading data   ----
  
  dtGeo <- readGeo()
  dtGeo
  dtGeo[city == 'New York'] # No such city???
  dtGeo[state == 'New York'] # Lets find it
  dtGeo[state == 'New York']$city
  
  # lets fix it !
  dtGeo[city == 'New York City', city:='New York']
  
  dtUS <- readCovidUS()
  dtUS
  dateMax <- dtUS$date %>% max; dateMax
  
  dtUS$recovered <- NULL
  
  #  1.1 New-York example   ----
  
  dtUS[state == 'New York']$city %>% unique
  
  city <- "Suffolk"
  city <- "New York"
  
  dtUS[city == 'Suffolk'][date==dateMax]
  dtUS[city == 'New York'][date==dateMax]
  dtUS[state == 'New York' & date==dateMax]
  
  # . Find top 3  cities in New York ----
  
  
  dt0 <-  dtUS[state == 'New York']
  dt0$city %>% unique() %>% length
  
  dt <- dt0[date==dateMax][order(-confirmed)][1:3];
  dt
  topNcities <- dt$city ; 
  topNcities
  dt <- dt0[ city %in% topNcities];
  dt
  
  # copied to covid.reduceToTopNCitiesToday <- function(dt0, N=5) 
  
  # Now the same using the function
  dt0 <- dt0 %>% covid.reduceToTopNCitiesToday()
  dt0$city %>% unique()
  dt <- dt0
  
  
  #  1.2 Merging Geo with Stats data   ----
  
  dtAll <- dtGeo [dt, on=c("country" , "state" , "city")];dtAll
  
  # 2. Plotting
  
  g <- dtAll[city == 'New York'][ date > dateMax - 30] %>% 
    ggplot() + 
    geom_line(aes(date, confirmed), col="orange") +
    geom_line(aes(date, deaths), col="red") 
  g
  
  dtAll[ date > dateMax - 30] %>% 
    ggplot() + 
    #facet_wrap( city ~ .) +
    # facet_wrap( reorder(city, lng) ~ .) +
    facet_wrap( reorder(city, -confirmed) ~ ., scales="free") +
    # facet_wrap( reorder(city, state) ~ .) +
    geom_line(aes(date, confirmed), col="orange") +
    geom_line(aes(date, deaths), col="red") +
    labs(
      title= paste0("Infected per day"),
      subtitle=paste0("State: ", 'New York', ". Date: ", dateMax),  
      caption=paste0(
        "Cities are sorted by location\n", 
        "Data source: Johns Hopkins University U.S. Coronavirus database\n", 
        "Generated by R Ottawa"
      )
    )
  
  
  ggsave("New York.png")
  
  # 1.3 Compute some stats: Totals
  
  
  dtAll[, sum(confirmed),  by=city][]
  dtAll[, sum(deaths),  by=city][]
  
  dtAll[ date > dateMax - 3][, mean(confirmed),  by=city][]
  dtAll[ date > dateMax - 3][, mean(deaths),  by=city][]
  
  # Better way
  
  cols <- c("confirmed", "deaths")
  colsTotal <- paste0(cols, "Total"); colsTotal
  colsSpeed <- paste0(cols, "Speed"); colsSpeed
  
  dtAll[, lapply(.SD, sum), .SDcols=cols]
  
  my.filter <- function(vector) {
    # ( vector[length(vector)] + vector[length(vector)-1] ) / 2
    ( vector[length(vector)] + vector[max(1, length(vector)-1) ] ) / 2
  }
  my.filter (1:10)
  
  dtAll[, lapply(.SD, my.filter), .SDcols=cols]
  
  
  # . By city ----
  
  dtAll[, lapply(.SD, sum), by=city, .SDcols=cols] 
  
  dt2 <- dtAll[, lapply(.SD, sum), by=city, .SDcols=cols] %>% setnames(cols, colsTotal)
  
  dt3 <- dtAll[, lapply(.SD, my.filter), by=city, .SDcols=cols] %>% setnames(cols, colsSpeed)
  
  dt2[dt3, on="city"]
  
  
  # .. Total for each day so we can plot it ----
  
  dtAll[, (colsTotal):= lapply(.SD, cumsum), .SDcols=cols]
  
  dtAll[ date > dateMax - 30] %>% 
    ggplot() + 
    #facet_wrap( city ~ .) +
    # facet_wrap( reorder(city, lng) ~ .) +
    # facet_wrap( reorder(city, lng) ~ ., scales="free") +
    facet_wrap( reorder(city, -confirmed) ~ .) +
    # facet_wrap( reorder(city, state) ~ .) +
    geom_line(aes(date, confirmedTotal), col="orange") +
    geom_line(aes(date, deathsTotal), col="red") +
    labs(
      title= paste0("Total infected"),
      subtitle=paste0("State: ", 'New York', ". Date: ", dateMax), 
      caption=paste0(
        "Cities are sorted by location\n", 
        "Data source: Johns Hopkins University U.S. Coronavirus database\n", 
        "Generated by R Ottawa"
      )
    )
  
  
}


if (F) {
  "
Other databases, dashboards and COVID-19 trackers for Canada
Official: www150.statcan.gc.ca: Table: 13-10-0766-01
Data: zip
https://art-bd.shinyapps.io/covid19canada (https://github.com/ishaberry/Covid19Canada)
https://art-bd.shinyapps.io/Ontario_Health_Unit_IDEA_model/
https://experience.arcgis.com/experience/2f1a13ca0b29422f9b34660f0b705043/
https://www.covid-19canada.com/
https://covid19tracker.ca/
http://gilchrist.ca/jeff/COVID-19/index.html ( http://gilchrist.ca/jeff/COVID-19/Ottawa.html)
https://covid19canada.herokuapp.com/
Ottawa & Ontario
https://www.ottawapublichealth.ca/en/reports-research-and-statistics/la-maladie-coronavirus-covid-19.aspx
Data: xls
https://613covid.ca/
https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario
Data: csv
  "
  
}