# source("covid-read.R")
# Author: Dmitry Gorodnichy (dmitry@gorodnichy.ca)

# setwd("/home/dmitry/GitHub/iTrack-covid-priv")
source("dt.R")  
library(flexdashboard)


# ****************************************************** -------

readGeoCa <- function() {
  
  if (T) {
    dtGeoCa <- readRDS("dtCitiesCa-fromUofT+Geo.Rds")   
    return(dtGeoCa)
  }
  
  dtCaCitiesInUofT <- fread("dtCitiesCa-fromUofT.csv", fill=T, header=T)   %>% # extracted in readUT()
    setnames("health_region", "city") %>%
    setnames( "province", "state") %>%
    dt.replaceAB("state", "BC", "British Columbia"  ) %>% 
    dt.replaceAB("state",  "NWT", "Northwest Territories" ) %>% 
    dt.replaceAB("state",  "NL", "Newfoundland and Labrador" ) %>% 
    dt.replaceAB("state", "PEI", "Prince Edward Island"  )
  dtCaCitiesInUofT[, city:=iconv(city,to="ASCII//TRANSLIT")]
  dtCaCitiesInUofT[, state:=iconv(state,to="ASCII//TRANSLIT")]
  
  dtCaCitiesInUofT <- dtCaCitiesInUofT[state!="Repatriated"]
  dtCaCitiesInUofT
  dtCaCitiesInUofT$state %>% unique()
  
  
  #   dtCaCitiesGeoPop <- fread("data/citiesCA.csv", stringsAsFactors=T);  # 
  dtCaCitiesGeoPop <- fread("https://simplemaps.com/static/data/country-cities/ca/ca.csv");  # 
  cols <- c(       "country"  ,  "iso2"   ,   "capital"     ,  "population_proper")
  dtCaCitiesGeoPop [ ,(cols):= NULL]
  dtCaCitiesGeoPop %>% setnames("lng", "lng")
  dtCaCitiesGeoPop
  setnames(dtCaCitiesGeoPop, "admin", "state")
#  setnames(dtCaCitiesGeoPop, "city", "admin2")
  dtCaCitiesGeoPop[, city:=iconv(city,to="ASCII//TRANSLIT")]
  dtCaCitiesGeoPop[, state:=iconv(state,to="ASCII//TRANSLIT")]
  dtCaCitiesGeoPop 
  dtCaCitiesGeoPop$state %>% unique()
  
  
  dtGeoCa <<- dtCaCitiesGeoPop %>% merge (dtCaCitiesInUofT, by=c("city", "state"), all = T) ; dtGeoCa
  
  dtGeoCa %>% setkey(state)
  dtGeoCa$state %>% unique
  for( st in dtGeoCa$state %>% unique)
    print(dtGeoCa[state==st][order(lat)])
  
  dtGeoCa %>% setkeyv(c("state", "city"))
  
  dtGeoCa$closest_city <- dtGeoCa$city
  
  addGeoFromClosestCity <- function(state, city, cities){
    
    dtR <- dtGeoCa[ state == state & city %in% cities]
    popR <- dtR$population %>% sum
    latR <- dtR$lat %>% mean(na.rm =T) %>% as.integer()
    lngR <- dtR$lng %>% mean(na.rm =T) %>% as.integer()
    
    dtGeoCa[state == state & city==city, ":="(
      population=popR, lat=latR, lng=lngR)]
  }
  
  
  addGeoFromClosestCity("Alberta", "Central", "Red Deer")
  addGeoFromClosestCity("Alberta", "North", "Fort McMurray")
  addGeoFromClosestCity("Alberta", "South", c("Lethbridge", "Medicine Hat"))
  
  # addNotReportedToCapital <- function(state, capital, city1="Not Reported") {
  # }
  # addNotReportedToCapital("Alberta", "Edmonton")
  
  
  saveRDS(dtGeoCa, "dtCitiesCa-fromUofT+Geo.Rds")
  return(dtGeoCa)
}


readGeo <- function() {
  if (T) {
    return(readRDS("dtCities-fromJHU.Rds"))
  }
  dtGeo <<- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") %>% 
    dt.rmcols (c("UID"  , "iso2" ,   "iso3"  ,   "code3", "FIPS", "Combined_Key"))  %>% 
    setnames(c("city", "state", "country", "lat", "lng", "population")) %>% lazy_dt() %>% 
    filter(state %ni% c ("Diamond Princess", "Grand Princess", "Recovered")) %>% 
    as.dt 
  
  #   dtGeo[state=="", state:="(National)"]
  
  #dtGeo[country=="Canada"]
  # dtGeo[admin2!=""]
  
  dtGeo[ state == "", state:="(National)", by=country]
  
  saveRDS(dtGeo, "dtCities-fromJHU.Rds")
  return(dtGeo)

}

# ****************************************************** -------

readCovidUS <- function() {
  
  # JHU  used for https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6
  # from https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
  #
  
  cols <- c ( "Lat"   ,  "Long"  ) ;
  # dtJHUc <- fread( "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" , stringsAsFactors=T)
  # dtJHUc [ ,(cols):=NULL] ; dtJHUc <- dtJHUc %>% melt(id=1:2); setnames(dtJHUc, c("state", "country", "date", "confirmed"))
  # dtJHUd <- fread( "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" , stringsAsFactors=T)
  # dtJHUd [ ,(cols):=NULL] ; dtJHUd <- dtJHUd %>% melt(id=1:2); setnames(dtJHUd, c("state", "country", "date", "deaths"))
  # dtJHUr <- fread( "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv" , stringsAsFactors=T)
  # (dtJHUr [ ,(cols):=NULL] %>%  melt(id=1:2) -> dtJHUr) %>%  setnames(c("state", "country", "date", "recovered"))
  # 
  # lapply (list(dtJHUc, dtJHUd, dtJHUr), setkeyv, c("state" ,  "country" ,   "date"))
  # dtJHU <<- dtJHUc [dtJHUd][dtJHUr]
  # 
  # rm(dtJHUc, dtJHUd, dtJHUr);
  # dtJHU [, date := as.character(date) %>% mdy ]
  
  #. US -----
  #
  cols <- c ( 1:5, 8:11 ) ;
  dtUSc <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  dtUSc [ ,(cols):=NULL] ; 
  dtUSc <- dtUSc %>% melt(id=1:2); 
  setnames(dtUSc, c("admin", "state", "date", "confirmed"))
  
  dtUSd <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  dtUSd [ ,(cols):=NULL] ; 
  dtUSd <- dtUSd %>% melt(id=1:2); 
  setnames(dtUSd, c("admin", "state", "date", "deaths"))
  
  #  dtUSr <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_US.csv")
  
  lapply (list(dtUSc, dtUSd), setkeyv, c("admin" ,  "state" ,   "date"))
  dtUS <<- dtUSc [dtUSd]
  
  rm(dtUSc, dtUSd);
  dtUS [, date := as.character(date) %>% mdy ]
  dtUS$date %>% max()
  dtUS
  
  
}

readCovidJHU <- function(coronavirus_data=NULL) {
  
  #used in [GitHub](https://github.com/AntoineSoetewey/coronavirus_dashboard){target="_blank"}.
  # library(coronavirus)
  # dtJHUa <- coronavirus %>%  data.table() # the same as below
  
  if (is.data.table(coronavirus_data)) {
    dtJHU <<- copy(coronavirus_data)
  } else {
    dtJHU  <<- fread("https://github.com/RamiKrispin/coronavirus-csv/raw/master/coronavirus_dataset.csv")
  }
  
  dtJHU <<- dtJHU %>% 
    dt.rmcols(c("Lat", "Long" )) %>%  
    setnames(c("state", "country", "date", "cases", "type")) %>% 
    dt.convert("date", ymd) %>% 
    setkeyv(c("state" ,  "country" ,   "date")) 
  
  dtJHU[ state == "", state:="(National)", by=country]
  
  dtJHU <<- dcast(dtJHU, date+country+state ~ type, value.var="cases") 
  
  # . clean Canada  ----
  
  cleanJHU.Canada()
}




cleanJHU.Canada <- function() {
  
  dtJHU[country=="Canada"] # NO PROVINCES are CSSEGISandData/COVID-19. !
  dtJHU[country == "Canada" & state == "(National)"]
  dtJHU[state == "Ontario"]
  dtJHU[state == "Quebec"] 
  
  # dtJHU[state %in% c("Recovered", "Diamond Princess", "Grand Princess")][type=="confirmed" & cases>0]
  # # 1: Grand Princess  Canada 2020-03-13     2 confirmed
  # # 2: Grand Princess  Canada 2020-03-17     6 confirmed
  # # 3: Grand Princess  Canada 2020-03-18     1 confirmed
  # # 4: Grand Princess  Canada 2020-03-20     1 confirmed
  # # 5: Grand Princess  Canada 2020-03-22     3 confirmed
  # dtJHU[state %in% c("Recovered", "Diamond Princess", "Grand Princess")][type=="death" & cases>0]
  # # 1: Diamond Princess  Canada 2020-03-22     1  death
  # dtJHU[state %in% c("Recovered", "Diamond Princess", "Grand Princess")][type=="recovered" & cases>0]
  # # Empty data.table 
  # #   dtJHU[state=="Recovered"]
  # dtJHU[state=="Recovered", .N, by=country] # Bug there! in Canada only:  Canada   158
  
  # fix bug
  dtJHU <<- dtJHU [state %ni% c("Recovered", "Diamond Princess", "Grand Princess") ]
  
  #  dtJHU[country == "Canada"][date > max(date) - 14][state=="(National)"]
  
  setkey(dtJHU, date)
  dtN2 <- dtJHU[country == "Canada" & state != "(National)" ,  
                lapply(.SD, sum, na.rm = T), by=c("date"),
                .SDcols= c("confirmed" ,"death" ,"recovered")]
  
  dtJHU[country == "Canada" & state == "(National)"]$confirmed <<- dtN2$confirmed
  dtJHU[country == "Canada" & state == "(National)"]$death <<- dtN2$death
  
  dtJHU[country == "Canada" & state == "(National)", confirmed:= dtN2$confirmed ] 
  dtJHU[country == "Canada" & state == "(National)", death := dtN2$death ]
  
  # if (F) {
  #   dtJHU[cases<0] # another bug ?
  #   dtJHU[cases<0, cases, by=c("country", "date", "type")]
  #   dtJHU$cases <<-  dtJHU$cases %>% abs() 
  #   
  #   dateNeg <-  dt0[recovered<0]$date
  #   negative <- dt0[recovered<0]$recovered
  #   dtJHU.dailytotals[date == dateNeg -1, recovered:=recovered+ negative/2]
  #   dtJHU.dailytotals[date == dateNeg, recovered:=recovered - negative/2]
  # }
  # 
}




readCovidUofT <- function () {
  
  strCovid <- "https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/"
  sheets <- c("Cases" ,"Mortality" ,"Recovered", "Testing" ,"Codebook")
  
  # i=1
  # strCovidCsv <- paste0(strCovid, "gviz/tq?tqx=out:csv&sheet=", sheets[i]); strCovidCsv
  # dt <- fread(strCovidCsv,header=T, stringsAsFactors=T)
  
  
  fOpen <- function(i, col1name) {
    strCovidCsv <- paste0(strCovid, "gviz/tq?tqx=out:csv&sheet=", sheets[i])
    dt <- fread(strCovidCsv, header=T, select= 1:15, stringsAsFactors = F)
    names (dt) <- c(col1name, names (dt) [-1] )
    return(dt)
  }
  
  fRemoveV <- function(dt) {
    str <- names(dt)
    cols <- str [ str_detect(str, regex("^V+")) ]
    dt [ , (cols):=NULL ]
    print(str)
    return(dt)
  }
  
  #1 ---
  dtCases <<- fOpen(1, "case_id")  %>% fRemoveV() %>%  lazy_dt() %>% select(1:12) %>% as.dt() 
  setnames(dtCases, "date_report", "date")
  dtCases [ , date := dmy(date)]
  # dtCases$travel_yn  <<- dtCases$travel_yn %>% as.factor()
  # dtCases[`locally_acquired`=="Close contact", locally_acquired:="Close Contact"]
  # dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) 
  
  #2 ---
  dtMortality  <<- fOpen(2,"death_id") %>% fRemoveV() %>%  lazy_dt() %>% select(c(1:9, 11)) %>%  as.dt() 
  setnames(dtMortality, "date_death_report", "date")
  dtMortality [ , date := dmy(date)]

  
  #
  #3 ---
  dtRecovered  <<- fOpen(3,"date_recovered") %>% fRemoveV() %>%  lazy_dt() %>% select(1:4) %>% as.dt() 
  setnames(dtRecovered, "date_recovered", "date")
  dtRecovered [ , date := dmy(date)]
  
  #4 ---
  dtTesting  <<- fOpen(4,"date_testing") %>% fRemoveV() %>%  lazy_dt() %>% select(1:4) %>% as.dt() 
  setnames(dtTesting, "date_testing", "date")
  dtTesting [ , date := dmy(date)]
  
  # #5 ---
  # dtCodeBook  <<- fOpen(5, "variable") %>% lazy_dt() %>% select(1:3) %>% slice(-(1:2)) %>% as.dt() 
  # setnames(dtCodeBook, c("Variable", "Description", "Label"))
  
  
  # dtCases %>% summary(30)
  # dtMortality %>%  summary
  # 
  # dtRecovered %>%  summary
  # dtTesting%>%  summary
  
  
  setnames(dtCases, "health_region", "city")
  setnames(dtMortality, "health_region", "city")
  # setnames(dtRecovered, "health_region", "city")
  setnames(dtCases, "province", "state")
  setnames(dtMortality, "province", "state")
  setnames(dtRecovered, "province", "state")
 }

# ****************************************************** -------

addDerivatives <- function (dt0, colCases, groupby, convolution_window=8, difference_window=1) {
  
  colTotal <- paste0(colCases, "Total")
  colSpeed <- paste0(colCases, "Speed") # DailyAve") # "Speed"
  colAccel <- paste0(colCases, "Accel") # "WeeklyDynamics") # "Accel." WeeklyAccel - change since last week in DailyAve
  # colSpeedAve <- paste0(colCases, "SpeedAve")
  colAccel. <- paste0(colCases, "Accel.")  # AccelAve
  colGrowth <- paste0(colCases, "Growth")  #
  colGrowth. <- paste0(colCases, "Growth.")  #
  # colAccelation <- paste0(colCases, "Acceleration") # Acecel2
  
  dt0[ ,  (colTotal) := cumsum(.SD),  by=c(groupby), .SDcols = colCases]
  dt0[ ,  (colSpeed) := frollmean(.SD, convolution_window, align = "right", fill=0),  by=c(groupby), .SDcols = colCases][, (colSpeed) := lapply(.SD, round, 1), .SDcols = colSpeed]
  
  dt0[ ,  (colSpeed) := lapply(.SD, function(x) ifelse (x<0,0,x) ), .SDcols = colSpeed]
  
  dt0[ ,  (colAccel) := .SD - shift(.SD,difference_window),  by=c(groupby), .SDcols = colSpeed]

  dt0[ ,  (colAccel.) := frollmean(.SD, convolution_window, align = "right", fill=0),  by=c(groupby), .SDcols = colAccel][, (colAccel.) := lapply(.SD, round, 2), .SDcols = colAccel.]
  
  
  # dN(T)/ N(T)
  # dt0[ ,  confirmedGrowth0 := confirmedTotal/confirmedSpeed]
  # dt0[ ,  recoveredGrowth0 := recoveredTotal/recoveredSpeed]
  # dt0[ ,  deathGrowth0 := deathTotal/deathSpeed]
  
  # dN(T+1) / dN(T)
  dt0[ ,  (colGrowth) := .SD / shift(.SD,difference_window),  by=c(groupby), .SDcols = colSpeed]
  
  dt0[ ,  (colGrowth.) := frollmean(.SD, convolution_window, align = "right", fill=0),  by=c(groupby), .SDcols = colGrowth][, (colGrowth.) := lapply(.SD, round, 2), .SDcols = colGrowth.]
  
  
}

#  . addAllMetrics ----  
# 
# addAllMetrics <- function(dt0) {
#   
#   # . add integrals, averages and derivatives ----
#   
#  # dt0 <<- dtGeo[, .(state, country , population)][dt0 , on=c("country", "state")]
#   
#   # groupby <- "state"
#   groupby <- c("state","country")
#   cols3 <- c("confirmed","death",  "recovered")
#   # cols4 <- c("C.", "D.", "R.")
#   # setnames(dt0, cols3, cols4)
#   
#   addDerivatives(dt0, cols3, groupby)[.N]
#   dt0 %>% names
# 
#    
#   return(dt0)
# }
# 

# . add Predicted ----

predictCovid <- function (x, t, v, a) {
  x1 <- x + v*t + a*t*t/2
  if (x1<0) x1 <- 0
  x1
}
predictCovid2 <- function (totalToday, days, dailySpeed, dailyAccel) {
  x1 <- x + v*t + a*t*t/2
  if (x1<0) x1 <- 0
  x1
}


getPredicted <- function (dt0) {
  
  
  colsPredicted1 <- paste0(cols, "Predicted1")
  dt0[ , (colsPredicted) := lapply(.SD, predictCovid, ), by=c("country", "state"), .SDcols=cols]
  
  
  t = 1
  c <- dt0[date==dt0$date %>% max] %$% predictCovid(confirmedTotal, 0, confirmedDailyAve, confirmedWeeklyDynamics)
  
  
  
  
}




getTodayMetric  <- function (dt0, metric, strMetrics) {
  
  
  # 
  # strMetrics = c(
  #   "New cases", # Absolute values"",
  #   "New cases per 1 million",
  #   'New cases (Percentation of population)',
  #   
  #   'Total cases',
  #   'Totals per 1 million',
  #   'Total cases (Percentation of population)',
  #   
  #   #  "Fraction of confirmed",
  #   #  "Percentage of confirmed",
  #   "Per Confirmed",
  #   "Total Per Confirmed",
  #   # "Ratio of cases",
  #   
  #   "Weekly daily average",
  #   "Weekly dynamics" # 10
  # )
  # 
  
  
  dtToday <<- dt0[date == dt0$date %>% max |
                 #  date == dt0$date %>% max - 30 |
                   date == dt0$date %>% max - 1 |
                   date == dt0$date %>% max - 7  ] [ 
                     , region:= paste0(country, " - ", state)]  %>% setcolorder("region")
  
  cols3 <- c("confirmed","death",  "recovered")
  cols2 <- c("country", "state", "date")

  # 
  # if  (metric == strMetrics[4]){
  #   dtToday1 <- dtToday[, c(cols2,paste0(cols3, "Total")), with=F] %>% setnames (c(cols2,cols3)); dtToday1
  #   #0 if (metric == strMetrics[5])
  #   #0  dtToday1 <- dtToday[, c(cols2,paste0(cols3, "TotalPer100K")), with=F] %>% setnames (c(cols2,cols3)); dtToday1
  #   #if (metric == strMetrics[6])
  #   #  dtToday1 <- dtToday[, c(cols2,paste0(cols3, "TotalPerPopulation")), with=F] %>% setnames (c(cols2,cols3)); dtToday1
  #   
  # }else if (metric == strMetrics[7]){
  #   dtToday1 <- dtToday[, c(cols2,paste0(cols3, "PerConfirmed")), with=F] %>% setnames (c(cols2,cols3)); dtToday1
  # }else if (metric == strMetrics[8]){
  #   dtToday1 <- dtToday[, c(cols2,paste0(cols3, "TotalPerConfirmed")), with=F] %>% setnames (c(cols2,cols3)); dtToday1
  #   
  # }else if (metric == strMetrics[9]){
  #   dtToday1 <-  dtToday[, c(cols2,paste0(cols3, "DailyAve")), with=F] %>% setnames (c(cols2,cols3)); dtToday1
  # }else if (metric == strMetrics[10]){
  #   dtToday1 <-  dtToday[, c(cols2,paste0(cols3, "WeeklyDynamics")), with=F] %>% setnames (c(cols2,cols3)); dtToday1
  # } else {
  #   # (metric == strMetrics[1]) {
  #   dtToday1 <- dtToday[, c(cols2, cols3), with=F]; dtToday1
  # } 
  # 
  # 
  # 
  return(dtToday1)
}




getMetricsToday2old <-  function() {  # (date)
  
  
  
  metric <- c("confirmed", "death", "recovered")
  cols <- c("state", "country")
  
  dtCountries1 <<- dtJHU[ 
    , .(
      total=sum(.SD), 
      today= as.integer(.SD[.N]+.SD[.N-1])/2,
      yesterday= as.integer(.SD[.N-2]+.SD[.N-1])/2,
      
    ), keyby = cols, .SDcol=metric]
  
  dtCountries1 <<- dtJHU[ 
    , .(
      total = sum(.SD), 
      today2 = as.integer(.SD[.N]+.SD[.N-1])/2,
      yesterday2 = as.integer(.SD[.N-2]+.SD[.N-1])/2,
      
    ), keyby = cols, .SDcol=metric]
  
  
  metric = "confirmed"
  
  dtCountries2 <<- dtJHU [ order(date)] [  
    , .(today= as.integer(.SD[.N]+.SD[.N-1])/2), keyby = cols, .SDcol=metric]
  dtCountries3 <<- dtJHU [ order(date)] [  
    , .(yesterday= as.integer(.SD[.N-2]+.SD[.N-1])/2), keyby = cols, .SDcol="confirmed"]
  
  a=0
  dtCountries4 <<- dtJHU [ order(date)] [  
    , .(DailyAveThisWeek = as.integer( (.SD[.N-a]+.SD[.N-1-a]+.SD[.N-2-a]+.SD[.N-3-a]+.SD[.N-4-a]+.SD[.N-5-a]+.SD[.N-6-a])/7 ) ), keyby = cols, .SDcol="confirmed"]; dtCountries4
  a=7
  dtCountries4b <<- dtJHU [ order(date)] [  
    , .(DailyAveLastWeek = as.integer( (.SD[.N-a]+.SD[.N-1-a]+.SD[.N-2-a]+.SD[.N-3-a]+.SD[.N-4-a]+.SD[.N-5-a]+.SD[.N-6-a])/7 ) ), keyby = cols, .SDcol="confirmed"]; dtCountries4b
  
  a=14
  dtCountries4c <<- dtJHU.dailytotals [ order(date)] [  
    , .(DailyAveLastWeek2 = as.integer( (.SD[.N-a]+.SD[.N-1-a]+.SD[.N-2-a]+.SD[.N-3-a]+.SD[.N-4-a]+.SD[.N-5-a]+.SD[.N-6-a])/7 ) ), keyby = cols, .SDcol="confirmed"]; dtCountries4b
  
  
  dtRegions <- dtCountries1[dtCountries2][dtCountries3][dtCountries4][dtCountries4b][dtCountries4c][
    , WeeklyDynamics:=DailyAveThisWeek - DailyAveLastWeek][
      ,  WeeklyDynamicsLastWeek:=DailyAveLastWeek - DailyAveLastWeek2]
  
  dtRegions[order(-WeeklyDynamics)][1:40]; 
  
  
  
  dtCountriesCases <- dtRegions[ , lapply(.SD, sum, na.rm=T), by=country, .SDcols = c("Cases", "today" ,"yesterday" ,"DailyAveThisWeek", "DailyAveLastWeek", "WeeklyDynamics")][order(-Cases)]; dtCountries[1:30]
  
  
  
  dtJHU.dailytotals  [, (cols):=NULL]
  
}


# ***************************** =====
testme <- function() {
  
  
  # >>>>   input    <<<< ----

  if (T) {
    input <- list()
    #input$date <- dtJHU$date %>% max
    input$country <- "Canada"
    input$state <- "(National)"
    input$state <- "Ontario"
    input$city <- "Ottawa"
    input$f = ""
    input$normalize = T
    input$convolution = 7
  }
  
  
  g <- ggplot()
  dtJHU <- dt0 <- dt00 <- dtToday <-  dtGeo <- data.table()
  dtCa <- dtCases <- dtMortality <- dtRecovered <- dtTesting <-  dtGeoCa <- data.table()
  
  dtGeo <-readGeo()
  #dtGeoCa <- readGeoCa()
  
  coronavirus  <- fread("https://github.com/RamiKrispin/coronavirus-csv/raw/master/coronavirus_dataset.csv") 
  dtJHU <-readCovidJHU(coronavirus) # reads dtJHU data readCovidJHU
  
  
  # 
  # dtGeo <-readGeo()
  # dtGeoCa[!is.na(N)]
  # dtGeoCa <- readGeoCa()

  dateMax <- dtJHU$date %>% max
  dtJHU[country == 'Canada', sum(confirmed),  by = state]
  
  
  groupby <- c("state","country")
  cols3 <- c("confirmed","death",  "recovered")
  dtJHU <- addDerivatives(dtJHU, cols3, groupby, input$convolution)
  # cols4 <- c("C.", "D.", "R.")
  # setnames(dt0, cols3, cols4)
  # cols3 <- cols4
  
  # dtUofT <- dtCases <- dtMortality <- dtRecovered <- dtTesting <- data.table()
  # readCovidUofT()
  
  
  dtCa <- dtCases <- dtMortality <- dtRecovered <- dtTesting <- data.table()
  dtCa <- readCovidUofT()
  
  dtUS <- readCovidUS()
  
  
  # r.dt0 <- reactive({ ----
  

  dt00 <<- dtJHU[country == 'Canada']
  # dt0 <<- dtJHU[country %in% 'Canada' & state !="(National)"]
  
  dt0 <<- dtJHU[ country == input$country & state == input$state ] 
  dt0 <<- dtGeo[, .(state, country , population)][dt0 , on=c("country", "state")]
  
  dateMax <- dt0$date %>% max
  dateMin <- dt0$date %>% min
  
  dt0[ date == dt0$date %>% max]
  
  #  . addAllMetrics ----  
  #  
  # dt0 <<- addAllMetrics(dt0)
  
  # . add integrals, averages and derivatives ----
  
  # dt0 <<- dtGeo[, .(state, country , population)][dt0 , on=c("country", "state")]
  

  
 # addDerivatives(dt0, cols3, groupby)[.N]
  

  
  dt0 %>% names
  
  
  
  # 
  #   
  # . add per Million  ----
  # 
  # 
  # 
  
  # . add per Million  ----
  
 # cols3 <- c("confirmed","death",  "recovered", "unrecovered" )
  # cols3 <- paste0(cols3, "Total")
  # 
  # colsPer100K <- paste0(cols3, "Per100K")
  # dt0[ , (colsPer100K):= lapply(.SD, function(x) {as.integer(x/population*1000000)}), .SDcols=cols3]
  # 
  # dt0 %>% names
  # 
  
  colsPer100K <- paste0(cols3, "Per100K")
  dt0[ , (colsPer100K):= lapply(.SD, function(x) {as.integer(x/population*1000000)}), by=c("country", "state"), .SDcols=cols3][.N]
  
  dt0 %>% names
  
  cols3total <- paste0(cols3, "Total")
  colsTotalPer100K <- paste0(cols3total, "Per100K")
  dt0[ , (colsTotalPer100K):= lapply(.SD, function(x) {as.integer(x/population*1000000)}), by=c("country", "state"), .SDcols=cols3total][.N]
  dt0 %>% names
  
  
  
  # # colsPerPopulation <- paste0(cols3, "PerPopulation")
  # # dt0[ , (colsPer100K):= lapply(.SD, function(x) {round(x/population*100,2)}), by=c("country", "state"), .SDcols=cols3][.N]
  # # dt0 %>% names
  # # 
  # # colsTotalPerPopulation <- paste0(cols3total, "PerPopulation")
  # # dt0 <- dtGeo[, .(state, country ,population)][dt0 , on=c("country", "state")]
  # # dt0[ , (colsTotalPer100K):= lapply(.SD, function(x) {round(x/population*100,2)}), by=c("country", "state"), .SDcols=cols3total][.N]
  # # dt0 %>% names
  # 
  
  
  
  #  . getTodayMetric ---- 
  
  dtToday <<- dt0[date == dateMax |
                        date == dateMax - 30 |
                        date == dateMax - 1 |
                        date == dateMax - 7  ]
  # dtToday <<- r.dt0()[date == input$date |
  #                   date == input$date - 30 |
  #                   date == input$date - 1 |
  #                   date == input$date - 7  ]
  dtToday[, region:= paste0(country, " - ", state)]; dtToday%>% setcolorder("region")
  
  dtToday[ , date:=as.character(date)]
  
  dtToday [date == as.character(dt0$date %>% max), date:="t"]
  dtToday [ date ==  as.character(dt0$date %>% max - 1) , date:="t-1"]
  dtToday [ date ==  as.character(dt0$date %>% max - 7)  , date:="t-7"]
  dtToday [ date ==  as.character(dt0$date %>% max - 30) , date:="t-30"]
  
  dtToday [, unrecovered := confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)]
  
  dtToday [, unrecoveredTotal := confirmedTotal - ifelse(is.na(recoveredTotal), 0, recoveredTotal) - ifelse(is.na(deathTotal), 0, deathTotal)]
  
  # dtToday [, unrecoveredPer100K :=
  
  # #  dtToday <<- getTodayMetric(dt0, input$date, input$metric)
  # 
  # dtToday <<- getTodayMetric  (dt0, strMetrics[9], strMetrics)
  # 
  
  
  # ... table, plot ----
  
  dtToday %>% knitr::kable()
  
  dtTodayHor <- dtToday %>% dcast(country + state ~ date, value.var= c("confirmed","death", "recovered"))
  dtTodayHor %>% setnames("country", "state", paste0("confirmed", 1:4), paste0("death", 1:4), paste0("recovered", 1:4))
  
  DT::datatable( dtToday, 
                 options = list(bPaginate = F) , 
                 #   options = list(bPaginate = T,      pageLength = 15) , 
                 filter="top"
  ) 
  
  # .... plot  ----
  
  
  # g <- ggplot(x, aes(reorder(variable, value), value))
  
  # ggplot(dtToday1) + facet_grid( . ~ region) +
  g <- ggplot(dtToday, aes(x=state))  + 
    coord_flip() + # facet_grid( . ~ date) +
    scale_fill_brewer(palette = "Greens", direction = 1) +   #scale_fill_grey(0.3, 0.9) +
    
    #geom_col(aes_string(y=strColLast), fill="Green", alpha=0.2) +  # for plotly
    # geom_col(aes_string(y=strColLast,  fill="Metric"), alpha=0.6) +
    # geom_col(aes_string(y="confirmed", size="date"), alpha=0.2, fill= "yellow", size=6) +
    # geom_col(aes_string(y="recovered"), alpha=0.2, fill= "green", size=4) +
    # geom_col(aes_string(y="death"), alpha=0.2, fill= "red", size=2) +
    # geom_point(aes(y=confirmed, size=as.character("date")), alpha=0.8, col="yellow", fill= "yellow") +
    # geom_point(aes(y=recovered, size=as.character("date")), alpha=0.8, col="green", fill= "green") +
    # geom_point(aes(y=death, size=as.character("date")), alpha=0.8, col="red", fill= "red") +
    # 
    # geom_point(aes_string(y="confirmed", size="date"), alpha=0.8, col="yellow") +
    # # geom_point(aes_string(y="recovered", size=as.character("date")), alpha=0.8, col="green", fill= "green") +
    # geom_point(aes_string(y="death", size="date"), alpha=0.8, col="red") +
    
    
    geom_point(aes(y=confirmed, size=as.ordered(date) ), alpha=0.3, col="yellow") +
  #  geom_point(aes_string(y="recovered", size=as.character("date")), alpha=0.8, col="green", fill= "green") +
    geom_point(aes(y=death, size=as.ordered(date) ), alpha=0.3, col="red") +
    
  #  geom_segment( aes_string(xend="state", y=strColPrevious, yend=strColLast), size = 1, col="brown",
      #             arrow = arrow(length = unit(0.2, "cm"))) +
    
    guides(fill="none", col="none") +
    theme_bw() +
    theme(legend.position = "bottom") +
    
    labs(
      title= paste0(metric, " (Confirmed, Recovered, Deaths) "),
      subtitle=paste0(my.paste(input$country, ", "), " on ", input$date),
      caption = "Data: John Hopkins University \nGenerated by iTrack (https://itrack.shinyapps.io/covid)",
      # caption="Green: Current period. Red: Previous period. Dot: historical average",
      # caption="Cross indicates historical average. Change since last period marked by arrow",
      #  y="Transactions",
      y=NULL,
      x=NULL
    ) 
  
  
  
  g
  ggplotly(g)
  
  # ... map ----
  
  
  plotMapJHU.CA (dtToday, input$metric)
  
  
  #  . plot Historical ---- 
  
  
  
  
  
}

leafletJHU.CA <- function (dtToday, metric=NULL) {
  
  # setkey(dt0,state,country)
  # setkey(dtGeo,state,country)
  # 
  # my.country <- "Canada"
  # dt <- dtJHU [country == my.country & date == my.date]; dt
  # dt$country <- dt$Lat <- dt$Long <- NULL
  # 
  # dtCast <- dcast(dt, state+date ~ type,value.var="cases"); dtCast
  
  
  dtCast <- dtGeo[dt0, on=c("state", "country")]; dtCast
  
  #  pal <- colorNumeric(c("green", "yellow", "red"), 0:30)
  
  
  
  dtCast[, ratingcol:= ifelse(is.na(confirmed), "black",
                              ifelse(confirmed == 0, "green",
                                     ifelse(confirmed <= 20, "yellow", "red")))]
  
  
  
  dtCast[, strMessage:= paste(
    sprintf("<b>%s</b> <br/>", state),
    sprintf("Total: %i <br>", confirmed), 
    sprintf("Today: %i <br>", confirmed), 
    sprintf("Speed: %i <br>", confirmed), 
    sprintf("Acceleration: %i <br>", confirmed)
  )
  ]
  
  # setnames(dtCast, c("Lat", "Long" ), c("lat","long"))
  
  
  leaflet(data = dtCast) %>% 
    addTiles() %>%
    addCircleMarkers(~lng, ~lat, 
                     #color = ~pal(traveller), 
                     color = ~ratingcol, 
                     popup = ~as.character(strMessage),
                     label = ~as.character(confirmed) #state)
    ) %>% 
    #  addMarkers(clusterOptions = markerClusterOptions())
    #  addMarkers(~long, ~lat,   popup = ~as.character(traveller), label = paste("BWT: ", ~traveller)             )  %>%
    addPopups(dtCast[country %in% input$country]$lng, 
              dtCast[country %in% input$country]$lat, 
              popup = dtCast[country %in% input$country]$strMessage, 
              options = popupOptions(closeButton = FALSE) ) %>% 
    addLegend("bottomleft", 
              colors = c("green", "yellow", "red", "black"),
              labels = c("No cases",
                         "Few cases",
                         "Many cases",
                         "N/A"), 
              opacity = 0.7)
  

  # leaflet from other -----

  library(leaflet)
  library(leafpop)
  library(purrr)
  coronavirus %>% setnames("Country.Region", "country")
  coronavirus %>% setnames("Province.State", "state")
  
  dt00 <- dtGeo[ , .(country, state, population)] [ 
    coronavirus [country %in% input$country ], on =c("country", "state") ] 


  populationCa <- unique(dt00, by=c("country", "state") ) [state!="", sum(population, na.rm=T)]
  dt00[state=="" & is.na(population), population:= populationCa]
  dt00[state=="", state:= "(National)"]
  
  dt00 [ , `cases per Million`:= as.integer(cases / population * 1000000)]
    
  cv_data_for_plot <- dt00 %>% #lazy_dt() %>% 
    dplyr::filter(cases > 0) %>% 
    dplyr::group_by(country,state,Lat,Long,type) %>% 
    dplyr::summarise(cases = sum(cases)) %>% 
    dplyr::mutate(log_cases = 2 * log(cases)) %>% 
    dplyr::ungroup()
  
  cv_data_for_plot.split <- cv_data_for_plot  %>% split(cv_data_for_plot$type)
  pal <- colorFactor(c("orange", "red","green"), domain = c("confirmed", "death","recovered"))
  map_object <- leaflet() %>% addProviderTiles(providers$Stamen.Toner)
  
  names(cv_data_for_plot.split) %>%
    purrr::walk( function(df) {
      map_object <<- map_object %>%
        addCircleMarkers(data=cv_data_for_plot.split[[df]],
                         lng=~Long, lat=~Lat,
                         #                 label=~as.character(cases),
                         color = ~pal(type),
                         stroke = FALSE,
                         fillOpacity = 0.8,
                         radius = ~log_cases,
                         popup =  leafpop::popupTable(cv_data_for_plot.split[[df]],
                                                      feature.id = FALSE,
                                                      row.numbers = FALSE,
                                                      zcol=c("type","cases per Million","country","state")),
                         group = df,
                         #                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                         labelOptions = labelOptions(noHide = F,
                                                     direction = 'auto'))
    })
  
  map_object %>%
    addLayersControl(
      overlayGroups = names(cv_data_for_plot.split),
      options = layersControlOptions(collapsed = FALSE) 
    )
  
  
  
  
# plot death vs. recovery ----    

dt0[date==dateMax] %>% #lazy_dt() %>% 
  # coronavirus  %>% 
  # dplyr::filter(Country.Region != "Others") %>%
  # dplyr::group_by(country, type) %>%
  # dplyr::summarise(total_cases = sum(cases)) %>%
  # tidyr::pivot_wider(names_from = type, values_from = total_cases) %>%
  dplyr::arrange(- confirmedTotal) %>%
  dplyr::filter(confirmedTotal >= 1000) %>%
  dplyr::mutate(recover_rate = recoveredTotal / confirmedTotal,
                death_rate = deathTotal / confirmedTotal) %>% 
  dplyr::mutate(recover_rate = dplyr::if_else(is.na(recover_rate), 0, recover_rate),
                death_rate = dplyr::if_else(is.na(death_rate), 0, death_rate)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(confirmed_normal = as.numeric(confirmedTotal) / max(as.numeric(confirmedTotal))) %>%
  plotly::plot_ly(y = ~ round(100 * recover_rate, 1),
                  x = ~ round(100 * death_rate, 1),
                  size = ~  log(confirmedTotal),
                  sizes = c(5, 70),
                  type = 'scatter', mode = 'markers',
                  color = ~ state, #country,
                  marker = list(sizemode = 'diameter' , opacity = 0.5),
                  hoverinfo = 'text',
                  text = ~paste("", state, # country, 
                                " Confirmed Cases: ", confirmedTotal,
                                " Recovery Rate: ", paste(round(100 * recover_rate, 1), "%", sep = ""),
                                " Death Rate: ",  paste(round(100 * death_rate, 1), "%", sep = ""))
  ) %>%
  plotly::layout(yaxis = list(title = "Recovery Rate", ticksuffix = "%"),
                 xaxis = list(title = "Death Rate", ticksuffix = "%", 
                              dtick = 1, 
                              tick0 = 0),
                 hovermode = "compare")




}
