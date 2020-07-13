# source("covid-read.R")
# Author: Dmitry Gorodnichy (dmitry@gorodnichy.ca)

# setwd("/home/dmitry/GitHub/iTrack-covid-priv")
# source("covid-read.R")
source("dt.R")  
source("my.quotes.R")
library(flexdashboard)

STR_ALL <- "ALL"
STR_TOTAL <- "COMBINED"
# STR_TOTAL <- "TOTAL"
# STR_TOTAL <- ""


colsGeo <- c("country",   "state",       "city") 
colsCases <- c("confirmed", "deaths", "recovered")


# Set colors
# https://www.w3.org/TR/css-color-3/#svg-color
confirmed_color <- "purple"
active_color <- "#1f77b4"
recovered_color <- "forestgreen"
death_color <- "red"

if (F) { # input ----
  input <- list()
  input$date <- "2020-03-01"
  input$dateToday <- "2020-05-10"
  input$country <- "Canada"
  input$state <- STR_ALL
  input$state <- "Ontario"
  input$city <- "Ottawa"
  
  input$ascending = F
  input$sortby = "confirmedSpeed"
  input$sortby = "confirmedGrowth."
  input$showN = 20
  input$ascending = T
  
  input$normalize = T
  input$scale = T
  input$provincial = T
  
  
  input$fRadio = "Speed"
  input$convolution = 7
  
  input$trend = T
  input$predict = T
  
}




caption=paste0("Generated on ",  format(Sys.time(), "%d %B, %Y") ," by iTrack COVID (https://itrack.shinyapps.io/covid)")


caption.covid  <- paste0(
  # "Change in number of infected since yesterday is marked by arrow.\n", 
  "Generated", 
  #" on ",  format(Sys.time(), "%d %B, %Y") ,
  " by iTrack Covid (https://itrack.shinyapps.io/covid)")



gQuote <- ggplot () +  theme_bw() +
  
  # annotate("text", x = 1, y = 2,  label = paste("Thank you for using iTrack Covid App.")    ) +
  # annotate("text", x = 1, y = 4,  label = "Please wait until the App loads the data...") + 
  # annotate("text", x = 1, y = 4,  label = "While data is being loaded from  University of Toronto database ...") + 
  
  annotate("text", x = 1, y = 3, label=paste0(
    "Please wait until data is loaded from databases and all metrics are computed (It can take a couple of minutes)...  \n\n\n", 
    # "Tip of the Day: \n Bookmark this page on your phone to have live pandemic update in your pocket.\n\n"        ,
    "Quote of the day: \n ",
    d7.getQuote()
  ) ) +
  annotate("text", x = 1, y = 1, label="") + 
  annotate("text", x = 1, y = 4, label="") + 
  labs(x=NULL, y=NULL)




# ****************************************************** -------


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
  if (F) {
    return(readRDS("dtCities-fromJHU.Rds"))
  }
  dtGeo <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") %>% 
    dt.rmcols (c("UID"  , "iso2" ,   "iso3"  ,   "code3", "FIPS", "Combined_Key"))  %>% 
    setnames(c("city", "state", "country", "lat", "lng", "population")) %>% lazy_dt() %>% 
    #   filter(state %ni% c ("Diamond Princess", "Grand Princess", "Recovered")) %>% 
    as.dt 
  
  dtGeo[ state == "", state:=STR_TOTAL]
  dtGeo[ city == "", city:=STR_TOTAL]
  
  setcolorder(dtGeo, c( "country" , "state" , "city" , "lat", "lng" , "population"  ) )
  # saveRDS(dtGeo, "dtCities-fromJHU.Rds")
  return(dtGeo)
}


readCovidJHU <- function(coronavirus_data=NULL) {
  
  # Used in:  https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6
  # Source:  https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
  
  cols <- c ( "Lat"   ,  "Long"  ) ;
  dtJHUc <- fread( "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = T )
  dtJHUc [ ,(cols):=NULL] ; dtJHUc <- dtJHUc %>% melt(id=1:2); setnames(dtJHUc, c("state", "country", "date", "confirmed"))
  dtJHUd <- fread( "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" , stringsAsFactors = T )
  dtJHUd [ ,(cols):=NULL] ; dtJHUd <- dtJHUd %>% melt(id=1:2); setnames(dtJHUd, c("state", "country", "date", "deaths"))
  dtJHUr <- fread( "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv" , stringsAsFactors = T )
  (dtJHUr [ ,(cols):=NULL] %>%  melt(id=1:2) -> dtJHUr) %>%  setnames(c("state", "country", "date", "recovered"))
  
  lapply (list(dtJHUc, dtJHUd, dtJHUr), setkeyv, c("state" ,  "country" ,   "date"))
  dtJHU <- dtJHUc [dtJHUd][dtJHUr]  [, date := as.character(date) %>% mdy ]
  rm(dtJHUc, dtJHUd, dtJHUr);
  
  dtJHU$city <- STR_TOTAL
  dtJHU[ state == "", state:=STR_TOTAL, by=country]
  setcolorder(dtJHU, c("date", "country", "state", "city", "confirmed", "deaths", "recovered" ))
  dtJHU
}


readCovidUS <- function() {
  
  cols <- c ( 1:5, 8:11 ) ;
  dtUSc <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", stringsAsFactors = T )
  dtUSc [ ,(cols):=NULL] ;   dtUSc <- dtUSc %>% melt(id=1:2);   setnames(dtUSc, c("city", "state", "date", "confirmed"))
  
  cols <- c ( 1:5, 8:12 ) ;
  dtUSd <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", stringsAsFactors = T )
  dtUSd [ ,(cols):=NULL] ;   dtUSd <- dtUSd %>% melt(id=1:2);   setnames(dtUSd, c("city", "state", "date", "deaths"))
  
  #  DOES NOT EXIST dtUSr <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_US.csv")
  
  lapply (list(dtUSc, dtUSd), setkeyv, c("city" ,  "state" ,   "date"))
  dtUS <- dtUSc [dtUSd]
  
  rm(dtUSc, dtUSd);
  dtUS [, date := as.character(date) %>% mdy ]
  
  
  dtUS
  dtUS[is.na(date)]
  
  dtUS[is.na(confirmed)]
  
  colMetrics <- c("confirmed","deaths")
  dtUS [, (colMetrics):= lapply(.SD, tidyr::replace_na, 0), .SDcol = colMetrics]
  
  
  dtUS$country <- "US"
  dtUS$recovered <- NA
  dtUS[ city == "", city:=STR_TOTAL]
  setcolorder(dtUS, c("date", "country", "state", "city", "confirmed", "deaths", "recovered" ))
  
}


readGeoCa <- function() {
  if (F) {
    dtGeoCa <- readRDS("dtCitiesCa-fromUofT+Geo.Rds")  
    return(dtGeoCa)
  }
  
  dtCa <- readCovidUofT() %T>% print 
  
  dtCaCitiesInUofT <-  dtCa[date == dtCa$date %>% max]
  # dtCaCitiesInUofT <- dtCa[ ,.(country ,   state,   city)]  %>% unique
  
  dtCaCitiesInUofT
  dtCaCitiesInUofT$state %>% unique()
  cities <- dtCaCitiesInUofT$city %>% unique(); cities
  dtCaCitiesInUofT$state %>% unique()
  
  
  
  #   dtCaCitiesGeoPop <- fread("data/citiesCA.csv", stringsAsFactors=T);  # 
  dtCaCitiesGeoPop <- fread("https://simplemaps.com/static/data/country-cities/ca/ca.csv");  
  
  
  cols <- c(       "country"  ,  "iso2"   ,   "capital"     ,  "population_proper")
  dtCaCitiesGeoPop [ ,(cols):= NULL]
  dtCaCitiesGeoPop %>% setnames("lng", "lng")
  dtCaCitiesGeoPop
  setnames(dtCaCitiesGeoPop, "admin", "state")
  #  setnames(dtCaCitiesGeoPop, "city", "admin2")
  dtCaCitiesGeoPop[, city:=iconv(city,to="ASCII//TRANSLIT")]
  dtCaCitiesGeoPop[, state:=iconv(state,to="ASCII//TRANSLIT")]
  
  dtCaCitiesGeoPop$state %>%  unique()
  
  dtCaCitiesGeoPop 
  dtCaCitiesGeoPop$state %>% unique()
  dtCaCitiesGeoPop$city %>% unique()
  
  dtGeoCa <- dtCaCitiesGeoPop %>% merge (dtCaCitiesInUofT, by=c("city", "state"), all = T) ; dtGeoCa
  
  dtGeoCa <- dtCaCitiesGeoPop [ dtCaCitiesInUofT, on=c("city", "state")]
  
  dtGeoCa <-  dtCaCitiesGeoPop[city %in% cities]
  
  dtGeoCa
  
  
  dtGeoCa <- dtCaCitiesGeoPop [ dtCaCitiesInUofT, on=c("city", "state")]
  
  # dtGeoCa
  # dtCaCitiesGeoPop[city %ni% cities]
  # 
  #  dtCaCitiesGeoPop [ dtCaCitiesInUofT, on=c("state")]
  # dtCaCitiesGeoPop %>% merge (dtCaCitiesInUofT, by=c("state"), all = T)
  #  
  #   
  # dtGeoCa$country <- "Canada"
  
  
  if (F) {
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
    
  }
  
  setcolorder(dtGeoCa, c( "country" , "state" , "city" , "lat", "lng" , "population"  ) )
  saveRDS(dtGeoCa, "dtCitiesCa-fromUofT+Geo.Rds")
  return(dtGeoCa)
}



readCovidUofT.2 <- function () {
  dtCAc <- fread("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_hr/cases_timeseries_hr.csv", stringsAsFactors = T )
  dtCAd <- fread("https://github.com/ishaberry/Covid19Canada/raw/master/timeseries_hr/mortality_timeseries_hr.csv", stringsAsFactors = T ) 
  
}

readCovidUofT <- function () {
  strCovid <- "https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/"
  sheets <- c("Cases" ,"Mortality" ,"Recovered", "Testing" ,"Codebook")
  
  
  fOpen <- function(i, col1name) {
    strCovidCsv <- paste0(strCovid, "gviz/tq?tqx=out:csv&sheet=", sheets[i])
    dt <- fread(strCovidCsv, header=T, select= 1:15, stringsAsFactors = F)
    setnames (dt, c(col1name, names (dt) [-1] ) )
    return(dt)
  }
  
  fRemoveV <- function(dt) {
    str <- names(dt)
    cols <- str [ str_detect(str, regex("^V+")) ]
    dt [ , (cols):=NULL ]
    # print(str)
    return(dt)
  }
  
  dtCases <- fOpen(1, "case_id")  %>% fRemoveV() %>%  lazy_dt() %>% select(1:12) %>% as.dt() 
  dtMortality  <- fOpen(2,"death_id") %>% fRemoveV() %>%  lazy_dt() %>% select(c(1:9, 11)) %>%  as.dt()
  dtRecovered  <- fOpen(3,"date_recovered") %>% fRemoveV() %>%  lazy_dt() %>% select(1:4) %>% as.dt()
  dtTesting  <- fOpen(4,"date_testing") %>% fRemoveV() %>%  lazy_dt() %>% select(1:4) %>% as.dt()
  
  
  setnames(dtCases, "date_report", "date")
  setnames(dtMortality, "date_death_report", "date")
  setnames(dtRecovered, "date_recovered", "date")
  setnames(dtTesting, "date_testing", "date")
  
  setnames(dtCases, "health_region", "city")
  setnames(dtMortality, "health_region", "city")
  
  setnames(dtCases, "province", "state")
  setnames(dtMortality, "province", "state")
  setnames(dtRecovered, "province", "state")
  setnames(dtTesting, "province", "state")
  
  
  dtCases [ , date := dmy(date)]
  dtMortality [ , date := dmy(date)]
  dtRecovered [ , date := dmy(date)]
  dtTesting [ , date := dmy(date)]
  
  # # # . get at state level ----
  
  dtDailyCasesTotals <- dtCases[ , .(cases=.N), keyby = c("date", "state")]; dtDailyCasesTotals$type="confirmed"
  dtDailyMortTotals  <- dtMortality[ , .(cases=.N), keyby = c("date", "state")]; dtDailyMortTotals$type="deaths"
  dtRecoveredTotals  <- dtRecovered[ , .(cases=.N), keyby = c("date", "state")]; dtRecoveredTotals$type="recovered"
  
  dt <- dtDailyCasesTotals %>% rbind (dtDailyMortTotals) %>% rbind(dtRecoveredTotals)
  dt$country="Canada"
  dt$city=STR_TOTAL
  setkey(dt, date, country, state, city,type)
  
  dt0 <- dcast(dt, date+country+state+city ~ type, value.var="cases")
  setcolorder(dt0,  c("date",  "country","state", "city",  "confirmed","deaths",  "recovered") )
  
  # . get at city level ####
  
  dtDailyCasesTotals <- dtCases[ , .(cases=.N), keyby = c("date", "state", "city")]; dtDailyCasesTotals$type <- "confirmed"
  dtDailyMortTotals  <- dtMortality[ , .(cases=.N), keyby = c("date", "state", "city")]; dtDailyMortTotals$type="deaths"
  
  dt <- dtDailyCasesTotals %>% rbind (dtDailyMortTotals)
  dt$country="Canada"
  setkey(dt, date, country, state, city, type)
  
  
  dt00 <- dcast(dt, date+country+state+city ~ type, value.var="cases")
  dt00$recovered = NA
  
  # DT::datatable(dtAll)
  
  setcolorder(dt00,  c("date",  "country", "state",  "city", "confirmed","deaths",  "recovered") )
  
  dt00 [, (colsCases):= lapply(.SD, tidyr::replace_na, 0), .SDcol = colsCases]
  
  dt0 <- dt0 %>% rbind(dt00) 
  
  dt0[, city:=iconv(city,to="ASCII//TRANSLIT")]
  dt0[, state:=iconv(state,to="ASCII//TRANSLIT")]
  dt0 <- dt0[state!="Repatriated"]
  dt0
  dt0 [, (colsCases):= lapply(.SD, tidyr::replace_na, 0), .SDcol = colsCases]  
  
  
  if (F) { 
    
    dt0 %>% 
      dt.replaceAB("state", "BC", "British Columbia"  ) %>% 
      dt.replaceAB("state",  "NWT", "Northwest Territories" ) %>% 
      dt.replaceAB("state",  "NL", "Newfoundland and Labrador" ) %>% 
      dt.replaceAB("state", "PEI", "Prince Edward Island"  )
    
    
  }
  
  return(dt0)
}



readCaGIS <- function() { 
  
  dt <- fread("https://opendata.arcgis.com/datasets/3aa9f7b1428642998fa399c57dad8045_1.csv")
  dt %>% names  
  dt %>% nrow()
  dt[,region:=paste0(Province, ":",HR_UID) %>% as.ordered()]
  dt[, Last_Updated:= as_date(ymd_hms(Last_Updated))]
  dateMax <- dt$Last_Updated %>% max 
  
  dtGeo <-  fread("https://opendata.arcgis.com/datasets/3aa9f7b1428642998fa399c57dad8045_0.csv")
  
  dtGeo %>% names
  dtGeo %>% nrow()
  dtGeo[, Last_Updated:= as_date(ymd_hms(Last_Updated))]
  dtGeo$Last_Updated %>% max 
  # GGally::ggpairs (dtGeo [, c()]) 
  
  dtGeo[ , .(HR_UID,ENGNAME) ] [ dt [Province=="ON"][Last_Updated > dateMax-7], on = "HR_UID"] %>% 
    ggplot() +
    facet_wrap(ENGNAME~.) +
    geom_line(aes(Last_Updated, CaseCount), alpha=0.3) +
    theme_bw() +
    # +
    scale_x_date(date_breaks = "1 week", date_labels = "%W", date_minor_breaks = "1 day") +
    # scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") 
    # coord_flip()
    
    labs(
      title="my.country",
      # title= title,
      #subtitle=paste0("Dates: ", dtJHU$date %>% min, " - ", dtJHU$date %>% max),
      x=NULL
      # y="Transactions"
    ) 
  
  ggplot(dtGeo) +
    facet_grid(Province~.) +
    geom_col(aes(HR_UID, CurrentCaseFreeDays), alpha=0.3) 
  
  
}

# ****************************************************** -------


covid.reduceToTopNCitiesToday <- function(dt0, N=5) {
  
  dateMax <- dt0$date %>% max
  dt <- dt0[date==dateMax][order(-confirmed)][1: min(N, nrow(dt))];
  dt
  topNcities <- dt$city ; 
  topNcities
  dt <- dt0[ city %in% topNcities];
  dt
}


addDerivatives <- function (dt0, colsCases, colsGeo, convolution_window=3, difference_window=1) {
  
  colTotal <- paste0(colsCases, "Total")
  colSpeed <- paste0(colsCases, "Speed") # DailyAve") # "Speed"
  colAccel <- paste0(colsCases, "Accel") # "WeeklyDynamics") # "Accel." WeeklyAccel - change since last week in DailyAve
  # colSpeedAve <- paste0(colsCases, "SpeedAve")
  colAccel. <- paste0(colsCases, "Accel.")  # AccelAve
  colGrowth <- paste0(colsCases, "Growth")  #
  colGrowth. <- paste0(colsCases, "Growth.")  #
  colGrowth.Accel <- paste0(colsCases, "Growth.Accel")  #
  
  
  dt0[ ,  (colTotal) := cumsum(.SD),  by=c(colsGeo), .SDcols = colsCases]
  dt0[ ,  (colSpeed) := frollmean(.SD, convolution_window, align = "right", fill=0),  by=c(colsGeo), .SDcols = colsCases][, (colSpeed) := lapply(.SD, round, 1), .SDcols = colSpeed]
  
  dt0[ ,  (colSpeed) := lapply(.SD, function(x) ifelse (x<0,0,x) ), .SDcols = colSpeed]
  
  dt0[ ,  (colAccel) := .SD - shift(.SD,difference_window),  by=c(colsGeo), .SDcols = colSpeed]
  
  dt0[ ,  (colAccel.) := frollmean(.SD, convolution_window, align = "right", fill=0),  by=c(colsGeo), .SDcols = colAccel][, (colAccel.) := lapply(.SD, round, 1), .SDcols = colAccel.]
  
  
  # dN(T)/ N(T)
  # dt0[ ,  confirmedGrowth0 := confirmedTotal/confirmedSpeed]
  # dt0[ ,  recoveredGrowth0 := recoveredTotal/recoveredSpeed]
  # dt0[ ,  deathGrowth0 := deathTotal/deathSpeed]
  
  # dN(T+1) / dN(T)
  dt0[ ,  (colGrowth) := .SD / shift(.SD, difference_window<-1),  by=c(colsGeo), .SDcols = colSpeed]
  
  dt0[ ,  (colGrowth.) := frollmean(.SD, 2, align = "right", fill=0),  by=c(colsGeo), .SDcols = colGrowth][, (colGrowth.) := lapply(.SD, round, 3), .SDcols = colGrowth.]
  
  # dt0[ ,  (colGrowth.) := colGrowth,  by=c(colsGeo), .SDcols = colGrowth][, (colGrowth.) := lapply(.SD, round, 3), .SDcols = colGrowth.]
  
  
  dt0[ ,  (colGrowth.Accel) := .SD - shift(.SD,difference_window),  by=c(colsGeo), .SDcols = colGrowth.][, (colGrowth.Accel) := lapply(.SD, round, 3), .SDcols = colGrowth.Accel]
  
  dt0[, (colAccel):= NULL]
  dt0[, (colGrowth) := NULL]
  
}


# predictCovid <- function (x, t, v, a) {
#   x1 <- x + v*t + a*t*t/2
#   if (x1<0) x1 <- 0
#   x1
# }

if (F) {
  date0 <- dateMax
  N=1
  dt00 <-  dt [city=="Ottawa" & country== "Canada"]
  
  dt
}

addPredictions  <- function (dt00, colsCases,  date0=dateMax, N=7) {
  # dt00$result <- "Observed"
  dt00[ , .(date, confirmedSpeed,result)]
  r2 <- r <- dt00 [date==date0]
  r2$result <- r$result <- "Expected"
  
  r2$date <- r$date <- dt00 [date==date0]$date + N
  
  a <- r$confirmed + r$confirmedAccel.*N ; a
  b <- r$confirmed * r$confirmedGrowth.^N ; b
  aa <- r$confirmed + r$confirmedAccel*N ; aa
  bb <- r$confirmed * r$confirmedGrowth^N ; bb
  
  r$confirmed <- min(a,b,aa,bb) %>% max(0)
  r2$confirmed <- min(a,b,aa,bb) %>% max(0)
  
  dt00 <- dt00 %>% rbind(r) %>% rbind(r2) %>% unique 
  setkey(dt00, date)
  
  # [1]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  2.0  2.2  2.2  2.2  2.4  2.1  1.8  2.2  3.2  4.4  5.5  9.5 15.0 20.9 28.1 32.1 35.0 36.4
  # [26] 38.2 38.2 35.6 33.0 30.2 29.4 29.9 30.1 31.1 30.0 31.1 33.8 32.9 34.9 37.0 39.1 40.5 41.8 44.5 41.5 44.1 43.9 45.5 49.8 53.6
  # [51] 55.4 56.1 55.5 53.1 50.5 44.8 38.2  
  # >   r$confirmed  # [1] 24
  # >   r$confirmedSpeed  # [1] 38.2
  # >   aa <- r$confirmed + r$confirmedAccel ; aa   # [1] 17.4
  # >   a <- r$confirmed + r$confirmedAccel. ; a    # [1] 22.55
  # >   bb <- r$confirmed * r$confirmedGrowth ; bb  # [1] 20.46429
  # >   b <- r$confirmed * r$confirmedGrowth. ; b   # [1] 23.28
}

# predictCovid <- function (totalToday, days, dailySpeed, dailyAccel) {
#   x1 <- x + v*t + a*t*t/2
#   if (x1<0) x1 <- 0
#   x1
# }
# 
# getPredicted <- function (dt0) {
#   
#   colsPredicted1 <- paste0(cols, "Predicted1")
#   dt0[ , (colsPredicted) := lapply(.SD, predictCovid, ), by=c("country", "state"), .SDcols=cols]
#   
#   t = 1
#   c <- dt0[date==dt0$date %>% max] %$% predictCovid(confirmedTotal, 0, confirmedDailyAve, confirmedWeeklyDynamics)
#   
#   
#   
#   
# }


if (F) {
  
  g1 <- ggplot(dt00, aes(x=city)) +
    coord_flip() +
    scale_col_brewer(palette = "Greens", direction = 1) +   #scale_fill_grey(0.3, 0.9) +
    
    #geom_col(aes_string(y=strColLast), fill="Green", alpha=0.2) +  # for plotly
    geom_col(aes_string(y=strColLast,  col="date"), alpha=0.6) +
    geom_point(aes_string(y=strColLast), alpha=0.9, col="green", size=7) +
    geom_point(aes_string(y=strColPrevious), alpha=0.8, col="brown", size=4) +
    geom_segment( aes_string(xend="Metric", y=strColPrevious, yend=strColLast), size = 1, col="brown",
                  arrow = arrow(length = unit(0.2, "cm"))) +
    
    guides(fill="none", col="none") +
    theme_bw() +
    theme(legend.position = "bottom") +
    
    labs(
      title= title,
      subtitle=paste0("Dates: ", from, " - ", to),
      caption = "Change since previous period is marked by arrow.",
      # caption="Green: Current period. Red: Previous period. Dot: historical average",
      # caption="Cross indicates historical average. Change since last period marked by arrow",
      #  y="Transactions",
      y=NULL,
      x=NULL
    ) 
  
  
  
}


getTodayMetric  <- function (dt0, metric, strMetrics) {
  
  
  dtToday <<- dt0[date == dt0$date %>% max |
                    #  date == dt0$date %>% max - 30 |
                    date == dt0$date %>% max - 1 |
                    date == dt0$date %>% max - 7  ] [ 
                      , region:= paste0(country, " - ", state)]  %>% setcolorder("region")
  
  
  return(dtToday1)
}




getMetricsToday2old <-  function() {  # (date)
  
  
  
  metric <- c("confirmed", "deaths", "recovered")
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
  
  #  source("covid-read.R")
  
  dtJHU <-readCovidJHU() %T>% print; 
  dtCa <- readCovidUofT() %T>% print 
  dtUS <- readCovidUS() %T>% print
  dtJHU[.N]; dtCa[.N]; dtUS[.N]
  dtAll <-   dtJHU %>% rbind ( dtCa ) %>% rbind (dtUS)
  
  dateMax <- dtAll$date %>% max ; dateMax
  
  
  
  dtGeoAll <-readGeo() %T>% print
  # dtGeoCa <- readGeoCa() %T>% print
  # dtGeoAll <- dtGeoCa %>% rbind(dtGeo)
  
  colsGeo <- c("country",   "state",       "city")
  colsCases <- c("confirmed", "deaths", "recovered")
  
  dtAll [, (colsCases):= lapply(.SD, tidyr::replace_na, 0), .SDcol = colsCases]
  
  dt <- dtGeoAll [dtAll, on=colsGeo]
  
  dt <- addDerivatives(dt, colsCases, colsGeo) # , input$convolution)
  
  
  
  dt [ , deathRate:= as.integer(deathsTotal / confirmedTotal * 100 )]
  dt [ , recoveryRate:= as.integer(recoveredTotal / confirmedTotal * 100 )]
  
  dt [ , activeTotal := confirmedTotal - recoveredTotal - deathsTotal]
  
  cols <- c("confirmed", "deaths",  "confirmedTotal" ,  "deathsTotal"  ,  "confirmedSpeed"  , "deathsSpeed"   )
  colsPerMil <- paste0(cols, "PerMil")
  dt[ , (colsPerMil):= lapply(.SD, function(x) {as.integer(x/population*1000000)}),.SDcols=cols]
  
  
  dt %>% names()
  
  colsNeeded <- c(
    "date"       ,      "country"   ,       "state"      ,      "city"      ,    
    "lat"       ,       "lng"            ,    "population"    ,
    # "confirmed",        "deaths"  ,   
    "confirmedTotal",   "deathsTotal"  ,   "confirmedSpeed" ,  "deathsSpeed"    ,  
    "confirmedAccel."  , "deathsAccel."  ,  "confirmedGrowth." ,"deathsGrowth.", "deathRate", 
    "confirmedTotalPerMil" ,"deathsTotalPerMil"  ,  "confirmedSpeedPerMil","deathsSpeedPerMil"  
  )
  
  
  dtGeo0 <- dt[date == dateMax][, (colsNeeded), with=F]
  
  setcolorder(dtGeo0, c( "date", "country" , "state" , "city" , "lat", "lng" , "population"  ) )
  
  saveRDS(dtGeo0, "dtGeoAll-0.Rds")  
  dtToday <- readRDS("dtGeoAll-0.Rds") 
  
  
  dtToday[ , region:=paste0( str_trunc(country, 2, ellipsis = ""), "-", str_trunc(state, 3, ellipsis = ""), ": ",  city)]
  
  dtToday %>% names
  dtToday [order(get(input$sortby))][1:input$showN]
  dtToday0 <- dtToday[country == "Canada"]
  
  
  # dt000: country=="Canada" & city=="Ottawa" ----
  
  
  
  dt000 <- dtAll[dtGeo0[country=="Canada" & city=="Ottawa", c(colsGeo, "lat", "lng", "population"), with =F], on=colsGeo]; dt000
  
  addDerivatives(dt000, colsCases, colsGeo) # , input$convolution)
  
  dt000$result <- "Observed"
  dt000 <- addPredictions(dt000, colsCases,  dateMax, N=7)
  dt000 <- addPredictions(dt000, colsCases,  dateMax, N=14)
  
  # dt000 <- addPredictions(dt000, colsCases,  dateMax, N=1); 
  # dt000 <- addPredictions(dt000, colsCases,  dateMax, N=3)
  # dt000 <- addPredictions(dt000, colsCases,  dateMax, N=2)
  
  dt000[ , .(date, confirmedSpeed,result)]
  
  
  dt000[ , region:=paste0( str_trunc(country, 2, ellipsis = ""), "-", str_trunc(state, 3, ellipsis = ""), ": ",  city)]
  
  
  plotTrendsPrediction( dt000, input)
  
  # .... plot  ----
  
  input$country <- "Canada";  input$state <- "Ontario" ; input$city <- "Ottawa"
  input$country <- "US";  input$state <- "New York" ; input$city <- "New York"
  
  
  input$sortby = choices[6]; input$sortby
  
  input$showN = 20
  
  
  input$city <- STR_ALL
  
  dtToday [    country %in%input$country  & state %in%  input$state ] [ city %in% input$city ]  
  
  
  dtToday0 <- dtToday [ country %in% ifelse  (input$country == STR_ALL, dtToday$country %>% unique, input$country )]  ; dtToday0
  dtToday0 <- dtToday0 [ state %in% ifelse  (input$state == STR_ALL, dtToday0$state %>% unique, input$state )]  ; dtToday0
  #  dtToday0 <- dtToday0 [ city %in% ifelse  (input$city == STR_ALL, dtToday0$city %>% unique, input$city )  ]  ;dtToday0
  dtToday0 <- dtToday0 [order(get(input$sortby))][ 1:input$showN] ; dtToday0
  
  input$sortby = "confirmedGrowth."
  input$fRadio = "confirmedSpeed"
  
  
  
  if (inp$sortby =="name"){
    dtToday0[ , region := reorder(region, confirmedSpeed)]
  } 
  
  dtToday0$region <- reorder( dtToday0$region , dtToday0[[input$sortby ]] )
  
  
  
  
  
}



plotToday <- function(dt00, input) {   
  cols <- c("confirmed", "recovered", "deaths")
  cols3 <- paste0(cols, input$fRadio)
  cols3 <- paste0(cols, "Speed")
  
  dtToday <- dt00[date==input$dateToday]
  dtToday [ , deathRate:= as.integer(deathsTotal / confirmedTotal * 100 )+1]
  
  if (input$normalize == T & dtToday$population %>% min(na.rm = T) > 1)
    dtToday[ , (cols3):= lapply(.SD, function(x) {as.integer(x/population*1000000)}),.SDcols=cols3]
  
  # dtToday[, region:=reorder(region, get(input$sortby))]
  
  g <- dtToday %>% 
    ggplot(
      aes(x=  region   )
    )  + 
    theme_bw() +
    # facet_grid(reorder(region, region)~.) +
    
    coord_flip() + # facet_grid( . ~ date) +
    scale_fill_distiller(palette = "Reds", direction = 1) +   #scale_fill_grey(0.3, 0.9) +
    
    geom_col(aes(y=confirmedSpeed), alpha=0.3, fill="orange") +
    geom_point(aes(y=confirmedSpeed-confirmedAccel., size=confirmedTotal), alpha=0.4, col="orange") +     
    geom_point(aes(y=confirmedSpeed, size=confirmedTotal ), alpha=0.99, col="orange") +
    
    # geom_point(aes(y=confirmedSpeed-confirmedAccel., size=confirmedTotal-confirmedSpeed ), alpha=0.4, col="orange") + 
    
    geom_col(aes(y=deathsSpeed, fill=deathRate), alpha=0.99) +
    geom_point(aes(y=deathsSpeed, size=deathsTotal ), alpha=0.9, col="red") +
    geom_point(aes(y=deathsSpeed-deathsAccel., size=deathsTotal), alpha=0.4, col="red") +
    
    geom_segment( aes( 
      # xend=reorder(region, get(input$sortby)),
      xend = region,
      yend=confirmedSpeed, y=confirmedSpeed-confirmedAccel.      ), 
      size = 1, col="black",
      arrow = arrow(length = unit(0.1, "cm"))
    ) +
    
    guides(col="none") +
    
    theme(legend.position = "bottom") +
    labs(
      title= paste0("Infected and deaths per day on ", input$dateToday),
      # title= paste0("Increase in the number of infected and deaths per day on ", input$dateToday),
      # title= paste0("Speed and acceleration of pandemic on ", format(Sys.time(), "%d %B, %Y") ),
      # title= paste0("Pandemic dynamics on ", input$dateToday), # dateMax),
      
      # subtitle=
      #   # r.subtitle(),
      #   paste0("Top ", min (dtToday %>% nrow, input$showN), " regions ",
      #          "in ", my.paste(input$region, ", "),
      #          " (sorted by '", input$sortby, "')."
      #          # , ". States/Provinces: ", my.paste(input$state, ", ")
      #   ),
      
      size="Total number of cases",
      fill="Mortality rate",
      # y="Infected (orange) and deaths (red) per day. \nChange since yesterday is marked by arrow.",
      y=NULL,
      x=NULL,
      
      caption=caption.covid
    )
  
  
  if (input$log10 ) {
    g <- g + scale_y_log10()
  }
  
  g
  
}

plotTrends <- function(dt00, input) {
  cols <- c("confirmed", "recovered", "deaths")
  cols3 <- paste0(cols, input$fRadio)
  
  dtToday <- dt00[date==input$dateToday]
  dt <- dt00[date >= input$date]
  
  
  if (input$normalize == T)
    # if (input$normalize == T & dt$population %>% min(na.rm = T) > 1)
    dt[ , (cols3):= lapply(.SD, function(x) {as.integer(x/population*1000000)}),.SDcols=cols3]
  
  
  g <- ggplot( dt  ) + 
    
    facet_wrap(.~
                 region,
               scales=ifelse(input$scale, "fixed", "free_y")   
    ) +
    #scale_y_continuous(limits = c(0, NA)) +
    # scale_y_continuous(limits = c(min(dt00[[ cols3[1] ]]), max(dt00[[ cols3[1] ]]) )) +
    scale_y_continuous(limits = c(min(dt[[ cols3[1] ]]), NA )) +
    
    geom_vline(xintercept=input$dateToday, col = "orange", size=2) +
    geom_vline(xintercept=ymd(input$dateToday)-14, col = "orange", size=1) +
    # geom_vline(xintercept=input$dateToday-14, col = "orange", size=1) +
    geom_point(aes_string("date", cols3[1]), alpha=0.5, col="purple", size=2) +
    geom_line(aes_string("date", cols3[1]), alpha=0.5, col="purple", size=1) +
    
    theme_bw() +
    scale_x_date(date_breaks = "1 week",
                 date_minor_breaks = "1 day", date_labels = "%b %d") +
    labs(
      
      title= paste0("Dynamics over time: from ", 
                    input$date , " to " ,  input$dateToday
                    # , 
                    # " (", input$fRadio, " over time)"
      ),
      
      # title= paste0("Number of infected from ", input$date , " to " ,  input$dateToday, 
      #               " (", input$fRadio, " over time)"),
      # 
      # subtitle=
      #   # r.subtitle(),
      #   paste0("Top ", min (dtToday %>% nrow, input$showN), " regions ",
      #          "in ", my.paste(input$region, ", "),
      #          " (sorted by '", input$sortby, "')."
      #          # , ". States/Provinces: ", my.paste(input$state, ", ")
      #   ),
      
      
      y=paste("Cases ", input$fRadio, ifelse(input$normalize, " (per Million)","")),
      # y=paste("Cases"),
      # y=NULL,
      x=NULL,
      
      caption=caption.covid
    )
  
  if (input$log10 ) {
    g <- g + scale_y_log10()
  }
  
  if (input$trend ) {
    
    # if (input$trend_SE ) {
    g <- g + geom_smooth(aes_string("date", cols3[1]), size = 1, level=0.99,
                         method= "gam", # method= "gam",  formula = y ~ s(x,k=3),
                         # method = "lm", formula = y ~ poly(x, 4),
                         col = "black", linetype = 2, alpha=0.3)
    # } else
    #   {
    #   g <- g + geom_smooth(aes_string("date", cols3[1]), size = 1, se = FALSE,
    #                        method= "gam",   # method= "gam",  formula = y ~ s(x,k=3),
    #                        # method = "lm", formula = y ~ poly(x, 4),
    #                        col = "black", linetype = 3, alpha=0.5)
    # }
    
  }
  
  g
}



plotTrendsPrediction <- function(dt00, input) {
  
  dt00 <- dt00[date >= input$date]
  
  cols <- c("confirmed", "recovered", "deaths")
  cols3 <- paste0(cols, input$fRadio)
  
  cols <- c("confirmed")
  cols3 <- "confirmedSpeed"
  
  
  g <- ggplot( dt00 [result=="Observed"] ) + 
    # facet_wrap(.~ region      ) +
    scale_y_continuous(limits = c(min(dt00[[ cols3[1] ]]), NA )) +
    # 
    # geom_vline(xintercept=input$dateToday, col = "orange", size=2) +
    # geom_vline(xintercept=ymd(input$dateToday)-14, col = "orange", size=1) +
    # geom_vline(xintercept=input$dateToday-14, col = "orange", size=1) +
    geom_point(aes_string("date", "confirmedSpeed"), alpha=0.5, col="purple", size=2) +
    geom_line(aes_string("date", cols3[1]), alpha=0.5, col="purple", size=1) +
    
    
    theme_bw() +
    scale_x_date(date_breaks = "1 week",
                 date_minor_breaks = "1 day", date_labels = "%b %d") +
    labs(
      
      title= paste0("Dynamics over time: from ", 
                    input$date , " to " ,  input$dateToday
                    # , 
                    # " (", input$fRadio, " over time)"
      ),
      
      # subtitle=
      #   # r.subtitle(),
      #   paste0("Top ", min (dt %>% nrow, input$showN), " regions ",
      #          "in ", my.paste(input$region, ", "),
      #          " (sorted by '", input$sortby, "')."
      #          # , ". States/Provinces: ", my.paste(input$state, ", ")
      #   ),
      
      
      y=paste("Cases ", input$fRadio, ifelse(input$normalize, " (per Million)","")),
      # y=paste("Cases"),
      # y=NULL,
      x=NULL,
      
      caption=caption.covid
    )
  
  
  if (input$trend ) {
    
    # if (input$trend_SE ) {
    g <- g + geom_smooth(aes_string("date", cols3[1]),  data=dt00, size = 1, level=0.99,
                         method= "gam", # method= "gam",  formula = y ~ s(x,k=3),
                         # method = "lm", formula = y ~ poly(x, 4),
                         col = "black", linetype = 2, alpha=0.3)
    
  }
  
  g
}




plotMap <- function(dt, input) {
  
  
  # dt[city==STR_ALL,  region:=paste0( country, ": ", state)]
  # dt[state==STR_ALL,  region:=paste0( country)]
  
  # pal <- colorNumeric(c("green", "yellow", "red"), 0:30)
  
  dt[, ratingcol:= 
       ifelse(confirmedSpeed==0, "green",
              ifelse(confirmedGrowth.<1, "yellow",
                     ifelse(confirmedGrowth. == 1, "orange", "red")))
     ]
  
  dt[, strShort:= paste(
    # region, "- ", confirmedSpeed, "(",  confirmedAccel., ") / day"
    sprintf("%s: %i (%+.2f) / day. R0=%.3f", 
            region, 
            as.integer(confirmedSpeed), confirmedAccel., confirmedGrowth.
    )
  )
  ]
  
  
  # dtToday[, strMessage:= paste0(state, " - ", city, 
  #                               ":<br><b>Confirmed</b>",
  #                               "<br>  Total: ", confirmedTotal, "(", confirmedTotalPercentage, "%)",
  #                               "<br>  Daily: ", confirmed, "(", confirmedPercentage, "% population)", 
  #                               "<br> <b>Deaths</b>",
  #                               "<br>  Total: ", deathsTotal, "(", deathsTotalPercentage, "%)",
  #                               "<br>  Daily: ", deaths, "(", deathsPercentage, "% population)",
  #                               "<br><b>Death rate </b>", 
  #                               "<br>  Today: ", deathRate, ". Average: ", deathRateAverage
  # )  ]
  
  dt[, strMessage:= paste(
    # region,
    sprintf("<b> %s </b><br>", region),
    
    sprintf("Growth rate (R0):  %+.3f (%+.3f)  <br>", as.integer(confirmedGrowth.), confirmedGrowth.Accel),
    sprintf("INFECTED:  %i (%+.2f) / day <br>", as.integer(confirmedSpeed), confirmedAccel.),
    # sprintf("Per capita, in a million:  %i (%+i) / day <br>", as.integer(confirmedSpeedPerMil), as.integer(confirmedAccel.PerMil)),
    sprintf("DEATHS:  %i (%+i) / day <br>", as.integer(deathsSpeed), as.integer(deathsAccel.)),
    # sprintf("Per capita, in a million:  %i (%+i) / day <br>", as.integer(deathsSpeedPerMil), as.integer(deathsAccel.PerMil)),
    #     sprintf("TOTAL:  %i (%+.2f) / day <br>", as.integer(confirmedSpeed), confirmedAccel.),
    # "TOTALS: ", confirmedTotal
    "MORTALITY RATE: ", deathRate, "(%)"
    # sprintf("Mortality rate: %.2f (%%)", deathsTotal/confirmedTotal) 
  )
  ]
  
  
  leaflet(data = dt) %>% 
    addTiles() %>%
    addCircleMarkers(~lng, ~lat, 
                     #color = ~pal(traveller), 
                     # radius = ~ confirmedSpeed,
                     color = ~ratingcol, 
                     popup = ~as.character(strMessage),
                     label = ~strShort, #"Click for details", #~region
                     labelOptions = labelOptions(
                       noHide = F,
                       direction = 'auto')
    ) %>% 
    #  addMarkers(clusterOptions = markerClusterOptions())
    #  addMarkers(~lng, ~lat,   popup = ~as.character(confirmedSpeed), label = paste("BWT: ", ~confirmedSpeed)             )  %>%
    addPopups(dt$lng, 
              dt$lat, 
              popup = dt$strShort, # region, 
              options = popupOptions(
                closeButton = F) 
    )   %>%
    addLegend("bottomleft",
              colors = c("green", "yellow", "orange", "red"),
              labels = c(
                "COVID free",
                "R0<1",
                "R0=1",
                "R0>1"),
              opacity = 0.7)
}

