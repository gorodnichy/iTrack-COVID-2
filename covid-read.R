# source("covid-read.R")
# Author: Dmitry Gorodnichy (dmitry@gorodnichy.ca)

# setwd("/home/dmitry/GitHub/iTrack-covid-priv")
# source("covid-read.R")
source("dt.R")  
source("my.quotes.R")


caption.covid  <- paste0(
  "Generated", 
  " on ",  format(Sys.time(), "%d %B, %Y") ,
  " by iTrack COVID (https://itrack.shinyapps.io/covid)")

caption=caption.covid
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
  dtGeo <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") 
  dtGeo[c(1:4)]
  dtGeo[c(1:2, (.N-1):.N), 1:3]
  cols <- c("UID"  , "iso2" ,   "iso3"  ,   "code3", "FIPS", "Combined_Key")
  dtGeo [ ,(cols):=NULL] ;  
  dtGeo %>% setnames(c("city", "state", "country", "lat", "lng", "population")) 
  
  dtGeo <- dtGeo[country=="US"]
  dtGeo[is.na(lat)]$city
  
  dtGeo
  dtGeo[ state == "", state:=STR_TOTAL]
  dtGeo[ city == "", city:=STR_TOTAL]
  
  setcolorder(dtGeo, c( "country" , "state" , "city" , "lat", "lng" , "population"  ) )
  
  dtGeo[city == 'New York City', city:='New York']
  
  
  
  dtGeo
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


if (F) {
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



readCovidUofT.2 <- function () { # csv from their github, which is produced from their google doc
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

# 
# covid.reduceToTopNCitiesToday <- function(dt0, N=5) {
#   
#   dateMax <- dt0$date %>% max
#   dt <- dt0[date==dateMax][order(-confirmed)][1: min(N, nrow(dt))];
#   dt
#   topNcities <- dt$city ; 
#   topNcities
#   dt <- dt0[ city %in% topNcities];
#   dt
# }



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
