# readCovid.R
# Author: Gorodnichy

setwd("/home/dmitry/GitHub/iTrack-covid-priv")


#  1.  EU /World  ----

if (F){
  #  1.1 (small summary) https://www.ecdc.europa.eu ----
  
  library(readxl)
  library(httr)
  
  #create the URL where the dataset is stored with automatic updates every day
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
  
  #download the dataset from the website to a local temporary file
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  #read the Dataset sheet into “R”
  dtEU <- read_excel(tf)  %>% data.table() %T>% print # 8102   10
  
  #          dateRep   day month  year cases deaths countriesAndTerritories  geoId countryterritoryCode popData2018
  #           <POSc> <num> <num> <num> <num>  <num>                  <char> <char>               <char>       <num>
  #    1: 2020-04-05     5     4  2020    35      1             Afghanistan     AF                  AFG    37172386
  #    2: 2020-04-04     4     4  2020     0      0             Afghanistan     AF                  AFG    37172386
  # 8904: 2020-03-22    22     3  2020     1      0                Zimbabwe     ZW                  ZWE    14439018
  # 8905: 2020-03-21    21     3  2020     1      0                Zimbabwe     ZW                  ZWE    14439018
}



# . Canada ---------


if (F){
  
  ##################################################### #
  # 2.1.a  www150.statcan.gc.ca Summary ----
  
  
  # from: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076601
  # https://www150.statcan.gc.ca/t1/tbl1/#?pid=13100766&file=1310076601-eng.csv
  #   https://www150.statcan.gc.ca/t1/tbl1/#?pid=13100766&file=1310076601-noSymbol.csv
  #  https://www150.statcan.gc.ca/n1/tbl/csv/13100766-eng.zip ->
  #   https://www150.statcan.gc.ca/n1/en/tbl/csv/13100766-eng.zip
  
  # From "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076701"
  
   dt <- fread("https://www150.statcan.gc.ca/t1/tbl1/#?pid=13100767&file=1310076701-eng.csv") # does not work-  needs to be save locally 
   dt <- fread("https://www150.statcan.gc.ca/t1/tbl1/#?pid=13100766&file=1310076601-noSymbol.csv") # does not work-  needs to be save locally 
   
  dt <- fread("data/1310076701-eng(1).csv", header=T, stringsAsFactors=T, skip=5);dt %>% dim ; dt %>% names # 3095   12
  names(dt) <- dt[1] %>% transpose() %>% unlist 
  dt <- dt [-1]
  dtStatsCan1 <- dt
  
  dt %>% dim; dt %>% names
  # 1] 3093   12
  # [1] "Case identifier number4"              "Reference period"                     "Date case was last updated - month 5" "Date case was last updated - day 5"   "Episode date - month 6"               "Episode date - day 6"                
  # [7] "Gender 7"                             "Age group 8"                          "Transmission 9"                       "Hospitalization 10"                   "Intensive care unit 11"               "Status 12"                           
  # 
  #        Case identifier number4 Reference period Date case was last updated - month 5 Date case was last updated - day 5 Episode date - month 6 Episode date - day 6 Gender 7 Age group 8 Transmission 9 Hospitalization 10 Intensive care unit 11 Status 12
  #                          <fctr>           <fctr>                               <fctr>                             <fctr>                 <fctr>               <fctr>   <fctr>      <fctr>         <fctr>             <fctr>                 <fctr>    <fctr>
  #     1:                       1             2020                                    3                                 29                      3                   27        2           2              2                  7                      9         9
  #     2:                       2             2020                                    3                                 29                      3                   27        1           5              2                  7                      9         9
  #  3092:                    3092             2020                                    3                                 29                     99                   99        1           4              1                  2                      2         9
  #  3093:                    3093             2020                                    3                                 29                     99                   99        2           9              2                  2                      9         9
}
if (F){
  
  # 2.1.b  www150.statcan.gc.ca Full ----
  
  # 13100767-eng.zip (Compressed Archive (ZIP), 295.43kB)
  # require(readr)
  # myData <- read_csv("https://www150.statcan.gc.ca/n1/tbl/csv/13100767-eng.zip")  #not work
  # data <- read.table("https://www150.statcan.gc.ca/n1/tbl/csv/13100767-eng.zip", nrows=10, header=T, quote="\"", sep=",")  #not work 
  
  
  
  str <- "https://www150.statcan.gc.ca/n1/en/tbl/csv/13100766-eng.zip"
  df <- readr::read_csv(str)
  df <- read.table(str)
  #dt <- fread (str)
  
  dt <- fread("data/13100767.csv");dt %>% dim ; dt %>% names # 30930    16
  dtStatsCan2 <- dt
  # 30930    16
  #         REF_DATE    GEO          DGUID Case identifier number                   Case information    UOM UOM_ID SCALAR_FACTOR SCALAR_ID      VECTOR COORDINATE VALUE STATUS SYMBOL TERMINATED DECIMALS
  #             <int> <char>         <char>                  <int>                             <char> <char>  <int>        <char>     <int>      <char>     <char> <int> <lgcl> <lgcl>     <lgcl>    <int>
  #      1:     2020 Canada 2016A000011124                      1 Date case was last updated - month Number    223         units         0 v1146035470      1.1.1     3     NA     NA         NA        0
  #      2:     2020 Canada 2016A000011124                      1   Date case was last updated - day Number    223         units         0 v1146035471      1.1.2    29     NA     NA         NA        0
  #  30929:     2020 Canada 2016A000011124                   3093                Intensive care unit Number    223         units         0 v1146066398   1.3093.9     9     NA     NA         NA        0
  #  30930:     2020 Canada 2016A000011124                   3093                             Status Number    223         units         0 v1146066399  1.3093.10     9     NA     NA         NA        0

  
}

if (F){
  # 2.3 www.canada.ca/en/public-health ----
  
  #From https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html
  dt <- fread("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"); dt %>% dim ; dt %>% names
  dtPH <- dt
  dtPH$date %>% dmy %>% max 
  
  # [1] 432  11
  # [1] "pruid"       "prname"      "prnameFR"    "date"        "numconf"     "numprob"     "numdeaths"   "numtotal"    "numtested"   "numtoday"    "percentoday"
  
  #       pruid                 prname                  prnameFR       date numconf numprob numdeaths numtotal numtested numtoday percentoday
  #      <int>                 <char>                    <char>     <char>   <int>   <int>     <int>    <int>     <int>    <int>       <num>
  #   1:    35                Ontario                   Ontario 31-01-2020       3       0         0        3        NA        3       3.000
  #   2:    59       British Columbia      Colombie-Britannique 31-01-2020       1       0         0        1        NA        1       1.000
  # 431:    99 Repatriated travellers       Voyageurs rapatriés 04-04-2020      13       0         0       13        NA        0       0.000
  # 432:     1                 Canada                    Canada 04-04-2020   13882      22       231    13904    311971     1367       0.109
  
  ##################################################### #
}

# ************************************** ----

# 2.2 (USE THIS) canada UoT ----  

# used in
# https://art-bd.shinyapps.io/covid19canada/ - R
# https://georgeflerovsky-canada.github.io/Canada-COVID-19-dashboard/  - PowerBI
# https://covid19canada.herokuapp.com/ - Dash 
# https://itrack.shinyapps.io/covid19-canada/ - R (where code below is used)
# More at https://github.com/gorodnichy/iTrack-covid


if (F) {
  require(lubridate, quietly = T); require(data.table, quietly = T); require(magrittr, quietly = T);
  require(R6, quietly = T)
  
  
  # cCovid <- -----
  cCovid <- 
    R6Class(
      "CCovid", portable = T, # set to FALSE to  use x<<-2 in addition to self$x <- 2
      public = list(
        
        dataset = "JHU", 
        dtAll = data.table(),
        input0 = list(),
        initialize = function (){
          input0$dates <<- format(Sys.time(), "%Y-%m-%d") %>% ymd; 
          # input0$dates[2] <<- format(Sys.time(), "%Y-%m-%d") %>% ymd; 
          # input0$dates[1] <<- input0$dates[2] - 27 # 
          
          input0$city <<- "Ottawa"
          input0$state <<- "Ontario"
          input0$country <<- 'Canada'
          
        },
        
        finalize = function() {
          paste("Checking for memory leaks... fixed ")
        },
        
        result =list(),
        storeResult = function(result) {
          self$results[[length(self$results)+1]] <-result
        },
        
        updateConfiguration = function (input) { 
          
          input0 <<- input
          
          #self$setPort(POE=input$poe$poeId, NAME=input$poe$poeName)
          
          self$printAll()
        }, 
        printAll = function() {# 
          # print(self); 
          
          cat("==============================================================\n")
          cat("Input: "); print(input0);  
          
          cat(sprintf("%3d", 222)); cat("\n")
          
          cat("==============================================================\n")
          
        }
        
        
        
      )
    ) #    "CCovid"
  
  
  covid <- cCovid$new() #  #covid <- cCovid$new(dataset="EU") 
  covid$printAll()
  
  input0 = list()
  input0$dates <- format(Sys.time(), "%Y-%m-%d") %>% ymd; 
  input0$dates[2] <- format(Sys.time(), "%Y-%m-%d") %>% ymd; 
  input0$dates[1] <- input0$dates[2] - 27 # 
  
  input0$city <- "Toronto"
  input0$state <- "Ontario"
  input0$country <- 'Canada'
  covid$updateConfiguration(input0)
  
  
  covid$printAll()
  
  
  
  
  dtCases  <- dtMortality   <- dtRecovered   <- dtTesting  <- data.table()
  
}


# ************* test ALL ****************** ##########


#```{r loadALL, include=FALSE, warning=FALSE, echo=F}  ----

setwd("/home/dmitry/GitHub/iTrack-covid-priv")
source("covid-read.R")

dtCountries <- dtCaRegionsPop <- dtCaCitiesInUofT <- dtCaCitiesGeoPop <- data.table()
dtJHU.dailytotals <-  dtJHU <- data.table()
#dtUofT.dailytotals <- dtUofT.dailytotalsCities <- dtCases <- dtMortality <- dtRecovered <- dtTesting <- data.table()

readCovidJHU() # reads dtJHU data into readCovidJHU
dt

# ```{r select d0} ----



# dateNeg <-  dt0[recovered<0]$date
# negative <- dt0[recovered<0]$recovered
# dtJHU.dailytotals[date == dateNeg -1, recovered:=recovered+ negative/2]
# dtJHU.dailytotals[date == dateNeg, recovered:=recovered - negative/2]



groupby <- "Province.State"
col <- c("confirmed","death", "recovered")
addDerivatives(dt0, col, groupby)


dt0[Province.State=="Ontario" & date > max(date) - 8]


# . compute National ----

dtNational <-  dt0[, lapply(.SD, sum, na.rm = T), by =c("date"), .SDcols=cols]
dtNational [date > max(date) - 8]

dtNational$Province.State <- "National"

# #intersect (names(dtNational) ,  names(dt0)) 
cols <- c("Country.Region"  ,   "Lat"     ,            "Long"             )
dt0 [, (cols) := NULL]


setcolorder(dtNational, names(dt0))

dt0 <- dt0[Province.State!="National"]

dtAll <- dt0 %>% rbind(dtNational)



#. make results as strings ----

# cols.str = paste0(".", cols);  cols.str
# dtAll[ , (cols.str):= lapply(.SD, format, big.mark = ","), .SDcols=cols][]



# . plot Facet(by regions) HERE !!! -----


cols <- c("confirmed", "recovered", "death")
#cols3 <- paste0(cols, "Speed")

cols3 <- paste0(cols, input$f)

 
dt00 <- dtGeo[  dtJHU[country %in% input$country][confirmed>=0][date>ymd("2020-03-10")], on = c("country", "state") ]
setcolorder(dt00, "date")

#colsPer100K <- paste0(cols3, "Per100K")

if (input$normalize == T) 
  dt00[ , (cols3):= lapply(.SD, function(x) {as.integer(x/population*1000000)}),.SDcols=cols3]

  
g <- ggplot(dt00) +  
#facet_grid(. ~ state, scales = "free") +
 #facet_grid(reorder(state, Long)~.) +
  facet_grid(state~., scales="free") +
  
  geom_line(aes_string("date", "confirmedSpeed"), alpha=0.5, col="purple", size=2) +
  # geom_step(aes_string("date", "recoveredSpeed"), alpha=0.5, col="green", size=1) +
  # geom_step(aes_string("date", "deathSpeed"), alpha=0.5, col="red", size=1) +
  # geom_col(aes_string("date", colCases0[1]), alpha=0.2, fill="blue", size=1) +
  # geom_col(aes_string("date", colCases0[2]), alpha=0.2, fill="red", size=1) +
  # geom_col(aes_string("date", colCases0[3]), alpha=0.2, fill="green", size=1) +
    
  geom_smooth(aes_string("date", "confirmedSpeed"), size = 1, 
             # method= "gam",  formula = y ~ s(x,k=3),
            #  method = "lm", formula = y ~ poly(x, 2), 
             # method = "lm", formula = y ~ x + I(x^2), 
              col = "red", linetype = "dashed", alpha=0.8) +
 # ylim (0, 1000) + #dtNational[[ colCases0[1] ]] %>% max) +
  theme_bw() +
  scale_x_date(date_breaks = "1 week", 
               date_minor_breaks = "1 day", date_labels = "%b %d") +
  labs(
    title=input$country,
    # title= title,
    #subtitle=paste0("Dates: ", dtJHU$date %>% min, " - ", dtJHU$date %>% max),
    y=paste("Confirmed ", input$f), 
    x=NULL
  ) 
g

ggplotly(g)


# pik.g <<- 
ggplot(dt0[date>ymd("2020-03-01")]) +
  facet_grid(reorder(Province.State, Long)~.) +
  # facet_grid(Province.State~.) +
  # ggplot(dtJHU [Country.Region == my.country]) +
  geom_col(aes(`date`, `cases`, col=type), alpha=0.3) +
  theme_bw() +
  labs(
    title=my.country,
    # title= title,
    #subtitle=paste0("Dates: ", dtJHU$date %>% min, " - ", dtJHU$date %>% max),
    x=NULL
    # y="Transactions"
  ) +
  # scale_x_date(date_breaks = "1 week", date_labels = "%W", date_minor_breaks = "1 day") +
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
  scale_y_log10()
# coord_flip()




# ******************************** -------
# GMDH  - predictions ------
# 
# 
# 

library(GMDH) # Short Term Forecasting  
library(GMDH2)
library(GMDHreg)
devtools::install_github("dratewka/rGMDH")
library(rGMDH)
library(caret)

if (F) { # . *** TEST ALL CANADA *** --------------
  
 
  
}

province <- "Ontario"
province <- "Quebec"

plotStats <- function(province) {
  
  
  ggplot(
    #dtCities [ dtAll[Province.State=="National" ], on="Province.State"],
    dtCities [ dtAll[date>ymd("2020-03-01")], on="Province.State"],
    #       aes(`date`, `cases`)
    #       aes(`date`, `speedAve5`) 
    aes(`date`, `acceleration5`) 
  ) +
    # coord_cartesian(xlim(c(ymd("2020-03-01"), ymd("2020-04-01"))) ) + 
    facet_grid(reorder(Province.State, Long)~., scales="free") +
    # facet_grid(Province.State~.) +
    # ggplot(dtJHU [Country.Region == my.country]) +
    #geom_col(aes(col=type), alpha=0.3) +
    geom_line(aes(col=type), size=2) +
    # geom_smooth(aes(col=type), linetype = "dashed") +
    theme_bw() +
    labs(
      title=my.country,
      # title= title,
      #subtitle=paste0("Dates: ", dtJHU$date %>% min, " - ", dtJHU$date %>% max),
      x=NULL
      # y="Transactions"
    ) +
    # scale_x_date(date_breaks = "1 week", date_labels = "%W", date_minor_breaks = "1 day") +
    scale_x_date(date_breaks = "1 week", 
                 #xlim(ymd("2020-03-01"), ymd("2020-04-01")),
                 date_minor_breaks = "1 day", date_labels = "%b %d") +
    scale_y_log10()
  # coord_flip()
  # 
  # 
  # 
  
  dt <-  dtCases %>%  lazy_dt() %>% 
    group_by(date_report) %>%  
    summarise(Total = n()) %>% as.dt
  
  dtRes1 <- dtCases[ , .N, keyby = c("date_report", "health_region")]
  
  
  ggplot() +
    geom_col(aes(date_report, N), data = dtRes1, alpha=0.3, fill="green") +
    # geom_col(aes(`Metric`, `Previous`), alpha=0.3, fill="red") +
    # geom_point(aes(`Metric`, `Previous`), alpha=0.3, fill="red", size=4) +
    theme_bw() + 
    facet_grid(health_region~.) +
    #    #  facet_grid(.~province) +
    labs(
      #  title=NULL,
      #   title= title,
      #   subtitle=paste0("Dates: ", dateStart, " - ", dateEnd),
      caption="Green: Current period. Red: Previous period. Dot: historical average",
      x=NULL,
      y="Cases"
    ) 
  
  print(pik.g)
  
}


# inpout ######





if (F) {
  sliderInput('lat',
              "Latitude:",
              #  min=-100, max=100,
              width="100%",
              value=c(dtGeo$Lat %>% min, dtGeo$Lat %>% max), step=20)
  
  
  sliderInput('long',
              "Longitute:",
              #  min=-100, max=100,
              width="100%",
              value=c(dtGeo$Long %>% min, dtGeo$Long %>% max), step=20)
  
  renderUI({
    selectInput('country',
                "Select Country",
                multiple=T,
                # selected = "Canada",
                choices = dtGeo [lat >= input$lat [1] & lat <= input$lat  [2] & lng >= input$long [1] & lng <= input$long [2] ]
                # choices = c(dt$Country.Region %>% unique %>% sort() )
                # choices = c(" - All combined - ", dt$Country.Region %>% unique %>% sort() )
    )
  })
} 
