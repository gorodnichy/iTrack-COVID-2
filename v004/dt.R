# source("../D7.common/dt.R")  


#.0 Common functions ----
if (T) {
  options(scipen=999); #remove scientific notation
  # Required packages
  packages <- c("ggplot2","plotly", "shiny")
  sapply(packages, library, character.only = TRUE)
  
 
  
  library(dplyr)
  #library(tidyverse) # includes: 
  # library(readxl)
  library(magrittr)
  library(lubridate,  quietly=T)
  options(lubridate.week.start =  1)
  
  library(stringr)
  # library(timeDate)
  # library(tibble)
  #library(png)
  # library(maps)
  # library(highcharter)
  # library(treemap)
  
  library("data.table"); 
  library(dtplyr);   
  as.dt <- as.data.table
  options(datatable.print.class=TRUE)
   
  library(flexdashboard)
  library(DT)
  library(leaflet)
  library(dygraphs)
  
  #library(timeDate)
  # library(zoo) # DT[, V1:=na.locf(V1, fromLast=F)]
}



if (T) {
  
  
  percentageOf <- function(a, equalto=0, decimals=2, multiply=100) {
    ( length ( which (a == equalto) )  /  length (a) * 100 ) %>% round(decimals)
  }
  if (F) {
    percentageOf (a=as.ordered(0:19))
    c( rep(1,3), rep(0,7) ) %>% as.ordered() %>% percentageOf
  }
  
  #meanOrdered <- function(a) {
  my.meanFactor <- function(a, decimals=2) {
    if (is.factor(a)) {
      percentageOf(a,decimals)
    } else {
      mean(a, na.rm = T) %>%  round(decimals)
    }
  }
  # my.max <- function(x) max(x, na.rm=T)
  # my.min <- function(x) min(x, na.rm=T)
  # my.mean <- function(x) mean(x, na.rm=T) %>% as.integer()
  # my.median <- function(x) my.median(x, na.rm=T)
  
  
  my.paste <- function(array, sep=" ") {
    str <- ""
    for (i in 1:length(array)) 
      str <- str %+% array[i] %+% sep
    return (substr(str, 1, nchar(str)-nchar(sep)))
  }
  
  cat.n <- function(x) {
    format(x, big.mark = ",")
  }
  
  "%+%" <- function (x,y) paste0(x,y)
  
  "%wo%" <- function(x, y) x[!x %in% y] #--  x not in y # does not work sometimes
  `%ni%` = Negate(`%in%`) # x not in y

  
  dt.rmcols <- function (dt, cols){
    dt [, (cols):=NULL]  %>% return
  }
  dt.rmrows <- function (dt, rows){
    dt [-rows] %>% return
  }
  dt.cols <- function(dt, column) {
    dt[, cols, with=F] %>% return
  }
  
  dt.rows <- function(dt, rows) {
    dt[rows] %>% return
  }

  
  
  dt.convert<- function(dt, cols, FUNCTION = ymd) {
    dt [, (cols):= lapply(.SD, function (x) x %>% as.character %>% FUNCTION), .SDcol = cols]
  }
  
  dt.replace <- function (dt, cols, FUNCTION = function (x) dplyr::if_else(is.na(x), 0L, x)) {
    dt [, (cols):= lapply(.SD, eval(FUNCTION)), .SDcol = cols]
  }
  dt.replaceNA <- function (dt, cols, value) {
    dt [, (cols):= lapply(.SD, function (x) dplyr::if_else(is.na(x), value, x)), .SDcol = cols]
  }
  dt.replaceNA2 <- function (dt, cols, value) {
    dt [, (cols):= lapply(.SD, function (x) ifelse(is.na(x), value, x)), .SDcol = cols]
  }
  dt.replaceNA0 <- dt.replace
  
  dt.replaceNA3 <- function (dt, cols, value) {
    dt [, (cols):= lapply(.SD, tidyr::replace_na, value), .SDcol = cols]
  }

  
  
  dt.replaceAB <- function(dt, cols, a, b) {
    dt[get(cols)==a, (cols):=b]
  }
  
  # dt.replaceAB2 <- function(dt, cols, a, b) {
  #  dt.replace [dt, cols, ifelse(get(x)==a, b, a)]
  
  
  # dt.order <- function (dt, cols) { # use - cols to reverse order
  #   dt[order(cols), with=F] #   dtGeoCa[order(get("state"))]
  # }
  # dt.arrange <- dt.order
  
  dt.filter <- function( dt, quote=.N) {
    dt[ eval(quote) ]
  }
  
  if (F) {
    
    dt.slice <- dt.rows
    dt.topN

    
    dt.distinct <- data.table::unique
    dt.select <- dt.cols 

    dt.filter <- dtplyr::filter
    
  }
  
  
  cat.rmd <- function(...) {
    cat("\n\n\n\n")
    cat (...)
    cat("\n\n\n\n")   
  }
  rmd.cat <- cat.rmd 
  
  set.seed(22)  #btn-block
  DIV_START <- function(strText="Details", bButton="button", bShow = "hhh"){
    iButton <- runif(1, min=0, max=100000) %>% as.integer()
    # "btn btn-primary btn-block btn-lg active" 
    if (bButton == "button") {
      rmd.cat( glue::glue(  paste0(
        '<button class="btn btn-primary btn-block active"  data-toggle="collapse" data-target="#BlockName',iButton, '"> Show/Hide ', strText, '</button>',   '\n',
        '<div id="BlockName', iButton,'" class="collapse', ifelse(bShow == "hide", "", " in"), '">'    
      )       )       )
    } else {
      rmd.cat(paste0("###", strText), "<div>")
    }
  }
  DIV_END <- function(str="") {
    rmd.cat("</div>")
  }
  
}