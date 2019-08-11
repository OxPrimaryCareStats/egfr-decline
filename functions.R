#=================#
#                 #
#### FUNCTIONS ####
#                 #
#=================#

#####################
### Notifications ###
#####################

notify <- function(message){
  if(Sys.info()["sysname"] == "Linux"){
    message <- paste0("'", message, "'")
    system(paste("notify-send -c im -u low -i /usr/share/pixmaps/rstudio.png RStudio", message))
  }
  #if(Sys.info()["sysname"] == "Darwin"){
  #  message <- paste0("'", message, "'")
  #  system(cat("osascript -e \"", paste("display notification", message, "with title 'RStudio'"), "\"", sep = ""))
  #}
}

##############################
### Convert Rates to x/day ###
##############################

time.denom <- function(value, units.in, units.out) {
  ### Define Regex Terms ###
  ps <- "^ps$|/ps$"
  ns <- "^ns$|/ns$"
  us <- "^us$|/us$"
  ms <- "^ms$|/ms$"
  s <- "^s$|/s$|^second$|/second$"
  min <-"^min$|/min$|^minute$|/minute$"
  hr <- "^h$|/h$|^hour$|/hour$|^hr$|/hr$"
  hr12 <- "^12hrs$|/12hrs$"
  day <- "^d$|/d$|^day$|/day$"
  week <- "^week$|/week$"
  month <- "^month$|/month$"
  year <- "^year$|/year$"
  
  ### Convert Rates to x/day ###
  if(grepl(ps, units.in)) {
    value.day <- value * 86400000000000000
  } else if(grepl(ns, units.in)) {
    value.day <- value * 86400000000000
  } else if(grepl(us, units.in)) {
    value.day <- value * 86400000000
  } else if(grepl(ms, units.in)) {
    value.day <- value * 86400000
  } else if(grepl(s, units.in)) {
    value.day <- value * 86400
  } else if(grepl(min, units.in)) {
    value.day <- value * 1440
  } else if(grepl(hr, units.in)) {
    value.day <- value * 24
  } else if(grepl(day, units.in)) {
    value.day <- value
  } else if(grepl(hr12, units.in)) {
    value.day <- value * 2
  } else if(grepl(week, units.in)) {
    value.day <- value / 7
  } else if(grepl(month, units.in)) {
    value.day <- value / (365.25 / 12)
  } else if(grepl(year, units.in)) {
    value.day <- value / (365.25)
  } else {
    value.day <- NA
  }

  ### Convert to Output Format ###
  if(units.out == "ps") {
    value.out <- value.day / 8.64E16
  } else if(units.out == "ns") {
    value.out <- value.day / 8.64E13
  } else if(units.out == "us") {
    value.out <- value.day / 8.64E10
  } else if(units.out == "ms") {
    value.out <- value.day / 8.64E7
  } else if(units.out == "s") {
    value.out <- value.day / 8.64E4
  } else if(units.out == "min") {
    value.out <- value.day / 1440
  } else if(units.out == "hr") {
    value.out <- value.day / 24
  } else if(units.out == "day") {
    value.out <- value.day
  } else if(units.out == "week") {
    value.out <- value.day * 7
  } else if(units.out == "month") {
    value.out <- value.day * (365.25 / 12)
  } else if(units.out == "year") {
    value.out <- value.day * (365.25)
  } else {
    value.out <- NA
  }
  
  ### Output Value ###
  return(value.out)
}

#######################
### Convert Weights ###
#######################

weight.num <- function(value, units.in, units.out) {
  ### Define Regex Terms ###
  pg <- "^pg$|^pg/"
  ng <- "^ng$|^ng/"
  ug <- "^ug$|^ug/"
  mg <- "^mg$|^mg/"
  g <- "^g$|^g/"
  kg <- "^kg$|^kg/"
  stone <- "^stone$"
  
  ### Convert Everything to Grams ###
  if(grepl(pg, units.in)) {
    value.g <- value / 1E12
  } else if(grepl(ng, units.in)) {
    value.g <- value / 1E9
  } else if(grepl(ug, units.in)) {
    value.g <- value / 1E6
  } else if(grepl(mg, units.in)) {
    value.g <- value / 1E3
  } else if(grepl(g, units.in)) {
    value.g <- value
  } else if(grepl(kg, units.in)) {
    value.g <- value * 1E3
  } else if(grepl(stone, units.in)) {
    value.g <- value * 6.37E3
  } else {
    value.g <- NA
  }
  
  ### Convert to Output Format ###
  if(units.out == "pg") {
    value.out <- value.g * 1E12
  } else if(units.out == "ng") {
    value.out <- value.g * 1E9
  } else if(units.out == "ug") {
    value.out <- value.g * 1E6
  } else if(units.out == "mg") {
    value.out <- value.g * 1E3
  } else if(units.out == "g") {
    value.out <- value.g
  } else if(units.out == "kg") {
    value.out <- value.g / 1E3
  } else if(units.out == "stone") {
    value.out <- value.g / 6.37E3
  } else {
    value.out <- NA
  }
  
  ### Output Value ###
  return(value.out)
}