library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)

#### Getting Player Info ####

## From NHL API
seasons_func <- function(x) {
  m <- vector("integer", 10)
  for(i in 1:10) {
    m[i] <- x - ((i-1) * 10001) 
  }
  return(m)
}

seasons <- seasons_func(20242025) %>%
  as.character()

## Getting skater info for these seasons

skaters_by_season <- data.frame()
goalies_by_season <- data.frame()

process_skater_data <- function(x) {
  global_vars <- "skaters_by_season"
  
  apiurl <- "https://api.nhle.com/stats/rest/en/skater/summary?limit=-1&cayenneExp=seasonId="
  season_id <- seasons[x]
  
  # call information on the skaters for the season
  getskaterinfo <- GET(paste0(apiurl,season_id))
  skaterinfo <- rawToChar(getskaterinfo$content) %>%
    fromJSON %>%
    '[['("data")
  #assign it to the global variable
  assign("skaters_by_season", rbind(get("skaters_by_season", envir = .GlobalEnv), skaterinfo), envir = .GlobalEnv)
}

#Run the function
for (i in 1:length(seasons)) {
  process_skater_data(i)
}

process_goalie_data <- function(x) {
  global_vars <- "goalies_by_season"
  
  apiurl <- "https://api.nhle.com/stats/rest/en/goalie/summary?limit=-1&cayenneExp=seasonId="
  season_id <- seasons[x]
  
  # call information on the skaters for the season
  getgoalieinfo <- GET(paste0(apiurl,season_id))
  goalieinfo <- rawToChar(getgoalieinfo$content) %>%
    fromJSON %>%
    '[['("data")
  #assign it to the global variable
  assign("goalies_by_season", rbind(get("goalies_by_season", envir = .GlobalEnv), goalieinfo), envir = .GlobalEnv)
}

#Run the function
for (i in 1:length(seasons)) {
  process_goalie_data(i)
}

goalies_by_season$positionCode = "G"

#write.csv(skaters_by_season, file = "Datasets/skaterdata.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
#write.csv(goalies_by_season, file = "Datasets/goaliedata.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")

## File encoding is helpful here because of special characters in names

## From Natural Stat Trick
## Scraping Data from natural stat Trick

# Rate limits are as follows
# 40 requests in 1 minute
# 75 requests in 5 minutes
# 100 requests in 15 minutes
# 180 requests in 1 hour

## Creating variables to iterate over different versions of the NTS link
rates <- cbind(c("y","n"),c("rates","counts"))
stdois <- cbind(c("std", "oi"),c("ind","onice"))
sits <- c("pp", "5v5", "all")

combined_tables <- list()

scrapedata <- function(i, j, x, y) {
  season <- seasons[i]
  rate <- rates[j, 1]
  stdoi <- stdois[x, 1]
  sit <- sits[y]
  
  table_id <- ifelse(stdoi == "oi", "players", "indreg")
  
  #opening the webpage
  webpage <- read_html(
    paste0("https://www.naturalstattrick.com/playerteams.php?fromseason=",
           season,
           "&thruseason=",
           season,
           "&stype=2&sit=",
           sit,
           "&score=all&stdoi=",
           stdoi,
           "&rate=",
           rate,
           "&team=ALL&pos=S&loc=B&toi=0&gpfilt=none&fd=&td=&tgp=410&lines=single&draftteam=ALL")
  )
  
  ## Grabbing the table from the page
  data <- webpage %>%
    html_nodes(paste0("table#", table_id)) %>%
    html_table() %>% 
    .[[1]] %>%
    select(-which(names(.) == "")) %>%
    mutate("season" = season, "rate" = rate, "stdoi" = stdoi, "sit" = sit) ## adding parameters
  
  data <- type_convert(data, na = c("", "NA", "-", "inf")) ## making sure N/A are captured correctly in their various forms
  
  return(data)
}

# Iterate over all combinations and collect data
for (j in 1:nrow(rates)) {
  for (x in 1:nrow(stdois)) {
    for (y in 1:length(sits)) {
      combination_data <- list()  # Temporary list for each combination
      
      for (i in 1:length(seasons)) {
        Sys.sleep(20)  # Respect website's rate limit
        season_data <- scrapedata(i, j, x, y)
        combination_data <- append(combination_data, list(season_data))
      }
      
      # Combine all season data for the current combination
      combined_table <- bind_rows(combination_data)
      key <- paste(rates[j, 2], stdois[x, 2], sits[y], sep = "_")
      combined_tables[[key]] <- combined_table
    }
  }
}

rm(combined_table)

## Save to CSV
#for(i in 1:length(combined_tables)){
#  write.csv(combined_tables[[i]], paste0("Datasets/NTS Data/", names(combined_tables)[i], ".csv"), row.names = FALSE, fileEncoding = "ISO-8859-1")
#}
