library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)
library(here)

## All of the data is stored in folders in the root folder of the project
setwd(dirname(here()))

## Creating list of IDs we need to pull from NHL API

widedf <- read.csv("Datasets/all_data_wide_wid.csv", 
                   fileEncoding = "ISO-8859-1", 
                   check.names = FALSE)

ids <- widedf |> 
    select(playerId) |>
  distinct() |>
  pull()


## Test

getinfo <- GET("https://api-web.nhle.com/v1/player/8478402/landing")
info <- rawToChar(getinfo$content) %>%
  fromJSON
  
str(info)

rm(getinfo)
rm(info)


## not much useful here other than maybe the birthdate, so let's grab that I guess

skaterbirthdates <- data.frame()
for (id in ids) {
  
  df <- data.frame()
  
  apiurl <- "https://api-web.nhle.com/v1/player/"
  
  
  getinfo <- GET(paste0(apiurl, id, "/landing")) 
  info <- rawToChar(getinfo$content) %>%
    fromJSON
  
  df <- data.frame(playerId = info$playerId,
                   birthdate = info$birthDate)
  
  skaterbirthdates <- rbind(skaterbirthdates, df)
}

ids_df <- data.frame(playerId = ids)

missing <- ids_df |>
  anti_join(skaterbirthdates, by = "playerId")

str(missing) #none missing

widedf_wbday <- widedf |>
  left_join(skaterbirthdates, by = "playerId") |>
  relocate(birthdate, .after = season) |>
  mutate(age = as.numeric(substring(widedf_wbday$season,1,4)) - 
           as.numeric(substring(widedf_wbday$birthdate,1,4))) |>
  relocate(age, .after = birthdate)
  
write.csv(widedf_wbday, file = "Datasets/widedf_full.csv", 
          fileEncoding = "ISO-8859-1", 
          row.names = FALSE)
