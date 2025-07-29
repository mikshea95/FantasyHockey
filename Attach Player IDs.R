library(tidyverse)
library(fuzzyjoin)
library(here)

## All of the data is stored in folders in the root folder of the project
setwd(dirname(here()))

### Getting player IDs onto the natural stat trick data

alldata_wide <- read.csv("Datasets/all_data_wide.csv", fileEncoding = "ISO-8859-1", check.names = FALSE)

skaters_by_season <- read.csv("Datasets/skaterdata.csv", fileEncoding = "ISO-8859-1")

## List of skaters from natural stat trick data
skaters <- alldata_wide %>%
  select(Player, Team, Position, season) %>%
  distinct(Player, Position, season, .keep_all = TRUE)



skaters_duplicates <- skaters %>%
  group_by(Player, season) %>%
  summarise(n = n()) %>%
  filter(n > 1)

print(skaters_duplicates)
## Need to worry about Sebastian Ahos and Elias Pettersons




## List of skaters from NHL API
nhl_skater_names <- skaters_by_season %>%
  select("Player" = "skaterFullName", playerId, "Position" = "positionCode") %>%
  group_by(Player, playerId) %>%
  distinct() %>%
  ungroup()

## Fuzzy matching because the names are not 1:1. this will save some time 
fuzzyjoin_player <- stringdist_left_join(
  skaters, nhl_skater_names,
  by = "Player", 
  method = "jw",  
  max_dist = 0.33,  
  distance_col = "dist") 



## running the antti join until it returns zero. Trying to get the fuzzy match to try for everyone
fuzzyanti_join <- fuzzyjoin_player %>%
  filter(is.na(playerId))

head(fuzzyanti_join)



## only showing the best match for each player
fuzzyjoin_df <- fuzzyjoin_player %>% 
  group_by(Player.x, Position.x) %>%
  slice_min(order_by = dist, with_ties = FALSE) %>%
  ungroup() 

head(fuzzyjoin_df %>%
       arrange(desc(dist)))

#write.csv(fuzzyjoin_df, file = "fuzzyjoin.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")



## Make edits directly in the file to quality check the fuzzy matches


#rm(fuzzyjoin_df)
#rm(fuzzyjoin_player)
#rm(fuzzyanti_join)

combo_player_wids <- read.csv(file = "Datasets/fuzzyjoin.csv", fileEncoding = "ISO-8859-1")

nts_player_wids <- combo_player_wids %>%
  select("Player" = "Player.x", Team, "Position" = "Position.x", playerId)

nhl_player_wids <- combo_player_wids %>%
  select("Player" = "Player.y", Team, "Position" = "Position.y", playerId)

combo_player_wids <- rbind(nts_player_wids, nhl_player_wids) %>%
  distinct(Player, Team, Position, .keep_all = TRUE)

#write.csv(combo_player_wids, "playeridindex.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")

df_wide_with_id <- alldata_wide |>
  left_join(combo_player_wids, by = c("Player", "Team", "Position")) |>
  relocate(playerId)

write.csv(df_wide_with_id, file = "Datasets/all_data_wide_wid.csv", fileEncoding = "ISO-8859-1", row.names = FALSE)