library(tidyverse)
library(fuzzyjoin)
library(here)

## All of the data is stored in folders in the root folder of the project
setwd(dirname(here()))

## Data Processing
### Converting the data to long form which may be easier lookups and transformations

filenames <- list.files("Datasets/NTS Data") # get file names

# read data
combined_tables <- lapply(filenames, function(file) {
  read.csv(paste0("Datasets/NTS Data/",file), fileEncoding = "ISO-8859-1", check.names = FALSE)  
})

#replace names
combined_tables <- setNames(combined_tables, gsub("\\.csv$", "", filenames))


## Convert data to long format for ease of transformation
combined_tables_long <- list()
for (name in names(combined_tables)) {
  long_df <- combined_tables[[name]] %>%
    pivot_longer(cols = !c("Player", "Team", "Position", "season", "rate", "stdoi", "sit"),
                 names_to = "stat",
                 values_to = "value")
  
  combined_tables_long[[name]] <- long_df
}

rm(long_df)

all_data_long <- bind_rows(combined_tables_long)

# deciding to use rate data only here so it doesn't become unwiedly. We can always calculate the actual stats later
long_df <- all_data_long

long_df <- all_data_long |>
  filter(rate == "y") |>
  select(-rate) |>
  arrange(Player, Team, Position, season, stdoi, sit, stat, value) |>
  filter(!stat %in% c("GP", "TOI")) |> 
  distinct()

allstats_wide_df <- long_df |>
  pivot_wider(id_cols = c("Player", "Team", "Position", "season"),
              names_from = c("sit", "stdoi", "stat"),
              values_from = value)


toidf <- all_data_long |>
  filter(rate == "y") |>
  filter(stat == "TOI" | stat == "TOI/GP") |>
  select(-stdoi, -rate) |> 
  distinct() |>
  pivot_wider(id_cols = c("Player", "Team", "Position", "season"),
              names_from = c("sit", "stat"),
              values_from = value)

wide_df <- all_data_long |>
  filter(rate == "y" & stat == "GP") |>
  select(Player, Team, Position, season, GP = value) |>
  distinct() |>
  left_join(toidf, by = c("Player", "Team", "Position", "season")) |>
  left_join(allstats_wide_df, by = c("Player", "Team", "Position", "season"))

write.csv(wide_df, file = "Datasets/all_data_wide.csv", fileEncoding = "ISO-8859-1", row.names = FALSE)



