# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(data.table)
library(nflreadr)
library(openxlsx)

roster_dir <- "App/data/"
roster_file <- "Playoff Fantasy Rosters for 2024-2025, Compiled 2025-01-14 085019.csv"

rosters <- fread(file = paste0(roster_dir,roster_file))
rosters[,team_abbr:=NULL]

rosters[,position_code:=ifelse(position_code=="D","Defense",position_code)]

if("Fantasy Owner Email" %in% names(rosters)){
  rosters[,`Fantasy Owner Email`:=NULL] # remove PII before further joins
}

stats_dir <- "App/data/"
stats_file_players <- "stats_2024_REG_POST_gen2025-01-27 091945.csv"
stats_players <- fread(file = paste0(stats_dir,stats_file_players))
stats_players[,player_id:=ifelse(position=="Defense",team_abbr,player_id)]

scored_rosters <- merge.data.table(stats_players[season_type == "Post"], rosters, by = "player_id", all.y = TRUE, allow.cartesian=TRUE)

scored_rosters <- scored_rosters[stat_type=="fantasy_points"]

output_file <- paste0(
  "App/data/NFL Fantasy Scores for 2024-2025 as of ",
  str_remove_all(str_sub(Sys.time(),1,19), ":"),
  ".csv"
)

fwrite(scored_rosters, output_file)

