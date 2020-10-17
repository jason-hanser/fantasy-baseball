##

library(rvest)
library(parallel)
library(tidyr)

setwd("C:/Users/hanserjt.CAMPUS/Google Drive/Institutional Effectiveness Home/Jason/R Library/Fantasy Baseball Project")




## Getting list of leagues to search for

brute_force_list           <- as.data.frame(seq(1:500000))
colnames(brute_force_list) <- c("League_ID")
brute_force_list$ESPN      <- paste("http://games.espn.com/flb/leaguesetup/settings?leagueId=", brute_force_list$League_ID, sep = "")









## ESPN Scraper

Brute_Force_ESPN_Scrape <- function(Search_Sites) {
  
  library(rvest)
  library(parallel)
  
  t1           <- data.frame(Search_Sites$League_ID)
  colnames(t1) <- c("League_ID")
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  for (i in 1:length(Search_Sites$ESPN)) {
  
    tryCatch(
      read_html(Search_Sites$ESPN[i]) -> league_site,
      error = function(e){league_site <<- NA})
      
    if (is.na(league_site) == TRUE){
      
      t1$League_Size[i]     <- "Read_HTML_Error"
      t1$Scoring_System[i]  <- "Read_HTML_Error"
      t1$Format[i]          <- "Read_HTML_Error"
      t1$Player_Univ[i]     <- "Read_HTML_Error"
      t1$Draft_Type[i]      <- "Read_HTML_Error"
      t1$Draft_Date[i]      <- "Read_HTML_Error"
      t1$League_Viewable[i] <- "Read_HTML_Error"
      t1$Season_Start[i]    <- "Read_HTML_Error"
      Sys.sleep(60*60)
      
    } else {
    
      league_site %>%
        html_nodes('table[class="leagueSettingsTable tableBody"]') -> league_settings
  
      league_settings %>%
        html_nodes('tr:contains(Number):contains(Teams) td div.viewable') %>%
        html_text() -> temp_league_size
      
      league_settings %>%
        html_nodes('tr:contains(Scoring):contains(Type) td:nth-of-type(2)') %>%
        html_text() -> temp_scoring_system
      
      league_settings %>%
        html_nodes('tr:contains(Format) td:nth-of-type(2)') %>%
        html_text() -> temp_league_format
    
      league_settings %>%
        html_nodes('tr:contains(Player):contains(Universe) div') %>%
        html_text() -> temp_player_univ
      
      league_settings %>%
        html_nodes('tr:contains(Draft):Contains(Type) td:nth-of-type(2)') %>%
        html_text() -> temp_draft_type
      
      league_settings %>%
        html_nodes('tr:contains(Draft):contains(Date) td div:contains("2017")') %>%
        html_text() -> temp_draft_date
      
      league_settings %>%
        html_nodes('tr:contains(Make):contains(League) td:contains("Yes")') %>%
        html_text() -> temp_viewable
      
      league_settings %>%
        html_nodes('tr:contains(Start):contains(Regular):contains(Season) td:contains("Week")') %>%
        html_text() -> temp_season_start
      
      
      t1$League_Size[i]     <- ifelse(length(temp_league_size) == 0, "NULL", temp_league_size)
      t1$Scoring_System[i]  <- ifelse(length(temp_scoring_system) == 0, "NULL", temp_scoring_system)
      t1$Format[i]          <- ifelse(length(temp_league_format) == 0, "NULL", temp_league_format)
      t1$Player_Univ[i]     <- ifelse(length(temp_player_univ) == 0, "NULL", temp_player_univ)
      t1$Draft_Type[i]      <- ifelse(length(temp_draft_type) == 0, "NULL", temp_draft_type)
      t1$Draft_Date[i]      <- ifelse(length(temp_draft_date) == 0, "NULL", temp_draft_date)
      t1$League_Viewable[i] <- ifelse(length(temp_viewable) == 0, "NULL", temp_viewable)
      t1$Season_Start[i]    <- ifelse(length(temp_season_start) == 0, "NULL", temp_season_start)
      
      rm(league_site, league_settings)
      rm(temp_league_size, temp_scoring_system, temp_league_format, temp_player_univ, temp_draft_type, temp_draft_date, temp_viewable, temp_season_start)
      }
    }
  rm(i)
  
  Output <- t1

}






## Arranging brute_force_list (possible league id's) into four, evenly-sized 
## dataframes - all contained within one list

data <- brute_force_list

groups   <- rep(1:4, length.out = length(data$League_ID))
data$group <- groups
rm(groups)

x1 <- data[data$group == 1, ]
x2 <- data[data$group == 2, ]
x3 <- data[data$group == 3, ]
x4 <- data[data$group == 4, ]

x_list <- list(x1, x2, x3, x4)
rm(x1, x2, x3, x4)
rm(data)








## Setting up Clusters & Running Brute Force Scrape

clusters <- makeCluster(detectCores())

ESPN_output <- parLapply(clusters, x_list, Brute_Force_ESPN_Scrape)

brute_force_results <- rbind(ESPN_output[[1]], ESPN_output[[2]], ESPN_output[[3]], ESPN_output[[4]])
write.csv(brute_force_results, "brute_force_results.csv")
















