##

setwd("C:/Users/hanserjt.CAMPUS/Google Drive/Institutional Effectiveness Home/Jason/R Library/Fantasy Baseball Project")

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)



############################################################
###### Step 1: Getting a list of batters and pitchers ######
############################################################

## Getting a list of batters and their game logs sites to scrape (at least 1 hit in 2017)

All_Batters <- read_html("https://www.baseball-reference.com/leagues/daily.fcgi?request=1&type=b&dates=lastndays&lastndays=365&since=2018-02-01&fromandto=2018-02-01.2018-02-28&level=mlb&franch=ANY&stat=b%3AH&stat_value=1")

All_Batters %>%
  html_nodes("table.sortable tr a[title = 'Game Logs']") %>%
  html_attr("href") %>%
  as.data.frame() -> temp_link

All_Batters %>%
  html_nodes("table.sortable tr td.left[data-stat='player']") %>%
  html_attr("csk") %>%
  as.data.frame() -> temp_player

All_Batters <- data.frame(Players = temp_player, 
                          Site    = temp_link
                          )

 colnames(All_Batters) <- c("Players", "Site")

All_Batters$Players <- as.character(All_Batters$Players)
All_Batters$Site    <- as.character(All_Batters$Site)
                          
All_Batters$Site <- paste0("https://www.baseball-reference.com", All_Batters$Site)

rm(temp_link, temp_player)



## Getting a list of pitchers and their game logs sites to scrape (at least 5 IP in 2017)

All_Pitchers <- read_html("https://www.baseball-reference.com/leagues/daily.fcgi?request=1&type=p&dates=lastndays&lastndays=365&since=2018-02-01&fromandto=2018-02-01.2018-02-28&level=mlb&franch=ANY&stat=p%3AIP&stat_value=1")

All_Pitchers %>%
  html_nodes("table.sortable tr a[title = 'Game Logs']") %>%
  html_attr("href") %>%
  as.data.frame() -> temp_link

All_Pitchers %>%
  html_nodes("table.sortable tr td.left[data-stat='player']") %>%
  html_attr("csk") %>%
  as.data.frame() -> temp_player


All_Pitchers <- data.frame(Players = temp_player, 
                          Site    = temp_link
)

colnames(All_Pitchers) <- c("Players", "Site")

All_Pitchers$Players <- as.character(All_Pitchers$Players)
All_Pitchers$Site    <- as.character(All_Pitchers$Site)

All_Pitchers$Site <- paste0("https://www.baseball-reference.com", All_Pitchers$Site)

rm(temp_link, temp_player)




###########################################
###### Step 2: Building the Scrapers ######
###########################################

## Scraping batting game logs

Batter_Scrape <- function(data) {
 
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  library(rvest)
  library(parallel)
  library(stringr)
  
  Game_Logs <- data.frame()
  
  for (i in 1:nrow(data)) {
    
  temp_site <- read_html(data$Site[i])
      
  temp_site %>%
    html_nodes("table#batting_gamelogs_milb") %>%
    html_table() %>%
    as.data.frame() -> temp_table
  
  temp_table        <- temp_table[temp_table$PA != "PA", ]
  temp_table$Player <- data$Players[i]
  
  Game_Logs <- rbind(Game_Logs, temp_table)
  
  rm(temp_site, temp_table)

  }
   
  Output <<- Game_Logs
  
}
  
  


Pitcher_Scrape <- function(data) {
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  library(rvest)
  library(parallel)
  library(stringr)
  
  Game_Logs <- data.frame()
  
  for (i in 1:nrow(data)) {
    
    temp_site <- read_html(data$Site[i])
    
    temp_site %>%
      html_nodes("table#pitching_gamelogs_milb") %>%
      html_table() %>%
      as.data.frame() -> temp_table
    
    temp_table$Player <- data$Players[i]
    
    Game_Logs <- rbind(Game_Logs, temp_table)
    
    rm(temp_site, temp_table)

  }
  
  Output <<- Game_Logs
  
}


  
##########################################
###### Step 3: Running the Scrapers ######
##########################################

Batter_Scrape(data = All_Batters)

Batter_Game_Logs <- Output
rm(Output, All_Batters, Batter_Scrape)



Pitcher_Scrape(data = All_Pitchers)

Pitcher_Game_Logs <- Output
rm(Output, All_Pitchers, Pitcher_Scrape)



###########################################
###### Step 4: Some Initial Cleaning ######
###########################################

Batter_Game_Logs  <- Batter_Game_Logs[Batter_Game_Logs$Lev %in% c("MLB-NL", "MLB-AL"), ]
Pitcher_Game_Logs <- Pitcher_Game_Logs[Pitcher_Game_Logs$Lev %in% c("MLB-NL", "MLB-AL"), ]

Batter_Game_Logs  <- Batter_Game_Logs[, c(26, 2:25) ]
Pitcher_Game_Logs <- Pitcher_Game_Logs[, c(36, 2:35)]



##########################################
###### Step 5: Writing Data to File ######
##########################################

write.csv(Batter_Game_Logs, 
          "Scraped Data/Baseball Reference Scrape/Batter_Game_Logs.csv", 
           row.names = FALSE)

write.csv(Pitcher_Game_Logs, 
          "Scraped Data/Baseball Reference Scrape/Pitcher_Game_Logs.csv", 
          row.names = FALSE)


