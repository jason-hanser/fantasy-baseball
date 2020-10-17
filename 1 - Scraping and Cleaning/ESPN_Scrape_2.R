##

library(rvest)
library(parallel)
library(tidyr)

setwd("C:/Users/hanserjt.CAMPUS/Google Drive/Institutional Effectiveness Home/Jason/R Library/Fantasy Baseball Project/Scraped Data")




#####################################
###### Setting up the Scrapers ######
#####################################


League_Index_Scraper <- function(data) {
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  library(rvest)
  library(parallel)
  library(stringr)
  
  data      <- data.frame(League_ID = data[, c("League_ID")])
  data$Site <- paste("http://games.espn.com/flb/leagueoffice?leagueId=", 
                     data$League_ID,
                     "&seasonId=2017", 
                        sep = "")
  
  League_Index <- data.frame()
  
  for (i in 1:nrow(data)) {
    
    temp_site <- html_err(data$Site[i])
    
    temp_site %>%
      html_nodes('div.info-area:contains(Index)') %>%
      html_text() %>%
      str_extract('(?<=Index: )[\\s\\S]*') %>%
      str_extract('[\\s\\S]*(?=Leaderboard)') -> Index


    temp_Index <- data.frame(League_ID    = data$League_ID[i],
                             League_Index = ifelse(length(Index) == 0, "NULL", Index))
    
    League_Index <- rbind(League_Index, temp_Index)
    
    rm(temp_Index, Index)
    print(i)
    }
  
  rm(i)
  
  Output <<- League_Index
  
}




Draft_Scraper <- function(data) {

  library(rvest)
  library(parallel)
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  data      <- data.frame(League_ID = data[, c("League_ID")])
  
  data$Site <- paste("http://games.espn.com/flb/tools/draftrecap?leagueId=", 
                     data$League_ID, 
                     "&mode=1", 
                     sep = "")
  
  Draft_Results <- data.frame()
  
  html_err <- function(x)  {
    temp_site <- try(read_html(as.character(x)), silent = TRUE)
    if(class(temp_site)[1] == "try-error") {
      print("Sleeping")
      Sys.sleep(60*15)
      return(html_err(x))
    }
    return(temp_site)
  }
  
  for (i in 1:nrow(data)){
    
    temp_site <- html_err(data$Site[i])
    
    temp_site %>%
      html_nodes('table') %>%
      html_table(fill = TRUE) -> temp_Tables
    
    temp_Draft_1 <- data.frame()
    
    for (j in 1:length(temp_Tables)){
      
      temp_Draft_2 <- temp_Tables[[j]][, 1:2]
      
      if (nrow(temp_Draft_2) != 26){
        rm(temp_Draft_2)
        
      } else {
        temp_Draft_2$Team      <- temp_Draft_2[1, 1]
        temp_Draft_2$League    <- data$League_ID[i]
        temp_Draft_2           <- temp_Draft_2[2:26, 1:4]
        colnames(temp_Draft_2) <- c("Draft_Pick", "Player", "Team", "League")
        
        temp_Draft_1           <- rbind(temp_Draft_1, temp_Draft_2)
        rm(temp_Draft_2)
      }
      
    }
    rm(j)

    Draft_Results <- rbind(Draft_Results, temp_Draft_1)
    rm(temp_Draft_1)
    
    print(i)
    
  }
  rm(i)
  
  Output <<- Draft_Results
}



Final_Roster_Scraper <- function(data) {
  
  library(rvest)
  library(parallel)
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  data      <- data.frame(League_ID = data[, c("League_ID")])
  data$Site <- paste("http://games.espn.com/flb/leaguerosters?leagueId=", 
                     data$League_ID, 
                        sep = "")
  
  Final_Rosters <- data.frame()
  
  html_err <- function(x)  {
    temp_site <- try(read_html(as.character(x)), silent = TRUE)
    if(class(temp_site)[1] == "try-error") {
      print("Sleeping")
      Sys.sleep(60*15)
      return(html_err(x))
    }
    return(temp_site)
  }
  
  for (i in 1:nrow(data)) {
    
    temp_site <- html_err(data$Site[i])
    
    temp_site %>%
      html_nodes('table.playerTableTable.tableBody') %>%
      html_table(fill = TRUE) -> temp_Tables
    
      temp_Rosters  <- data.frame()
    
      for (j in 1:length(temp_Tables)) {
        
        temp_Roster           <- temp_Tables[[j]][2:nrow(temp_Tables[[j]]), ]
        temp_Roster$Team      <- colnames(temp_Roster[1])
        temp_Roster$League_ID <- data$League_ID[i]
          colnames(temp_Roster) <- c("Slot", "Player", "Acquired", "Team", "League_ID")
        
        temp_Rosters <- rbind(temp_Rosters, temp_Roster)
          rm(temp_Roster)
      }
      rm(j)
      
      Final_Rosters <- rbind(Final_Rosters, temp_Rosters)
        rm(temp_Rosters, temp_Tables, temp_site)
        
      print(i)
    
    }
  rm(i)
  
  Output <<- Final_Rosters
  
}





Matchup_Scraper <- function(data) {
  
  library(rvest)
  library(parallel)
  library(stringr)
  library(tidyr)
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  data           <- data.frame(data[, c("League_ID")])
  data           <- merge(data, 1:21)
  colnames(data) <- c("League_ID", "Week_ID")
  
  data$Site      <- paste("http://games.espn.com/flb/scoreboard?leagueId=",
                          data$League_ID,
                          "&seasonId=2017&matchupPeriodId=",
                          data$Week_ID,
                            sep = "")
  
  Matchups <- data.frame()
  
  html_err <- function(x)  {
    temp_site <- try(read_html(as.character(x)), silent = TRUE)
    if(class(temp_site)[1] == "try-error") {
      print("Sleeping")
      Sys.sleep(60*15)
      return(html_err(x))
    }
    return(temp_site)
  }
  
  for (i in 1:nrow(data)) {
    
    temp_site <- html_err(data$Site[i])
    
    temp_site %>%
      html_nodes('table.tableBody') %>%
      html_table(fill = TRUE) %>%
      as.data.frame() -> temp_Table
  
    temp_Table <- temp_Table[, c(1, 3:7, 9:13)]    
    temp_Table <- temp_Table[temp_Table$X1 != "", ]
    temp_Table <- temp_Table[temp_Table$X1 != "Full Box Score", ]
    temp_Table <- temp_Table[temp_Table$X3 != "BATTERS", ]
      colnames(temp_Table) <- as.character(temp_Table[1, ])
      
    temp_Table <- temp_Table[temp_Table$R != "R", ]
      row.names(temp_Table) <- 1:10
    
    temp_Table$League_ID <- data$League_ID[i]
    temp_Table$Week      <- str_extract(data$Site[i], '(?<=matchupPeriodId=)[\\s\\S]*')
    
    temp_Table$Oppenent[1]  <- temp_Table$NAME[2]
    temp_Table$Oppenent[2]  <- temp_Table$NAME[1]
    temp_Table$Oppenent[3]  <- temp_Table$NAME[4]
    temp_Table$Oppenent[4]  <- temp_Table$NAME[3]
    temp_Table$Oppenent[5]  <- temp_Table$NAME[6]
    temp_Table$Oppenent[6]  <- temp_Table$NAME[5]
    temp_Table$Oppenent[7]  <- temp_Table$NAME[8]
    temp_Table$Oppenent[8]  <- temp_Table$NAME[7]
    temp_Table$Oppenent[9]  <- temp_Table$NAME[10]
    temp_Table$Oppenent[10] <- temp_Table$NAME[9]
    
    Matchups <- rbind(Matchups, temp_Table)
      rm(temp_Table, temp_site)
    
  }
  rm(i)
  
  Output   <<- Matchups
  
}




AB_IP_Scraper <- function(data) {
  
  library(rvest)
  library(parallel)

  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  data <- data.frame(League_ID = data[, c("League_ID")])
  data <- merge(data, 1:10)
  
  Week_ID <- data.frame(id = c(8, 15, 22, 29, 36, 43, 50, 57, 69, 71, 78, 85, 92, 106, 113, 120, 127, 134, 141, 148, 155))
  data    <- merge(data, Week_ID)
  rm(Week_ID)
  
  colnames(data) <- c("League_ID", "Team_ID", "Week_ID")
  
  data$Site <- paste("http://games.espn.com/flb/boxscorefull?leagueId=",
                     data$League_ID,
                     "&teamId=",
                     data$Team_ID,
                     "&scoringPeriodId=",
                     data$Week_ID,
                     "&seasonId=2017&view=matchup&version=full",
                        sep = "")

  AB_IP <- data.frame()
  
  for (i in 1:nrow(data)) {
    
    tryCatch(
      read_html(data$Site[i]) %>%
        html_nodes('table.maincontainertbl') %>%
        html_table(fill = TRUE) %>%
        as.data.frame() -> temp_Table,
      error = function(e) {temp_Table <<- NA})
    
    if (is.na(temp_Table) == TRUE) {
      
      print("Sleep")
      Sys.sleep(60*60)
      
    } else {

      x <- temp_Table[temp_Table$X1 == "", c("X1", "X3", "X4")]
      x <- x[1:2, ]
      
      x$Category[1] <- "AB"
      x$Total[1]    <- x$X4[1]
      x$Category[2] <- "IP"
      x$Total[2]    <- x$X3[2]
        
      x <- x[, c("Category", "Total")]
        
      x$Team      <- temp_Table[6, c("X1")]
      x$Team_ID   <- data$Team_ID[i]
      x$League_ID <- data$League_ID[i]
      x$Week_ID   <- data$Week_ID[i]
        rm(temp_Table)
      
      AB_IP <- rbind(AB_IP, x)
        rm(x)
        print(i)
        
    }
        
  }
  rm(i)
  
  Output <<- AB_IP
  
}




Week_19_Roster_Scraper <- function(data) {
  
  library(rvest)
  library(parallel)
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  Teams <- data.frame(League_ID = data[, c("League_ID")])
  Teams <- merge(Teams, 1:10)
  
  colnames(Teams) <- c("League_ID", "Team_ID")
  
  Teams$Site <- paste("http://games.espn.com/flb/boxscorefull?leagueId=", 
                      Teams$League_ID,
                      "&teamId=",
                      Teams$Team_ID,
                      "&scoringPeriodId=141&seasonId=2017&view=matchup&version=full",
                          sep = "")
  
  Week_19_Rosters <- data.frame()
  
  html_err <- function(x)  {
    temp_site <- try(read_html(as.character(x)), silent = TRUE)
    if(class(temp_site)[1] == "try-error") {
      print("Sleeping")
      Sys.sleep(60*15)
      return(html_err(x))
    }
    return(temp_site)
  }
  
  for (i in 1:nrow(Teams)) {
  
    temp_site <- html_err(Teams$Site[i])
    
    temp_site %>%
      html_nodes('table.playerTableTable.tableBody') %>%
      html_table(fill = TRUE) -> temp_Tables
      
      temp_Batters  <- data.frame(League_ID = Teams$League_ID[i],
                                  Team_ID   = Teams$Team_ID[i],
                                  Team_Name = temp_Tables[[1]][1,1],
                                  Role      = "Batter",
                                  Player    = temp_Tables[[1]][,1])
      
      temp_Pitchers <- data.frame(League_ID = Teams$League_ID[i],
                                  Team_ID   = Teams$Team_ID[i],
                                  Team_Name = temp_Tables[[1]][1,1],
                                  Role = "Pitcher",
                                  Player = temp_Tables[[2]][,1])
      
      temp_Roster <- rbind(temp_Batters, temp_Pitchers)
      
      Week_19_Rosters <- rbind(Week_19_Rosters, temp_Roster)
      
      rm(temp_Batters, temp_Pitchers, temp_Tables, temp_Roster, temp_site)
    
  }

  Output <<- Week_19_Rosters 
  
}


League_Winner_Scraper <- function(data) {
  
  library(rvest)
  library(parallel)
  
  options(HTTPUserAgent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
  
  Leagues <- data.frame(League_ID = data[, c("League_ID")])
  
  Leagues$Site <- paste("http://games.espn.com/flb/leagueoffice?leagueId=",
                        Leagues$League_ID,
                        "&seasonId=2017",
                        sep = "")
  
  Playoffs <- data.frame()
  
  html_err <- function(x)  {
    temp_site <- try(read_html(as.character(x)), silent = TRUE)
    if(class(temp_site)[1] == "try-error") {
      print("Sleeping")
      Sys.sleep(60*15)
      return(html_err(x))
    }
    return(temp_site)
  }

  for (i in 1:nrow(Leagues)) {
    
    temp_site <- html_err(Leagues$Site[i])
    
    temp_site %>%
      html_nodes('div.champ-info-wrap strong:first-of-type') %>%
      html_text() %>%
      data.frame() -> temp_winner
    
    colnames(temp_winner) <- c("Winner")
    temp_winner$League    <- Leagues[i, 1]
    
    Playoffs <- rbind(Playoffs, temp_winner)
    
    rm(temp_winner, temp_site)
    
  }
  
  Output <<- Playoffs 
  
}
















##################################
###### Running the Scrapers ######
##################################


## Reading in Data and doing some initial cleaning

Leagues <- read.csv("brute_force_results.csv")

Leagues <- Leagues[Leagues$League_Size     == 10 &
                   Leagues$Format          == "Standard" & 
                   Leagues$Player_Univ     == "All MLB" &
                   Leagues$Draft_Type      == "Snake" &
                   Leagues$League_Viewable == "Yes" &
                   Leagues$Draft_Date      != "NULL" &
                   Leagues$Scoring_System  == "Head to Head Each Category" &
                   Leagues$Season_Start    == "Week 1 (Start of Season)", 2:10]

Leagues            <- separate(Leagues, Draft_Date , c("Date_1", "Date_2", "Date_3"), sep = ",")
Leagues$Date_3     <- substr(trimws(Leagues$Date_3), 1, 4)
Leagues$Draft_Date <- paste(trimws(Leagues$Date_2), Leagues$Date_3, sep = " ")
Leagues            <- Leagues[, c(1:6, 10:12)]
Leagues$Draft_Date <- as.Date(Leagues$Draft_Date, format = "%B %d %Y")

Leagues <- Leagues[Leagues$Draft_Date < "2017-04-02", ]

Leagues <- Leagues[!(Leagues$League_ID %in% c(76378, 35688)), ]

write.csv(Leagues, "Leagues.csv", row.names = FALSE)




## Setting up Leagues data frame for parallel computing

Leagues               <- data.frame(Leagues, 
                                    group = rep(1:4, length.out = nrow(Leagues)))

Leagues_list <- list(Leagues[Leagues$group == 1,],
                     Leagues[Leagues$group == 2,],
                     Leagues[Leagues$group == 3,],
                     Leagues[Leagues$group == 4,])

clusters <- makeCluster(detectCores())




## Running the League Index Scraper

x            <- parLapply(clusters, Leagues_list, League_Index_Scraper)
League_Index <- rbind(x[[1]], x[[2]], x[[3]], x[[4]])

write.csv(League_Index, "League_Index.csv", row.names = FALSE)




## Running the Draft Result Scraper

x             <- parLapply(clusters, Leagues_list, Draft_Scraper)
Draft_results <- rbind(x[[1]], x[[2]], x[[3]], x[[4]])

write.csv(Draft_results, "Draft_results.csv", row.names = FALSE)




## Running the Final roster Scraper

x             <- parLapply(clusters, Leagues_list, Final_Roster_Scraper)
Final_Rosters <- rbind(x[[1]], x[[2]], x[[3]], x[[4]])
  
write.csv(Final_Rosters, "Final_Rosters.csv", row.names = FALSE)




## Running Match Up Scraper

x        <- parLapply(clusters, Leagues_list, Matchup_Scraper)
Matchups <- rbind(x[[1]], x[[2]], x[[3]], x[[4]])

write.csv(Matchups, "Matchups.csv", row.names = FALSE)




## Running the AB_IP_Scraper

x     <- parLapply(clusters, Leagues_list, AB_IP_Scraper)
AB_IP <- rbind(x[[1]], x[[2]], x[[3]], x[[4]])

write.csv(AB_IP, "AB_IP_4.csv", row.names = FALSE)




## Setting up data for Week 19 Roster Scraper

x               <- parLapply(clusters, Leagues_list, Week_19_Roster_Scraper)
Week_19_Rosters <- rbind(x[[1]], x[[2]], x[[3]], x[[4]])

write.csv(Week_19_Rosters, "Week_19_Rosters.csv", row.names = FALSE)


## Setting up data for Week 19 Roster Scraper

x              <- parLapply(clusters, Leagues_list, League_Winner_Scraper)
League_Winners <- rbind(x[[1]], x[[2]], x[[3]], x[[4]])

write.csv(League_Winners, "League_Winners.csv", row.names = FALSE)
