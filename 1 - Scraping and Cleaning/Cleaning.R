## Loading libraries

library(dplyr)
library(tidyr)
library(stringr)




##############################
######## LOADING DATA ########
##############################

## Loading ESPN data

Leagues         <- read.csv("data//Raw Data//ESPN Fantasy//Leagues.csv", stringsAsFactors = FALSE)
League_Index    <- read.csv("data//Raw Data//ESPN Fantasy//League_Index.csv", stringsAsFactors = FALSE)
Matchups        <- read.csv("data//Raw Data//ESPN Fantasy//Matchups.csv", stringsAsFactors = FALSE)
AB_IP           <- read.csv("data//Raw Data//ESPN Fantasy//AB_IP.csv", stringsAsFactors = FALSE)
Final_Rosters   <- read.csv("data//Raw Data//ESPN Fantasy//Final_Rosters.csv", stringsAsFactors = FALSE)
Week_19_Rosters <- read.csv("data//Raw Data//ESPN Fantasy//Week_19_Rosters.csv", stringsAsFactors = FALSE)
Draft_Results   <- read.csv("data//Raw Data//ESPN Fantasy//Draft_results.csv", stringsAsFactors = FALSE)
League_Winners  <- read.csv("data//Raw Data//ESPN Fantasy//League_Winners.csv", stringsAsFactors = FALSE)
Matchup_Dates   <- read.csv("data//Raw Data//ESPN Fantasy//Matchup_dates.csv", stringsAsFactors = FALSE)

## Loading Baseball Reference Data

Batter_Game_Logs  <- read.csv("data//Raw Data//Baseball Reference//Batter_Game_Logs.csv", stringsAsFactors = FALSE)
Pitcher_Game_Logs <- read.csv("data//Raw Data//Baseball Reference//Pitcher_Game_Logs.csv", stringsAsFactors = FALSE)

## Loading player id file (crosswalk between espn and BR data)

Player_ID       <- read.csv("data//Raw Data//Player_IDs.csv", stringsAsFactors = FALSE)




########################################################
######## DATA CLEANING - LEAGUES & LEAGUE INDEX ########
########################################################

## Appending League Index to Leagues dataframe

Leagues %>%
  left_join(y  = League_Index,
            by = "League_ID") -> Leagues

rm(League_Index)



#################################################
######## DATA CLEANING - WEEK 19 ROSTERS ########
#################################################

## Cleaning up Team name, Player Name, and Role

Week_19_Rosters %>%
  mutate(Team = str_extract(Team_Name, '.*(?=\\sBox Score)'),
         Team = trimws(tolower(Team)),
         Team = str_sub(Team, 1, 20),
         
         Player = str_extract(Player, '^[\\w\\s.\'-]*,\\s\\w*'),
         Role   = ifelse(Role == "Batter", "B", "P")) -> Week_19_Rosters


## Appending Player IDs

Week_19_Rosters %>%
  left_join(y  = unique(Player_ID[, c("Player_ID", "ESPN_Name")]),
            by = c("Player" = "ESPN_Name")) %>%
  filter(is.na(Player_ID) == FALSE) -> Week_19_Rosters


## Removing rows with no player name

Week_19_Rosters %>%
  filter(is.na(Player) == FALSE,
         Player != "PLAYER, TEAM") -> Week_19_Rosters


## Getting rid of Leagues where there are multiple teams with the same name

Week_19_Rosters %>%
  group_by(League_ID) %>%
  summarise(CT = n_distinct(Team)) %>%
  filter(CT == 10) -> temp_leagues

Week_19_Rosters <- Week_19_Rosters[Week_19_Rosters$League_ID %in% temp_leagues$League_ID, ]

rm(temp_leagues)


## Rearranging and renaming columns

Week_19_Rosters %>%
  select(League_ID,
         Team_ID,
         Team,
         Role,
         Player_ID,
         Player) -> Week_19_Rosters


## Appending League Index to Week 19 Rosters

Week_19_Rosters %>%
  left_join(x  = Week_19_Rosters,
            y  = Leagues[, c("League_ID", "League_Index")],
            by = "League_ID") -> Week_19_Rosters



##########################################
######## DATA CLEANING - TEAM IDS ########
##########################################

Team_ID <- unique(Week_19_Rosters[, c("League_ID", "Team", "Team_ID"), ])




###############################################
######## DATA CLEANING - DRAFT RESULTS ########
###############################################

## Converting Team to character, making lowercase, $ trimming white space

Draft_Results %>%
  mutate(Team = tolower(trimws(Team)),
         Team = str_sub(Team, 1, 20)) -> Draft_Results



## Appending Team_ID to dataframe

Draft_Results %>%
  left_join(y  = Team_ID,
            by = c("League" = "League_ID",
                   "Team"   = "Team")) %>%
  filter(is.na(Team_ID) == FALSE) -> Draft_Results


## Cleaning up player name

Draft_Results %>%
  mutate(Player = str_extract(Player, '^[\\w\\s.\'-]*,\\s\\w*')) -> Draft_Results


## Appending player ID

Draft_Results %>%
  left_join(y  = unique(Player_ID[, c("ESPN_Name", "Player_ID", "Role")]),
            by = c("Player" = "ESPN_Name")) %>%
  filter(is.na(Player_ID) == FALSE) -> Draft_Results


## Converting Pick to Round

Draft_Results %>%
  mutate(Round = floor((Draft_Pick-1)/10)+1) -> Draft_Results


## Getting rid of Leagues where there are multiple teams with the same name

Draft_Results %>%
  group_by(League) %>%
  summarise(CT = n_distinct(Team)) %>%
  filter(CT == 10) -> temp_leagues

Draft_Results <- Draft_Results[Draft_Results$League %in% temp_leagues$League, ]

rm(temp_leagues)


## Getting rid of Leagues were each team didn't draft 25 players

Draft_Results %>%
  group_by(League, 
           Team) %>%
  summarise(PLAYER_CT = n()) %>%
  filter(PLAYER_CT == 25) %>%
  group_by(League) %>%
  summarise(TEAM_CT = n()) %>%
  filter(TEAM_CT == 10) -> temp_leagues

Draft_Results <- Draft_Results[Draft_Results$League %in% temp_leagues$League, ]

rm(temp_leagues)


## Rearranging & renaming columns

Draft_Results %>%
  transmute(League_ID = League,
            Team_ID   = Team_ID,
            Team      = Team,
            Player_ID = Player_ID,
            Player    = Player,
            Role      = Role,
            Pick      = Draft_Pick,
            Round     = Round) -> Draft_Results


## Appending League Index to Draft Results dataframe

Draft_Results %>%
  left_join(y = Leagues[, c("League_ID", "League_Index")],
            by = "League_ID") -> Draft_Results



###############################################
######## DATA CLEANING - FINAL ROSTERS ########
###############################################

## Removing empty rows

Final_Rosters %>%
  filter(!Player == "") -> Final_Rosters


## Cleaning up team names

Final_Rosters %>%
  mutate(Team = str_extract(Team, '.*(?=\\s[(]\\d*[-])'),
         Team = trimws(tolower(Team)),
         Team = str_sub(Team, 1, 20)) -> Final_Rosters


## Appending Team_ID to dataframe

Final_Rosters %>%
  left_join(y  = Team_ID,
            by = c("League_ID" = "League_ID", 
                   "Team"      = "Team")) %>%
  filter(is.na(Team_ID) == FALSE) -> Final_Rosters


## Cleaning up player names

Final_Rosters %>%
  mutate(Player = str_extract(Player, '^[\\w\\s.\'-]*,\\s\\w*')) -> Final_Rosters


## Appending Player IDs

Final_Rosters %>%
  left_join(y  = unique(Player_ID[, c("Player_ID", "ESPN_Name", "Role")]),
            by = c("Player" = "ESPN_Name")) %>%
  filter(is.na(Player_ID) == FALSE) -> Final_Rosters


## Getting rid of Leagues where there are multiple teams with the same name

unique(Final_Rosters[, c("Team", "League_ID")]) %>%
  group_by(League_ID) %>%
  tally() %>%
  filter(n == 10) -> temp_leagues

Final_Rosters <- Final_Rosters[Final_Rosters$League_ID %in% temp_leagues$League_ID, ]

rm(temp_leagues)


## Rearranging and renaming columns

Final_Rosters %>%
  transmute(League_ID = League_ID,
            Team_ID   = Team_ID,
            Team      = Team,
            Acquired  = Acquired,
            Slot      = Slot,
            Player_ID = Player_ID,
            Player    = Player,
            Role      = Role) -> Final_Rosters


## Appending League Index to League

Final_Rosters %>%
  left_join(y  = Leagues[, c("League_ID", "League_Index")],
            by = "League_ID") -> Final_Rosters




#######################################
######## DATA CLEANING - AB_IP ########
#######################################

## Cleaning up AB_IP Team Name

AB_IP %>%
  mutate(Team = str_extract(Team, '.*(?=\\sBox Score)'),
         Team = trimws(tolower(Team)),
         Team = str_sub(Team, 1, 20)) -> AB_IP


## Spreading AB & IP over two columns

AB_IP %>%
  spread(Category, Total) -> AB_IP


## Cleaning up AB & IP Totals

AB_IP %>%
  mutate(AB = str_extract(AB, '(?<=/).*')) %>%
  filter(str_detect(IP, "/") == FALSE,
         is.na(Week_ID) == FALSE) %>%
  mutate(IP = as.numeric(as.character(IP))) -> AB_IP


## Cleaning up AB_IP Week_ID

temp_weeks <- data.frame(Week_ID = c(8, 15, 22, 29, 36, 43, 50, 57, 69, 71, 78, 85, 92, 106, 113, 120, 127, 134, 141, 148, 155),
                         Week    = c(1:21))

AB_IP %>%
  left_join(y = temp_weeks,
            by = "Week_ID") -> AB_IP

AB_IP$Week_ID <- AB_IP$Week

AB_IP %>%
  select(-Week) -> AB_IP

rm(temp_weeks)


## Getting rid of Leagues where there are multiple teams with the same name

unique(AB_IP[, c("Team", "League_ID")]) %>%
  group_by(League_ID) %>%
  tally() %>%
  filter(n == 10) -> temp_leagues

AB_IP <- AB_IP[AB_IP$League_ID %in% temp_leagues$League_ID, ]

rm(temp_leagues)




##########################################
######## DATA CLEANING - MATCHUPS ########
##########################################

## Cleaning up team and opponent name

Matchups %>%
  mutate(NAME = str_extract(NAME, '.*(?=\\s[(]\\d*[-])'),
         NAME = trimws(tolower(NAME)),
         NAME = str_sub(NAME, 1, 20),
         
         Oppenent = str_extract(Oppenent, '.*(?=\\s[(]\\d*[-])'),
         Oppenent = trimws(tolower(Oppenent)),
         Oppenent = str_sub(Oppenent, 1, 20)
         ) -> Matchups


## Appending Team_ID to dataframe

Matchups %>%
  left_join(y = Team_ID,
            by = c("League_ID" = "League_ID", 
                   "NAME"      = "Team")) %>%
  left_join(y = Team_ID,
            by = c("League_ID" = "League_ID", 
                   "Oppenent"  = "Team")) -> Matchups

colnames(Matchups)[15:16] <- c("Team_ID", "Opponent_ID")

Matchups %>%
  filter(is.na(Team_ID) == FALSE,
         is.na(Opponent_ID) == FALSE) -> Matchups


## Appending AB_IP data to Matchup data

Matchups %>%
  left_join(y = AB_IP,
            by = c("League_ID" = "League_ID",
                   "Team_ID"   = "Team_ID",
                   "Week"      = "Week_ID")) -> Matchups

rm(AB_IP)


## Removing rows where AB/IP data is missing

Matchups %>%
  filter(is.na(AB) == FALSE) -> Matchups


## Removing teams/leagues where we don't have the full season

Matchups %>%
  group_by(League_ID, 
           NAME) %>%
  summarise(WEEK_CT = n()) %>%
  filter(WEEK_CT == 21) %>%
  group_by(League_ID) %>%
  summarise(TEAM_CT = n()) %>%
  filter(TEAM_CT == 10) -> temp_leagues 

Matchups <- Matchups[Matchups$League_ID %in% temp_leagues$League_ID,]


## Rearranging and renaming columns

Matchups %>%
  transmute(League_ID   = League_ID,
            Week_ID     = Week,
            Team_ID     = Team_ID,
            Opponent_ID = Opponent_ID,
            Team        = NAME,
            Opponent    = Oppenent,
            R           = R,
            HR          = HR,
            RBI         = RBI,
            SB          = SB,
            AVG         = AVG,
            AB          = as.numeric(AB),
            K           = K,
            W           = W,
            SV          = SV,
            ERA         = ERA,
            WHIP        = WHIP,
            IP          = IP) -> Matchups


## Appending League Index to Matchups

Matchups %>%
  left_join(y  = Leagues[, c("League_ID", "League_Index")],
            by = "League_ID") -> Matchups

rm(Leagues, temp_leagues)




####################################################################
######## DATA CLEANING - REMOVING LEAGUES WITH MISSING DATA ########
####################################################################

## Getting complete leagues for each data frame

unique(Matchups[, c("League_ID", "Team")]) %>%
  group_by(League_ID) %>%
  tally() %>%
  filter(n == 10) -> temp_Matchups

unique(Draft_Results[, c("League_ID", "Team")]) %>%
  group_by(League_ID) %>%
  tally() %>%
  filter(n == 10) -> temp_Draft_Results

unique(Week_19_Rosters[, c("League_ID", "Team")]) %>%
  group_by(League_ID) %>%
  tally() %>%
  filter(n == 10) -> temp_Week_19_Rosters

unique(Final_Rosters[, c("League_ID", "Team")]) %>%
  group_by(League_ID) %>%
  tally() %>%
  filter(n == 10) -> temp_Final_Rosters

temp_Matchups$df        <- "Matchups"
temp_Draft_Results$df   <- "Draft_Results"
temp_Week_19_Rosters$df <- "Week_19_Rosters"
temp_Final_Rosters$df   <- "Final_Rosters"

temp_master <- rbind(temp_Matchups,
                     temp_Draft_Results,
                     temp_Week_19_Rosters,
                     temp_Final_Rosters)

rm(temp_Matchups, temp_Draft_Results, temp_Week_19_Rosters, temp_Final_Rosters)


## spreading the temp data set and finding leagues with 10 teams in each dataframe

temp_master <- spread(temp_master, df, n)
temp_master <- na.omit(temp_master)


## Removing incomplete leagues from final data sets

Draft_Results   <- Draft_Results[Draft_Results$League_ID %in% temp_master$League_ID, ]
Final_Rosters   <- Final_Rosters[Final_Rosters$League_ID %in% temp_master$League_ID, ]
Matchups        <- Matchups[Matchups$League_ID %in% temp_master$League_ID, ]
Week_19_Rosters <- Week_19_Rosters[Week_19_Rosters$League_ID %in% temp_master$League_ID, ]

rm(temp_master)




###########################################
######## DATA CLEANING - STANDINGS ########
###########################################

## Unwrapping Matchups

Matchups[, c(1:6, 19, 7:11, 13:17)] %>% 
  gather(key   = "Category",
         value = "Total",
         c("R", "RBI", "HR", "SB", "AVG", "ERA", "WHIP", "K", "SV", "W")
         ) -> Standings

Standings %>%
  left_join(y  = Standings[, c("League_ID", "Week_ID", "Team_ID", "Category", "Total")],
            by = c("League_ID"   = "League_ID",
                   "Week_ID"     = "Week_ID",
                   "Opponent_ID" = "Team_ID",
                   "Category"    = "Category")) -> Standings


## Figuring out if they won or lost the category matchup

Standings$Win  <- ifelse(Standings$Category %in% c("WHIP", "ERA") & Standings$Total.x < Standings$Total.y, 1, 
                  ifelse(!Standings$Category %in% c("WHIP", "ERA") & Standings$Total.x > Standings$Total.y, 1, 0))
                         
Standings$Loss <- ifelse(Standings$Category %in% c("WHIP", "ERA") & Standings$Total.x > Standings$Total.y, 1, 
                  ifelse(!Standings$Category %in% c("WHIP", "ERA") & Standings$Total.x < Standings$Total.y, 1, 0))


## Aggregating data - Getting weekly count of wins and losses

Standings %>%
  group_by(League_ID, Week_ID, Team_ID, Team, League_Index) %>%
  summarise(Wins   = sum(Win),
            Losses = sum(Loss)) %>%
  ungroup() -> Standings


## Getting the running total of wins and losses

Standings %>%
  select(League_ID,
         Week_ID,
         Team_ID) %>%
  left_join(y  = Standings,
            by = c("League_ID", "Team_ID")) %>%
  filter(Week_ID.y <= Week_ID.x) -> Standings


## Aggregating data - getting season record at end of each week

Standings %>%
  group_by(League_ID, 
           Week_ID.x, 
           Team_ID, 
           Team, 
           League_Index) %>%
  summarise(Wins   = sum(Wins),
            Losses = sum(Losses),
            PCT    = sum(Wins)/(sum(Losses) + sum(Wins))
            ) -> Standings

colnames(Standings)[2] <- "Week_ID"


## Getting Weekly Rank

Standings %>%
  group_by(League_ID, 
           Week_ID) %>%
  mutate(Rank = order(order(PCT, decreasing = TRUE))
            ) -> Standings

## Fixing ties (setting rank as lowest rank of the ties)

Standings %>%
  group_by(League_ID, 
           Week_ID, 
           PCT) %>%
  summarise(Count    = sum(PCT == PCT),
            Min_Rank = min(Rank)) %>%
  filter(Count > 1) -> temp_ties

Standings %>%
  left_join(y = temp_ties[, c("League_ID", "Week_ID", "PCT", "Min_Rank")],
            by = c("League_ID", "Week_ID", "PCT")) -> Standings

rm(temp_ties)

Standings %>%
  mutate(Rank = ifelse(is.na(Min_Rank) == TRUE, Rank, Min_Rank)) -> Standings

Standings <- Standings[, 1:9]


## Appendings league Winners

League_Winners %>%
  mutate(Winner = trimws(tolower(Winner))) -> League_Winners
  
Standings %>%
  left_join(y  = League_Winners,
            by = c("League_ID" = "League")) %>%
  mutate(Winner = ifelse(as.character(Winner) == as.character(Team), 1, 0)) -> Standings

rm(League_Winners, Team_ID)




###########################################
######## DATA CLEANING - GAME LOGS ########
###########################################

## Unique data

Batter_Game_Logs  <- unique(Batter_Game_Logs)
Pitcher_Game_Logs <- unique(Pitcher_Game_Logs)


## Cleaning up dates

Batter_Game_Logs$Date  <- as.Date(as.character(Batter_Game_Logs$Date), "%m/%d/%Y")
Pitcher_Game_Logs$Date <- as.Date(as.character(Pitcher_Game_Logs$Date), "%m/%d/%Y")

Matchup_Dates$Start <- as.Date(as.character(Matchup_Dates$Start), "%m/%d/%Y")
Matchup_Dates$End   <- as.Date(as.character(Matchup_Dates$End), "%m/%d/%Y")


## Appending Player_ID to game log data & Removing players that didn't appear in the ESPN dataset

Batter_Game_Logs %>%
  left_join(y  = unique(Player_ID[, c("BR_Name", "Player_ID", "Role")]),
            by = c("Player" = "BR_Name")) %>%
  filter(is.na(Player_ID) == FALSE) -> Batter_Game_Logs

Pitcher_Game_Logs %>%
  left_join(y = unique(Player_ID[, c("BR_Name", "Player_ID", "Role")]),
            by = c("Player" = "BR_Name")) %>%
  filter(is.na(Player_ID) == FALSE) -> Pitcher_Game_Logs

rm(Player_ID)


## Appending Match up dates to game logs

Batter_Game_Logs <- merge(x = Batter_Game_Logs,
                          y = Matchup_Dates,
                          all = TRUE)

Pitcher_Game_Logs <- merge(x = Pitcher_Game_Logs,
                           y = Matchup_Dates,
                           all = TRUE)

Batter_Game_Logs  <- Batter_Game_Logs[Batter_Game_Logs$Date >= Batter_Game_Logs$Start &
                                      Batter_Game_Logs$Date <= Batter_Game_Logs$End, ]

Pitcher_Game_Logs <- Pitcher_Game_Logs[Pitcher_Game_Logs$Date >= Pitcher_Game_Logs$Start &
                                       Pitcher_Game_Logs$Date <= Pitcher_Game_Logs$End, ]

rm(Matchup_Dates)


## Getting weekly totals

Batter_Game_Logs %>%
  group_by(Player_ID, 
           Player, 
           Role, 
           Week) %>%
  summarise(GB  = sum(Player_ID == Player_ID),
            H   = sum(H), 
            AB  = sum(AB),
            AVG = sum(H)/sum(AB),
            R   = sum(R),
            RBI = sum(RBI),
            HR  = sum(HR),
            SB  = sum(SB)
            ) -> Batter_Weekly_Totals

Pitcher_Game_Logs %>%
  group_by(Player_ID, 
           Player, 
           Role,
           Week) %>%
  summarise(GP   = sum(Player_ID == Player_ID),
            IP   = sum(IP),
            W    = sum(Dec == "W"),
            SV   = sum(Dec == "S"),
            K    = sum(SO),
            ER   = sum(ER),
            BR   = sum(H) + sum(BB) + sum(HBP),
            ERA  = sum(ER)/sum(IP)*9,
            WHIP = (sum(H) + sum(BB) + sum(HBP))/sum(IP)
            ) -> Pitcher_Weekly_Totals

rm(Pitcher_Game_Logs)
rm(Batter_Game_Logs)


## Setting up a data frame for all weekly stats

Weekly_Totals <- data.frame(Week_ID = (1:23))

temp_players <- unique(rbind(Batter_Weekly_Totals[, c("Player_ID", "Player", "Role")],
                             Pitcher_Weekly_Totals[, c("Player_ID", "Player", "Role")]))

Weekly_Totals <- merge(x = Weekly_Totals,
                       y = temp_players,
                       all = TRUE)

rm(temp_players)


## Combing Batter totals and Pitcher totals into one dataframe

Weekly_Totals %>%
  left_join(y = Batter_Weekly_Totals[Batter_Weekly_Totals$Role == "B", ],
            by = c("Week_ID" = "Week", 
                   "Player" = "Player", 
                   "Player_ID" = "Player_ID",
                   "Role" = "Role")) -> Weekly_Totals

Weekly_Totals %>%
  left_join(y = Pitcher_Weekly_Totals[Pitcher_Weekly_Totals$Role == "P", ],
            by = c("Week_ID" = "Week", 
                   "Player" = "Player", 
                   "Player_ID" = "Player_ID",
                   "Role" = "Role")) -> Weekly_Totals

rm(Batter_Weekly_Totals, Pitcher_Weekly_Totals)


## Replacing null values with zeros

Weekly_Totals[is.na(Weekly_Totals)] <- 0




######################################
######## WRITING DATA TO FILE ########
######################################

## Writing Data to file

write.csv(Draft_Results, "data//Clean Data//Draft_Results.csv", row.names = FALSE)
write.csv(Final_Rosters, "data//Clean Data//Final_Rosters.csv", row.names = FALSE)
write.csv(Matchups, "data//Clean Data//Matchups.csv", row.names = FALSE)
write.csv(Week_19_Rosters, "data//Clean Data//Week_19_Rosters.csv", row.names = FALSE)
write.csv(Standings, "data//Clean Data//Standings.csv", row.names = FALSE)
write.csv(Weekly_Totals, "data//Clean Data//Weekly_Totals.csv", row.names = FALSE)




