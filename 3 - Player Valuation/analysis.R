
library(dplyr)
library(tidyr)


##############################
######## LOADING DATA ########
##############################

weekly_totals    <- read.csv("data//Clean Data//Weekly_Totals.csv", stringsAsFactors = FALSE)
matchups         <- read.csv("data//Clean Data//Matchups.csv", stringsAsFactors = FALSE)
coefs_categories <- read.csv("2 - What does it take to win//output//coefficients.csv", stringsAsFactors = FALSE)


###############################
######## DATA CLEANING ########
###############################

## tidying weekly total data

weekly_totals %>%
  filter(Week_ID <= 21) %>%
  mutate(Match_Type = ifelse(Week_ID == 14, "All-Star", "Regular")) %>%
  gather(key   = "Category", 
         value = "Total", 
         c("R", "RBI", "HR", "SB", "AVG", "K", "W", "SV", "ERA", "WHIP")) -> weekly_totals 


## Getting average weekly number of ABs and IPs

matchups %>%
  filter(League_Index >= 0.25,
         League_Index <= 0.75,
         Week_ID <= 21) %>%
  mutate(Match_Type = ifelse(Week_ID == 14, "All-Star", "Regular")) %>%
  group_by(Match_Type) %>%
  summarise(avg_AB = mean(AB),
            avg_IP = mean(IP)) -> avg_AB_IP

rm(matchups)


## Joining coefs/avg_AB_IP onto weekly totals

weekly_totals %>%
  left_join(y  = coefs_categories,
            by = c("Category", "Match_Type")) %>%
  left_join(y  = avg_AB_IP,
            by =c("Match_Type"))-> weekly_totals

rm(coefs_categories, avg_AB_IP)



## Splitting weekly totals into two data.frames basd on roles

weekly_totals %>%
  filter(Role == "B",
         Category %in% c("R", "RBI", "HR", "SB", "AVG")) %>%
  select(-GP,
         -IP,
         -ER,
         -BR,
         -avg_IP) -> totals_batters

weekly_totals %>%
  filter(Role == "P",
         Category %in% c("K", "W", "SV", "ERA", "WHIP")) %>%
  select(-GB,
         -H,
         -AB,
         -avg_AB) -> totals_pitchers



#######################################
######## DATA ANALYSIS, PART I ########
#######################################

## getting raw change in win probability (i.e. Wins)

totals_batters %>%
  mutate(prob_50   = -coef_Int/coef_Value,
         new_total = case_when(Category == "AVG" ~ (prob_50*avg_AB + H)/(avg_AB + AB), 
                               TRUE ~ prob_50+Total),
         new_prob  = 1/(1+exp(-(coef_Value*new_total + coef_Int))),
         Wins      = new_prob - 0.5
         ) -> totals_batters

totals_pitchers %>%
  mutate(prob_50   = -coef_Int/coef_Value,
         new_total = case_when(Category == "ERA"  ~ (prob_50/9*avg_IP + ER)/(avg_IP + IP)*9, 
                               Category == "WHIP" ~ (prob_50*avg_IP + BR)/(avg_IP + IP),
                               TRUE ~ prob_50+Total),
         new_prob  = 1/(1+exp(-(coef_Value*new_total + coef_Int))),
         Wins      = new_prob - 0.5
         ) -> totals_pitchers



########################################
######## CLEANING DATA, PART II ########
########################################

## getting rid of unnessecary columns

totals_batters %>%
  select(-H,
         -avg_AB,
         -prob_50,
         -new_total,
         -new_prob,
         -coef_Int,
         -coef_Value) %>%
  rename(Games = GB,
         AB_IP = AB) -> totals_batters

totals_pitchers %>%
  select(-ER,
         -BR,
         -avg_IP,
         -prob_50,
         -new_total,
         -new_prob,
         -coef_Int,
         -coef_Value) %>%
  rename(Games = GP,
         AB_IP = IP) -> totals_pitchers


## combing pitcher and batter data

player_value <- rbind(totals_batters,
                      totals_pitchers)

rm(totals_batters, totals_pitchers)


## Fixing wins at zero for players who did not play in a given week (minors, DL)

player_value %>%
  mutate(Wins = ifelse(Games == 0, 0, Wins)) -> player_value



########################################
######## DATA ANALYSIS, PART II ########
########################################

## Getting average weekly wins per player

player_value %>%
  filter(Games > 0) %>%
  group_by(Player_ID,
           Player,
           Role,
           Week_ID) %>%
  summarise(Wins = sum(Wins)) %>%
  group_by(Player_ID,
           Player,
           Role) %>%
  summarise(avg_Wins = mean(Wins)) %>%
  ungroup() -> temp_replacements


## Getting list of replacement level players (top 25 P/B outside of the top 250 players)

temp_replacements %>%
  arrange(desc(avg_Wins)) %>%
  filter(row_number() > 250) %>%
  group_by(Role) %>%
  top_n(n  = 25,
        wt = avg_Wins) -> temp_replacements


## Getting average weekly Wins for each category of replacement level players

player_value %>%
  filter(Player_ID %in% temp_replacements$Player_ID,
         Games > 0) %>%
  group_by(Match_Type,
           Role,
           Category) %>%
  summarise(r_Wins = mean(Wins)) -> temp_replacements


## Substracting replacement level performance from raw Wins

player_value %>%
  left_join(y  = temp_replacements,
            by = c("Match_Type", "Role", "Category")) %>%
  mutate(WAR = ifelse(Games == 0, 0, Wins - r_Wins)) %>%
  select(-r_Wins) -> player_value




######################################
######## WRITING DATA TO FILE ########
######################################


write.csv(player_value, "3 - Player Valuation//output//player_value.csv", row.names = FALSE)





