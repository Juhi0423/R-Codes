library(flexdashboard)
library(dplyr)
library(data.table)
library(tidyr)
library(openxlsx)
library(shiny)
library(flexdashboard)
library(shinyWidgets)
setwd("E:/Data Visualization/Assignments/Assignment - 3/IPL Data 2008-16/")
ball_by_ball <- read.xlsx("ball_by_ball.xlsx")
batsman_scored <-read.xlsx("batsman_scored.xlsx")
batting_style <-read.xlsx("batting_style.xlsx")
bowling_style <-read.xlsx("bowling_style.xlsx")
city <-read.xlsx("city.xlsx")
country <-read.xlsx("country.xlsx")
extra_runs <-read.xlsx("extra_runs.xlsx")
extra_type <-read.xlsx("extra_type.xlsx")
match <-read.xlsx("match.xlsx")
outcome <-read.xlsx("outcome.xlsx")
out_type <-read.xlsx("out_type.xlsx")
player <-read.xlsx("player.xlsx")
player_match <-read.xlsx("player_match.xlsx")
rolee <-read.xlsx("rolee.xlsx")
season <-read.xlsx("season.xlsx")
team <-read.xlsx("team.xlsx")
toss_decision <-read.xlsx("toss_decision.xlsx")
umpire <-read.xlsx("umpire.xlsx")
venue <-read.xlsx("venue.xlsx")
wicket_taken <-read.xlsx("wicket_taken.xlsx")
win_by <-read.xlsx("win_by.xlsx")

#==================== Question - 1
# 1. Top 10 Batsmen by Runs
data1 <- inner_join(batsman_scored,ball_by_ball,by = c("match_id" = "match_id","over_id" = "over_id","ball_id" = "ball_id","innings_no" = "innings_no"))
data1 <- data1 %>% group_by(striker) %>% summarise("Runs" = sum(runs_scored)) %>% arrange(-Runs)
data2 <- inner_join(data1,player,by = c("striker" = "player_id"))
runs <- data2 %>% select(player_name,Runs) %>% head(10)
runs <- as.data.frame(runs)
save(runs,file = "runs.rda")

# 2. Top 10 Batsmen by Batting Average
data3 <- inner_join(player_match,player,by = c("player_id" = "player_id"))
data3 <- data3 %>% group_by(player_id) %>% summarise("Matches" = length(unique(match_id))) %>% arrange(-Matches)
data4 <- inner_join(batsman_scored,ball_by_ball,by = c("match_id" = "match_id","over_id" = "over_id","ball_id" = "ball_id","innings_no" = "innings_no"))
data4 <- data4 %>% group_by(striker) %>% summarise("Runs" = sum(runs_scored)) %>% arrange(-Runs)
data5 <- inner_join(data3,data4,by = c("player_id" = "striker"))
batting.avg <- data5 %>% mutate("Bat.Avg" = round(Runs/Matches,3)) %>% arrange(-Bat.Avg) %>% head(10)
batting.avg <- batting.avg %>% select(player_id,Bat.Avg)
batting.avg <- inner_join(batting.avg,player,by= c("player_id" = "player_id"))
batting.avg <- batting.avg %>% select(player_name,Bat.Avg)
batting.avg <- as.data.frame(batting.avg)
save(batting.avg,file = "bat_avg.rda")

# 3. Top 10 Batsmen by Strike Rate
data5 <- inner_join(batsman_scored,ball_by_ball,by = c("match_id" = "match_id","over_id" = "over_id","ball_id" = "ball_id","innings_no" = "innings_no"))
data5 <- data5 %>% group_by(striker) %>% summarise("Runs" = sum(runs_scored))
data6 <- ball_by_ball %>% group_by(striker) %>% summarise("balls" = n())
data7 <- inner_join(data5,data6,by = c("striker" = "striker"))
data7 <- data7 %>% mutate("Strike.Rate" = round(Runs/balls,2)) %>% arrange(-Strike.Rate)
data7 <- data7 %>% select(striker,Strike.Rate)
data8 <- inner_join(data7,player,by = c("striker" = "player_id"))
strike_rate <- data8 %>% select(player_name,Strike.Rate) %>% head(10)
strike_rate$Strike.Rate <- strike_rate$Strike.Rate * 100
strike_rate <- as.data.frame(strike_rate)
save(strike_rate,file = "strike_rate.rda")

# 4. Based on Highest Score
data9 <- inner_join(batsman_scored,ball_by_ball,by = c("match_id" = "match_id","over_id" = "over_id","ball_id" = "ball_id","innings_no" = "innings_no"))
data9 <- data9 %>% group_by(striker,match_id) %>% summarise("Runs" = sum(runs_scored)) %>% arrange(-Runs)
data9 <- as.data.frame(data9)
data10 <- inner_join(data9,player,by = c("striker" = "player_id"))
data10 <- data10 %>% select(player_name,Runs) %>% arrange(-Runs) %>% head(10)
high_score <- as.data.frame(data10)
save(high_score,file = "high_score.rda")


##============= Question - 2
data11 <- inner_join(player,season,by = c("player_id" = "purple_cap"))
purple_cap <- data11 %>% select(player_name,season_year) %>% arrange(season_year)
purple_cap <- as.data.frame(purple_cap)
save(purple_cap,file = "purple_cap.rda")

data12 <- inner_join(player,season,by = c("player_id" = "orange_cap"))
orange_cap <- data12 %>% select(player_name,season_year) %>% arrange(season_year)
orange_cap <- as.data.frame(orange_cap)
save(orange_cap,file = "orange.rda")

#============== Question - 3
#1 -  Total Matches
matches <- length(unique(match$match_id))

#2 - Wickets taken
names(wicket_taken)
names(out_type)
out_type
data13 <- inner_join(wicket_taken,out_type,by = c("kind_out" = "out_id"))
wickets <- data13 %>% filter(out_name %in% c("bowled","caught","run out","lbw","caught and bowled","stumped","hit wicket")) %>% summarise("Wickets" = n())

#3 - 6's
sixes <- batsman_scored %>% filter(runs_scored == 6) %>% summarise(count = n())

#4 - 4's
fours <- batsman_scored %>% filter(runs_scored == 4) %>% summarise(count = n())

res <- data.frame(matrix(ncol = 2,nrow = 4))
names(res) <- c("Attribute","Value")
res$Attribute <- c("Matches","Wickets","Sixes","Fours")
res$Value <- c(matches,wickets,sixes,fours)
res <- apply(res,2,as.character)
res <- as.data.frame(res)
save(res,file = "summary.rda")

