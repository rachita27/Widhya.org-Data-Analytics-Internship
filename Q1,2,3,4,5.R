library(readxl)
dat= read_excel("matches_SHORT.xlsx",sheet = 2)
dat

library(ggplot2)
library(plotly)

str(dat)
     ##########
#### Question 1 ####
     ##########
l = ggplot(dat, aes(x= winner))+geom_bar( aes( fill = factor(winner)))
l
ggplotly(l)
### 20 matches played out of which 9 matches DD & DC, 11 matches won by RR.

dg = read_excel("matches_SHORT.xlsx",sheet = 1)
dg
u = ggplot(dg, aes(x= winner))+geom_bar( aes( fill = factor(winner)))
u
ggplotly(u)
## DC & DD IN TOTAL WON 77 MATCHES WHEREAS RR 75 MATCHES IN TOTAL

jj = ggplot(dg, aes(x= win_by_wickets))+geom_histogram( aes( fill = factor(winner)),color = "black",bins = 10)
jj
ggplotly(jj)
## RR- 46 matches won by for 2.5 and more wickets majorly for more than 5 wickets
## DC & DD WON 47 MATCHES

ll= ggplot(dat, aes(x= win_by_wickets))+geom_histogram( aes( fill = factor(winner)),color = "black",bins = 10)
ll
ggplotly(ll)
###rr- won 6 MATCHES by 2 or more wickets or more by DC & DD whereas RR won 5 matches

jk= ggplot(dg, aes(x= win_by_runs))+geom_histogram( aes( fill = factor(winner)),color = "black",bins = 10)+coord_cartesian(xlim= c(0,100))
jk
ggplotly(jk)
##dd & dc won 12 matches and rr won 14 matches by runs greater than 25.


ok= ggplot(dat, aes(x= win_by_runs))+geom_histogram( aes( fill = factor(winner)),color = "black",bins = 10)+coord_cartesian(xlim= c(0,100))
ok
ggplotly(ok)
##win_by_runs - 5 matches (rr) + 2(dc)

tt =ggplot(dat, aes(y= win_by_runs, x = winner))+geom_boxplot(aes( fill = factor(winner)))
tt
ggplotly(tt)
##MEDIAN OF WIN BY RUNS OF DD IS 1 AND RR 0 & max winning same for both
po =ggplot(dg, aes(y= win_by_runs, x = winner))+geom_boxplot(aes( fill = factor(winner)))
po
ggplotly(po)
##dc max win by 39 runs & rr max won 34 runs against rest all teams

tl =ggplot(dat, aes(y= win_by_wickets, x = winner))+geom_boxplot(aes( fill = factor(winner)))
tl
ggplotly(tl)
##win_by wickets average is rr-  6 , dd - meadin 0 (right skewed mean little above), DC- 6wickets (Average)

tp =ggplot(dg, aes(y= win_by_wickets, x = winner))+geom_boxplot(aes( fill = factor(winner)))
tp
ggplotly(tp)
##dd average 5(dd) , rr(4) - win by wickets

     ##########
#### QUESTION 2 ####
     ##########
table(dat$id)
innings = read.csv("deliveries_SHORT.csv")
head(innings) ##data set of batting team DD and bowling RR & filtered overs first six overs

team = c("Delhi Daredevils","Delhi Capitals","Rajasthan Royals")
library(dplyr)
ncol(innings)
inn = innings[,c(1,3,4,5,18)]
head(inn)
inn_new = inn %>% group_by(match_id)
head(inn_new)
inn_new%>% summarise(total_runs = sum(total_runs))
six_innings =inn_new%>% summarise(total_runs = sum(total_runs))
##shows the sum of total runs by DC in first six innings of each match

sum(six_innings)
mean(six_innings$total_runs) 
##47.65 runs on an average DC will score in first 6 over

     ##########
#### Question 3 ####
     ##########
kat = read.csv("deliveries_SH.csv")
head(kat)
nrow(kat)
team = c("Delhi Daredevils","Delhi Capitals","Rajasthan Royals")
batman =filter(kat, batting_team %in% team ,bowling_team %in% team, batsman == "SPD Smith")
table(batsman$bowling_team)
table(batsman$match_id)
head(batsman)
nrow(batsman)
grouped = group_by(batsman,match_id)
final = summarise(grouped,total_runs = sum(total_runs))
mean(final$total_runs) 
##31 runs in just 2 matches played against rajasthan royals so best to judge by his average rate in all matches
##before RAJASTHAN ROYALS he was in RISING PUNE SEARGIANT & PUNE WARRIORS

batsm =filter(kat, batsman == "SPD Smith")
table(batsm$batting_team)
nrow(batsm)
head(batsm)
grouped_2 = group_by(batsm,match_id)
total_run_smith = summarise(grouped_2, total_runs = sum(total_runs))
mean(total_run_smith$total_runs)
#29.625 average rate of smith
     
     ##########
#### Question 4 ####
     ##########
kat = read.csv("deliveries_SH.csv")
nrow(kat)
head(kat)
runs = c(1,2,3,5)
wid =filter(kat, batting_team %in% team, bowling_team %in% team,wide_runs %in% runs)
nrow(wid)
table(wid$wide_runs)

nrow(wid)
table(new$wide_runs)

gh = group_by(wid, match_id)
gh
kk= count(gh, wide_runs)
table(kk$wide_runs)

kt = group_by(kk, match_id) 
gt = summarise(kt, wide_ball_number = sum(n))
mean(gt$wide_ball_number)
##on an average 7.3 wide balls in match

     ##########
#### Question 5 ####
     ##########
 
hf = read.csv("last.csv")
gat =filter(hf, batting_team %in% team, bowling_team %in% team )
head(gat)
nrow(gat)


hg = group_by(gat, match_id)

ko = count(hg, player_dismissed)
ko

hg = group_by(ko, match_id)
hg
ok= summarise(hg, number_of_players_bowled = sum(n))
ok
mean(ok$number_of_players_bowled)
