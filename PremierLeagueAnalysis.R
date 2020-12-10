library('httr')
library('tidyverse')
library('XML')
library('data.table')
library('matrixStats')


## first I'll be using the data found on this website

url <- 'https://fbref.com/en/comps/9/3232/schedule/2019-2020-Premier-League-Scores-and-Fixtures'

PremierLeague2019.2020 <- GET(url)
PremierLeague2019.2020_parsed <- htmlParse(PremierLeague2019.2020) 

PremierLeague.tables <- readHTMLTable(PremierLeague2019.2020_parsed, stringsAsFactors = FALSE)

LeagueTable <- PremierLeague.tables[[1]]


## I will be simplifying our table and removing the extra information

League <- LeagueTable[,c(1,5,7,9)]


## we are plotting into three separate columns here, the home goals, away goals and the -, 
## we will be removing the dash column next
## reference: https://stats.stackexchange.com/questions/6806/splitting-a-numeric-column-for-a-dataframe

League <- separate(League,Score, into = c('HGoals','AGoals', sep = '-'),  extra ='merge')
League$`-`= NULL

## remove all NAs and empty rows
## reference: https://stackoverflow.com/questions/6437164/removing-empty-rows-of-a-data-file-in-r

League <- League[!apply(is.na(League) | League == "",1,all), ]

## Create two columns for points earned by teams playing at Home and Away 

League$HWin <- ifelse(League$HGoals > League$AGoals, 3, 
                      ifelse(League$HGoals == League$AGoals,1,0))
League$AWin <- ifelse(League$AGoals > League$HGoals, 3, 
                      ifelse(League$HGoals == League$AGoals,1,0))
League
## creating temporary dataframes that will be combined into one, hence I didnt worry about the naming
df1 <- League[,c(1,2,6)]
df2 <- League[,c(1,5,7)]   

TeamPoints <- data.frame(Weeks = c(df1$Wk,df2$Wk),Teams = c(df1$Home, df2$Away), Points = c(df1$HWin,df2$AWin))

rt1 <- TeamPoints %>% filter(Weeks == 1) %>% 
  rename(Wk1 = Points) %>% 
  arrange(Teams)
  rt1$Weeks =NULL
rt2 <- TeamPoints %>% filter(Weeks == 2) %>% 
  rename(Wk2 = Points) %>% 
  arrange(Teams)
  rt2$Weeks =NULL
rt3 <- TeamPoints %>% filter(Weeks == 3) %>% 
  rename(Wk3 = Points) %>% 
  arrange(Teams)
  rt3$Weeks =NULL
rt4 <- TeamPoints %>% filter(Weeks == 4) %>% 
  rename(Wk4 = Points) %>% 
  arrange(Teams)
  rt4$Weeks =NULL
rt5 <- TeamPoints %>% filter(Weeks == 5) %>% 
  rename(Wk5 = Points) %>% 
  arrange(Teams)
  rt5$Weeks =NULL
rt6 <- TeamPoints %>% filter(Weeks == 6) %>% 
  rename(Wk6 = Points) %>% 
  arrange(Teams)
  rt6$Weeks =NULL
rt7 <- TeamPoints %>% filter(Weeks == 7) %>% 
  rename(Wk7 = Points) %>% 
  arrange(Teams)
  rt7$Weeks =NULL
rt8 <- TeamPoints %>% filter(Weeks == 8) %>% 
  rename(Wk8 = Points) %>% 
  arrange(Teams)
  rt8$Weeks =NULL
rt9 <- TeamPoints %>% filter(Weeks == 9) %>% 
  rename(Wk9 = Points) %>% 
  arrange(Teams)
  rt9$Weeks =NULL
rt10 <- TeamPoints %>% filter(Weeks == 10) %>% 
  rename(Wk10 = Points) %>% 
  arrange(Teams)
  rt10$Weeks =NULL
rt11 <- TeamPoints %>% filter(Weeks == 11) %>% 
  rename(Wk11 = Points) %>% 
  arrange(Teams)
  rt11$Weeks =NULL
rt12 <- TeamPoints %>% filter(Weeks == 12) %>% 
  rename(Wk12 = Points) %>% 
  arrange(Teams)
  rt12$Weeks =NULL
rt13 <- TeamPoints %>% filter(Weeks == 13) %>% 
  rename(Wk13 = Points) %>% 
  arrange(Teams)
  rt13$Weeks =NULL
rt14 <- TeamPoints %>% filter(Weeks == 14) %>% 
  rename(Wk14 = Points) %>% 
  arrange(Teams)
  rt14$Weeks =NULL
rt15 <- TeamPoints %>% filter(Weeks == 15) %>% 
  rename(Wk15 = Points) %>% 
  arrange(Teams)
  rt15$Weeks =NULL
rt16 <- TeamPoints %>% filter(Weeks == 16) %>% 
  rename(Wk16 = Points) %>% 
  arrange(Teams)
  rt16$Weeks =NULL
rt17 <- TeamPoints %>% filter(Weeks == 17) %>% 
  rename(Wk17 = Points) %>% 
  arrange(Teams)
  rt17$Weeks =NULL
rt18 <- TeamPoints %>% filter(Weeks == 18) %>% 
  rename(Wk18 = Points) %>% 
  arrange(Teams)
  rt18$Weeks =NULL
rt19 <- TeamPoints %>% filter(Weeks == 19) %>% 
  rename(Wk19 = Points) %>% 
  arrange(Teams)
  rt19$Weeks =NULL
rt20 <- TeamPoints %>%  filter(Weeks ==20) %>% 
  rename(Wk20 = Points) %>% 
  arrange(Teams)
  rt20$Weeks = NULL
rt21 <- TeamPoints %>% filter(Weeks == 21) %>% 
  rename(Wk21 = Points) %>% 
  arrange(Teams)
  rt21$Weeks =NULL
rt22 <- TeamPoints %>% filter(Weeks == 22) %>% 
  rename(Wk22 = Points) %>% 
  arrange(Teams)
  rt22$Weeks =NULL
rt23 <- TeamPoints %>% filter(Weeks == 23) %>% 
  rename(Wk23 = Points) %>% 
  arrange(Teams)
  rt23$Weeks =NULL
rt24 <- TeamPoints %>% filter(Weeks == 24) %>% 
  rename(Wk24 = Points) %>% 
  arrange(Teams)
  rt24$Weeks =NULL
rt25 <- TeamPoints %>% filter(Weeks == 25) %>% 
  rename(Wk25 = Points) %>% 
  arrange(Teams)
  rt25$Weeks =NULL
rt26 <- TeamPoints %>% filter(Weeks == 26) %>% 
  rename(Wk26 = Points) %>% 
  arrange(Teams)
  rt26$Weeks =NULL
rt27 <- TeamPoints %>% filter(Weeks == 27) %>% 
  rename(Wk27 = Points) %>% 
  arrange(Teams)
  rt27$Weeks =NULL
rt28 <- TeamPoints %>% filter(Weeks == 28) %>% 
  rename(Wk28 = Points) %>% 
  arrange(Teams)
  rt28$Weeks =NULL
rt29 <- TeamPoints %>% filter(Weeks == 29) %>% 
  rename(Wk29 = Points) %>% 
  arrange(Teams)
  rt29$Weeks =NULL
rt30 <- TeamPoints %>% filter(Weeks == 30) %>% 
  rename(Wk30 = Points) %>% 
  arrange(Teams)
  rt30$Weeks =NULL
rt31 <- TeamPoints %>% filter(Weeks == 31) %>% 
  rename(Wk31 = Points) %>% 
  arrange(Teams)
  rt31$Weeks =NULL
rt32 <- TeamPoints %>% filter(Weeks == 32) %>% 
  rename(Wk32 = Points) %>% 
  arrange(Teams)
  rt32$Weeks =NULL
rt33 <- TeamPoints %>% filter(Weeks == 33) %>% 
  rename(Wk33 = Points) %>% 
  arrange(Teams)
  rt33$Weeks =NULL
rt34 <- TeamPoints %>% filter(Weeks == 34) %>% 
  rename(Wk34 = Points) %>% 
  arrange(Teams)
  rt34$Weeks =NULL
rt35 <- TeamPoints %>% filter(Weeks == 35) %>% 
  rename(Wk35 = Points) %>% 
  arrange(Teams)
  rt35$Weeks =NULL
rt36 <- TeamPoints %>% filter(Weeks == 36) %>% 
  rename(Wk36 = Points) %>% 
  arrange(Teams)
  rt36$Weeks =NULL
rt37 <- TeamPoints %>% filter(Weeks == 37) %>% 
  rename(Wk37 = Points) %>% 
  arrange(Teams)
  rt37$Weeks =NULL
rt38 <- TeamPoints %>% filter(Weeks == 38) %>% 
  rename(Wk38 = Points) %>% 
  arrange(Teams)
  rt38$Weeks =NULL


WeeklyStandings <- merge(rt1,rt2)

c <- list(rt3,rt4,rt5,rt6,rt7,rt8,rt9,rt10,rt11,rt12,rt13,rt14,rt15,rt16,rt17,rt18,rt19,rt20,
            rt21,rt22,rt23,rt24,rt25,rt26,rt27,rt28,rt29,rt30,rt31,rt32,rt33,rt34,rt35,rt36,rt37,rt38)

for ( i in 1:length(c)){
  WeeklyStandings <- merge(WeeklyStandings,c[i]) 
}

head(WeeklyStandings)

## now we add up weekly totals so we have a running count


## Since my first column are character strings, I cant use the rowsum function right away.
## I'll create a function that will generate the row sums for me. requires the user to input the
## dataframe and the column indexes that are numeric


temp <- WeeklyStandings[,2:39]
temp <- as.matrix(temp)
temp <- rowCumsums(temp)
WeeklyStandings[,2:39] <- as.data.frame(temp)

## we are done  but at this point we have 

WeeklyStandings_Transpose <- as.data.frame(t(as.matrix(WeeklyStandings)))

colnames(WeeklyStandings_Transpose) <- WeeklyStandings[,1]
WeeklyStandings_Transpose <- WeeklyStandings_Transpose[-c(1),]
WeeklyStandings_Transpose$Weeks<- rownames(WeeklyStandings_Transpose)

view(WeeklyStandings)
view(WeeklyStandings_Transpose)
WeeklyStandings_Transpose


