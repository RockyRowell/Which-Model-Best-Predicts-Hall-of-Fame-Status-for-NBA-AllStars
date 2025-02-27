## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## ----new player data--------------------------------------------------------------------------------------------------------------------------------------------------------------
player<-read.csv("Data/player_50to17/Seasons_Stats.csv")


## ----check player data------------------------------------------------------------------------------------------------------------------------------------------------------------
head(player, 5)
tail(player, 5)


## ----make career totals-----------------------------------------------------------------------------------------------------------------------------------------------------------
career<- player %>%
  group_by(Player) %>%
  summarise(G = sum(G), GS=sum(GS), MP=sum(MP), OWS=sum(OWS), DWS=sum(DWS), WS=sum(WS), 
            FG=sum(FG), FGA=sum(FGA), Three=sum(X3P), Three_Attempt=sum(X3PA), FT=sum(FT), 
            FTA=sum(FTA), ORB=sum(ORB), DRB=sum(DRB), TRB=sum(TRB), AST=sum(AST), 
            STL=sum(STL), BLK=sum(BLK), TOV=sum(TOV), PF=sum(PF), PTS=sum(PTS))

head(career, 5)


## ----remove star------------------------------------------------------------------------------------------------------------------------------------------------------------------
career$Player<-gsub("\\*", "", career$Player)


## ----check career data------------------------------------------------------------------------------------------------------------------------------------------------------------
test_career <- subset(career, Player %in% c("Steve Nash", "Jason Kidd", "P.J. Brown", "Magic Johnson"))
test_career


## ----remove tot rows--------------------------------------------------------------------------------------------------------------------------------------------------------------
player <- player %>% filter(Tm != "TOT")

career<- player %>%
  group_by(Player) %>%
  summarise(G = sum(G), GS=sum(GS), MP=sum(MP), OWS=sum(OWS), DWS=sum(DWS), WS=sum(WS), 
            FG=sum(FG), FGA=sum(FGA), Three=sum(X3P), Three_Attempt=sum(X3PA), FT=sum(FT), 
            FTA=sum(FTA), ORB=sum(ORB), DRB=sum(DRB), TRB=sum(TRB), AST=sum(AST), 
            STL=sum(STL), BLK=sum(BLK), TOV=sum(TOV), PF=sum(PF), PTS=sum(PTS))

career$Player<-gsub("\\*", "", career$Player)


## ----check kidd-------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_career


## ----hof data---------------------------------------------------------------------------------------------------------------------------------------------------------------------
hof<-read.csv("/Users/rockyrowell/Desktop/UofSC/Statistics/STAT 517 - Advanced Statistical Models/Project/Data/NBA Hall of Famers 2021.csv")


## ----check hof data---------------------------------------------------------------------------------------------------------------------------------------------------------------
head(hof, 5)


## ----remove active players--------------------------------------------------------------------------------------------------------------------------------------------------------
hof<-subset(hof, In_Hall_of_fame != 2)

summary(as.factor(hof$In_Hall_of_fame))


## ----combine datasets-------------------------------------------------------------------------------------------------------------------------------------------------------------
# rename hof player column to match
colnames(hof)[colnames(hof) == "Name"] <- "Player"

# combine df
career.hof<-merge(career, hof, by="Player")

# check data
head(career.hof, 5)
dim(career.hof)
summary(as.factor(career.hof$In_Hall_of_fame))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract data
write.csv(career.hof,file='hof_fixed.csv',)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# read back in data after adding awards in excel
final.data <- read.csv("Data/hof_post_excel.csv")

# now I need to make one last df where all awards are 1 or 0
final.data <- final.data %>%
  mutate(
    MVP = ifelse(MVP > 0, 1, 0),
    FMVP = ifelse(FMVP > 0, 1, 0),
    DPOY = ifelse(DPOY > 0, 1, 0),
  )

# check data
summary(final.data)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(final.data,file='hof_final.csv',)

