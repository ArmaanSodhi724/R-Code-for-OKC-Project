library(tidyverse)
library (ggplot2)
library
LEFT_X_2PT_LINE = -22.0
RIGHT_X_2PT_LINE = 22.0
CORNER_THREE_Y_LINE = 7.8
THREE_PT_ARC = 23.5
ShotData <- read.csv(file = 'data/shots_data.csv')
head (ShotData)

ShotData %>%
  group_by(team) %>%
  ggplot(aes(x=ShotData$x,y=ShotData$y,color=ShotData$team)) +
  geom_point()

twoPointer <- ShotData %>%
  filter(pointDistance(cbind(x,y), c(0,0), lonlat = FALSE)<THREE_PT_ARC) %>%
  filter((x>=LEFT_X_2PT_LINE)&(x<=RIGHT_X_2PT_LINE))
  
  twoPointer %>%
    group_by(team) %>%
    ggplot(aes(x=twoPointer$x,y=twoPointer$y,color=twoPointer$team)) +
    expand_limits(y=c(-5, 30), x= c(-25,25))+
    geom_point() 
  
  
CthreePointer <- ShotData %>%
  filter(((x < LEFT_X_2PT_LINE)|(x > RIGHT_X_2PT_LINE)) & y < CORNER_THREE_Y_LINE)

  CthreePointer %>%
    group_by(team) %>%
    ggplot(aes(x=CthreePointer$x,y=CthreePointer$y,color=CthreePointer$team)) +
    expand_limits(y=c(-5, 30), x= c(-25,25))+
    geom_point() 

threePointer <- ShotData %>%
  filter(pointDistance(cbind(x,y), c(0,0), lonlat = FALSE)>THREE_PT_ARC) %>%
  filter(y > CORNER_THREE_Y_LINE)

  threePointer %>%
    group_by(team) %>%
    ggplot(aes(x=threePointer$x,y=threePointer$y,color=threePointer$team)) +
    expand_limits(y=c(-5, 30), x= c(-25,25))+
    geom_point() 

  ShotData <-ShotData %>%
    mutate(
      shot_type =case_when(
        pointDistance(cbind(x,y), c(0,0), lonlat = FALSE)<THREE_PT_ARC & (x>=LEFT_X_2PT_LINE)&(x<=RIGHT_X_2PT_LINE)~ "2P",
        pointDistance(cbind(x,y), c(0,0), lonlat = FALSE)>THREE_PT_ARC & y>=CORNER_THREE_Y_LINE ~ "3P",
        y<CORNER_THREE_Y_LINE & (((x<LEFT_X_2PT_LINE)|(x>RIGHT_X_2PT_LINE))) ~ "C3P")
    )
  ShotData %>%
    group_by(team,shot_type) %>%
    summarize(fga=n(), fgm = sum(fgmade), fg2PT= (fgm)/fga, efg3PT= (fgm*1.5)/fga) %>%
    mutate(total =sum(fga),
           perAtt = fga / total,
           efg = case_when(
             shot_type == '2P' ~ fg2PT,
             shot_type == '3P' ~ efg3PT,
             shot_type == 'C3P' ~ efg3PT
           )
    )
