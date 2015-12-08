#Make sure your working directory is set to c://Sabermetrics-with-R or to the directory all
#the Github files are pulled from https://github.com/Gr8Dev/Sabermetrics-with-R

setwd("c://Sabermetrics-with-R")
#Install these packages
remove.packages("pitchRx")
remove.packages("xlsx")
remove.packages("plyr")
remove.packages("dplyr")
remove.packages("cowplot")
remove.packages("RSQLite")
install.packages("RSQLite")
install.packages("xlsx")
install.packages("plyr")
install.packages("dplyr")
install.packages("cowplot")
install.packages("pitchRx")

#Load all the packages
library(pitchRx)
library(dplyr)
library(plyr)
library(RSQLite)
library(ggplot2)
library(grid)
library(gridExtra)
require(cowplot)
library(xlsx)
library(data.table)

#Set working directory to the desired directory but here it is set to c://
########################################################################
#Deliverable 1: Read EXcel file and store in data frame and table
######################################################################
FIPstats<-read.xlsx("c:\\Sabermetrics-with-R\\PitchFX2015.xls", 1)

FIPStatsTable <- data.table(FIPstats, key=c("Pitcher", "year","month"))

#Calculate FIP by month
FIPStatsTable$FIPNumerator = ((13*FIPStatsTable$HR) + (3*(FIPStatsTable$BB + FIPStatsTable$H)) -(2*(FIPStatsTable$SO)) )
FIPStatsTable$FIPDenominator    =                     (FIPStatsTable$IP + 3.2)
FIPStatsTable$FIP = FIPStatsTable$FIPNumerator/FIPStatsTable$FIPDenominator
FIPStatsTable$month<-factor(FIPStatsTable$month)

#Plot the graphs
g7 <- ggplot(data=FIPStatsTable, aes(x=month, y=abs(FIP), group = Pitcher, color = Pitcher)) +
  geom_line(size=2) +
  geom_point( size=4, shape=21, fill="white")+ggtitle("2015 FIP Comparison for July-Sep ")+
  scale_y_continuous("FIP",limits=c(0,9)) 

g8 <- ggplot(data=FIPStatsTable, aes(x=month, y=abs(ERA), group = Pitcher, color = Pitcher)) +
  geom_line(size=2) +
  geom_point( size=4, shape=21, fill="white")+ggtitle("2015 ERA Comparison for July-Sep ")+
  scale_y_continuous("ERA",limits=c(0,9)) 

plot_grid(g7, g8, align="h")



####################################################################################
#Deliverable 3: ERA(Earned run Average) baseball metric:  We are going to choose  2015 World Series team
#NY Mets and calculate for their pitchers ERA comparison:
########################################################################
NYMetsPitchers <- c("Bartolo Colon","Hansel Robles","Noah Syndergaard","Alex Torres","Logan Verrett","Matt Harvey","Sean Gilmartin","Jacob deGrom")
NYMetsERA <-c(4.16,3.67,3.24,3.15,3.03,2.71,2.67,2.54)
NYMets<-data.frame(Pitchers=NYMetsPitchers, ERA=NYMetsERA)

packs <- c("png","grid")
lapply(packs, require, character.only = TRUE) 
img <- readPNG("c://Sabermetrics-with-R//newyorkmets.png") 

pict <- rasterGrob(img, interpolate=TRUE) 

nymetsplot<-ggplot(NYMets,aes(x = Pitchers, y = ERA, fill=Pitchers)) + 
  annotation_custom(pict,xmin=3, xmax=6, ymin=2.5, ymax=8)+
  geom_bar(stat = "identity",color="black") +
  scale_y_continuous("ERA",limits=c(0,6)) + coord_flip()+
  scale_x_discrete("Pitchers") +
  theme(axis.text.x=element_text(colour="slateblue4",size=12,face="bold"))+
  theme(axis.text.y=element_text(colour="slateblue4",size=12,face="bold"))+
  theme(axis.title.x=element_text(colour="slateblue4",size=16,face="bold"))+
  theme(axis.title.y=element_text(colour="slateblue4",size=16,face="bold"))+
  theme(plot.title=element_text(colour="slateblue4", face="bold", size=20))+
  ggtitle("2015 ERA NY METS PITCHERS ")+
  theme(strip.text.x = element_text(size=12, face="bold"),strip.background = element_rect(colour="red", fill="#CCCCFF"))+
  theme(panel.background = element_rect(fill='white', colour='red'))

#Plot the graph
nymetsplot
########################################################################


####################################################################################
#Deliverable 2: For each of the above 5 pitchers, we will analyze the pitch types used in the games for 
#July 2015 - Aug 2015.. The PITCHf/x data classifies the pitches as CH(changeup), 
#CU(Curve ball), FA (Fast ball), FC(Clutter), FF(Four-seam fastball), PO(Pitchout),SL(Slider) etc.
########################################################################

my_db <- src_sqlite("GamedayDB.sqlite3", create = TRUE)
scrape(start = "2015-07-01", end = "2015-07-01",suffix = "inning/inning_all.xml", connect = my_db$con)

scrape(start = "2015-07-01", end = "2015-08-30",suffix = "inning/inning_all.xml", connect = my_db$con)

#Create indexes for faster query
dbSendQuery(my_db$con, "CREATE INDEX IUurl_atbat ON atbat(url)")
dbSendQuery(my_db$con, "CREATE INDEX IUurl_pitch ON pitch(url)")
dbSendQuery(my_db$con, "CREATE INDEX IUpitcher_index ON atbat(pitcher_name)")
dbSendQuery(my_db$con, "CREATE INDEX IUdes_index ON pitch(des)")

#Load pitchf/x data into R dataframe - pitch and atbat table data
metaStore <- data.frame(name = character(), FIP= numeric())
IUpitch11 <-select(tbl(my_db,"pitch"),pitch_type, px, pz, des, num, gameday_link,inning,type)
IUatbat11 <-select(tbl(my_db,"atbat"),batter,pitcher_name, batter_name, num, gameday_link, event, stand,inning,b,s)
IUdata <- collect(inner_join(IUpitch11, IUatbat11))

#Filter for 5 pitchers
jake <-c("Jake Arrieta")
david <-c("David Price")
max   <-c("Max Scherzer")
chris <- c("Chris Sale")
clayton <- c("Clayton Kershaw")

#Jake Arrietta
#What types of pitches did Jake Arrietta throw in this game?
IUJake <- IUdata[IUdata$pitcher_name %in% jake,]
with(IUJake, table(pitch_type))
#what were the outcomes of these pitches?
with(IUJake, table(des, pitch_type))
g0<-strikeFX(IUJake,  point.alpha = 1 ,
             layer=facet_wrap(~pitch_type, ncol=2)) +  ggtitle("Jake Arrieta") +
  theme(legend.position = "right", legend.direction = "vertical") +
  theme_bw()

#What types of pitches did Clayton throw in this game?
IUClayton <- IUdata[IUdata$pitcher_name %in% clayton,]
with(IUClayton, table(pitch_type))
#what were the outcomes of these pitches?
with(IUClayton, table(des, pitch_type))
g1<-strikeFX(IUClayton,  point.alpha = 1 ,
             layer=facet_wrap(~pitch_type, ncol=2)) +  ggtitle("Clayton Kershaw") +
  theme(legend.position = "right", legend.direction = "vertical") +
  theme_bw()

#What types of pitches did David Price throw in this game?
IUDavid <- IUdata[IUdata$pitcher_name %in% david,]
with(IUDavid, table(pitch_type))
#what were the outcomes of these pitches?
with(IUDavid, table(des, pitch_type))
g2<-strikeFX(IUDavid,  point.alpha = 1 ,
             layer=facet_wrap(~pitch_type, ncol=2)) +  ggtitle("David Price") +
  theme(legend.position = "right", legend.direction = "vertical") +
  theme_bw()

#Max Scherzer
What types of pitches did Max Scherzer throw in this game?
IUMax <- IUdata[IUdata$pitcher_name %in% max,]
with(IUMax, table(pitch_type))
#what were the outcomes of these pitches?
with(IUMax, table(des, pitch_type))
g3<-strikeFX(IUMax,  point.alpha = 1 ,
             layer=facet_wrap(~pitch_type, ncol=2)) +  ggtitle("Max Scherzer") +
  theme(legend.position = "right", legend.direction = "vertical") +
  theme_bw()

#Chris Sale
#What types of pitches did Chris Sale throw in this game?
IUChris <- IUdata[IUdata$pitcher_name %in% chris,]
with(IUChris, table(pitch_type))
#what were the outcomes of these pitches?
with(IUChris, table(des, pitch_type))
g4<-strikeFX(IUChris,  point.alpha = 1 ,
             layer=facet_wrap(~pitch_type, ncol=2)) +  ggtitle("Chris Sale") +
  theme(legend.position = "right", legend.direction = "vertical") +
  theme_bw()

#Plot all the  graphs in the grid
plot_grid(g0,g1,g2,g3,g4,  align='h')

