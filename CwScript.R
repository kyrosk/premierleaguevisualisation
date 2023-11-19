install.packages(c("ggplot2","dplyr","gganimate","gifski","RColorBrewer","plotly"))

install.packages('gganimate')

library(ggplot2)
library(dplyr)

library(gganimate)
library(gifski)

library(RColorBrewer)
library(plotly)

#We import the dataset with all the years in it
generaldata <- read.csv("Dataset/final_dataset.csv", header=FALSE)

names(generaldata) <- c("Unkown","Date","HomeTeam","AwayTeam","FinalTimeHomeGoal","FinalTimeAwayGoal")

#We edit the format of date

generaldata$Date <- as.Date(generaldata$Date , format = "%d/%m/%Y")

#Question1 : How many home goals did liverpool score in 2016-2017 and how many in 2017-2018 ? Was there an improvement?

# Firstly we obtain the columns that we are interested in and we create another dataset so we can work on it.

Q1data <- generaldata %>% select(c('Date','HomeTeam','AwayTeam','FinalTimeHomeGoal','FinalTimeAwayGoal'))
Q1data$Date <- as.Date(Q1data$Date,format = "%d/%m/%Y")

#Now we select the team that we want to examine, and the season we want to examine to different datasets
Q1Liverpool1617 <- Q1data %>% filter(HomeTeam == 'Liverpool') %>% subset(Date>="2016-08-13" & Date<="2017-05-21")
Q1Liverpool1718 <- Q1data %>% filter(HomeTeam == 'Liverpool') %>% subset(Date>="2017-08-11" & Date<="2018-05-13")

#We convert the columns that we want to compare to numeric classes
x_num_1617 <- as.numeric(as.character(Q1Liverpool1617$FinalTimeHomeGoal))
x_num_1718 <- as.numeric(as.character(Q1Liverpool1718$FinalTimeHomeGoal))

#We find the sum of them
sum1617 <- sum(x_num_1617)
sum1718 <- sum(x_num_1718)

#Create the columns and rows
SumDate <- c("Sum1617","Sum1718")
GoalsScored <- c(sum1617,sum1718)

#we create a dataset to store our variables sto we can properly create a bar graph
Q1Finale <- data.frame(SumDate,GoalsScored)
h<-Q1Finale

#draw the bar graph using ggplot
ggplot(h, aes(GoalsScored,SumDate)) +geom_bar(stat = "identity",colour = "yellow",fill = "red")

#Question 2 : Which is the referee that blew the whistle the most in the 2015-2016 season? Which was the team that refereed the most?
Q2Data <- read.csv("Dataset/2015-16.csv", header=FALSE)
names(Q2Data) <-  c("Division","Date","HomeTeam","AwayTeam","FinalTimeHomeGoal","FinalTimeAwayGoal","FullTimeResult","HalfTimeHomeGoal","HalfTimeAwayGoal","HalfTimeResult","Referee")

#Firstly we create a table to view how many times each referee blew the whistle
MostRefereesBlown <- table(Q2Data$Referee)

#We transform our table to dataset
MostRefereesDataset <- as.data.frame(MostRefereesBlown)

#We rename the columns
names(MostRefereesDataset) <- c("Referee","WhistleTimes")

#We delete the row assigned to the string "Referee" to clean our dataset
MostRefereesDatasetCorrected <- MostRefereesDataset[-c(18),]

#We create the ggplot for our question "Which Referee blew the whislte most in the 2015-16 season"
ggplot(MostRefereesDatasetCorrected , aes(Referee ,WhistleTimes,group=1)) + geom_line(size=1,colour="blue") + geom_point(colour="black",size=2) + scale_y_continuous(labels = as.character(MostRefereesDatasetCorrected$WhistleTimes),breaks=MostRefereesDatasetCorrected$WhistleTimes)

#To solve the other part of the question we will need to select 3 columns , the teams and the referees
Q2part2 <- Q2Data %>% select(c("HomeTeam","AwayTeam","Referee"))

#We filter to get only the games M Dean blew the whistle
Q2part2 <- Q2part2 %>% filter(Referee=="M Dean")

HomeTeamValues <- table(Q2part2$HomeTeam)

AwayTeamValues <- table(Q2part2$AwayTeam)

#We create data frames with the times all the teams appear in the specific column(Home team or Away team)

HomeTeamValuesDF <- as.data.frame(HomeTeamValues)

AwayTeamValuesDF <- as.data.frame(AwayTeamValues)

#We rename the columns

names(HomeTeamValuesDF) <- c("Teams","Times")

names(AwayTeamValuesDF) <- c("Teams","Times")

#We take the sum of the two columns that show the times a specific team is shown in the column to find all the times the particular referee was assigned to this match

SumofTheTimes <- HomeTeamValuesDF$Times + AwayTeamValuesDF$Times

#We drop all the teams in one variable

Alltheteams <- HomeTeamValuesDF$Teams

#We put the values SumofTheTimes and Alltheteams in one dataframe

FinalDataframeQ2part2 <- data.frame(Alltheteams,SumofTheTimes)

#We clean our dataset from the value "HomeTeam"

FinalDataframeQ2part2Corrected <- FinalDataframeQ2part2[-c(7),]

#We then create our ggplot for the final part of question2

ggplot(FinalDataframeQ2part2Corrected, aes(x=Alltheteams,y=SumofTheTimes)) + geom_col(fill="blue")

#Refined question for question 2 : Who was in the 2nd place that refereed the team that M Dean refereed the most ?

#We know that the team is Manchester United , so we start again and we choose manchester united

Q2Refined <- Q2Data %>% select(c("HomeTeam","AwayTeam","Referee"))

Q2RefinedH <- Q2Refined %>% filter ( HomeTeam =="Man United")

Q2RefinedA <- Q2Refined %>% filter( AwayTeam == "Man United")

Q2RefereedH <- table(Q2RefinedH$Referee)

Q2RefereedHVal <- as.data.frame(Q2RefereedH)

Q2ReefereedA <- table(Q2RefinedA$Referee)

Q2RefereedAVal <- as.data.frame(Q2ReefereedA)

#We store the frequency of the whistles each referee has blown
SumofrefereedManUtd <- Q2RefereedHVal$Freq + Q2RefereedAVal$Freq
RefereesCol <- Q2RefereedHVal$Var1

#We store into a dataframe the frequency of referees that blew the whistle for Man utd
FinalDataframeQ2Refined <- data.frame(RefereesCol , SumofrefereedManUtd)

#We clean our dataframe
FinalDataframeQ2RefinedCorrected <- FinalDataframeQ2Refined[-c(18),]
ggplot(FinalDataframeQ2RefinedCorrected , aes(x=RefereesCol,y=SumofrefereedManUtd)) + geom_area(fill="Green" , alpha = .3 , group=1) 

#Question3: Which month among the first 5 full months(Sept-Oct-Nov-Dec-January) of the 2010-11 season can be idenitified with the highest average number of home goals from teams that their name start with A and M? What can we say about those months?

#Firstly we create 5 datasets that will be needed to work things out

Q31011 <- generaldata %>% select(c("Date","HomeTeam","FinalTimeHomeGoal"))

Q31011$Date <- as.Date(Q31011$Date,format = "%d/%m/%Y")

Q31011 <- Q31011 %>% filter(Date >= "2010-08-14" & Date<= "2011-05-22")

Q31011SEP <- Q31011 %>% filter(Date >= "2010-09-11" & Date<= "2010-09-26" , grepl('A',HomeTeam) | grepl('M',HomeTeam))

Q31011OCT <- Q31011 %>% filter(Date >= "2010-10-02" & Date<= "2010-10-31" , grepl('A',HomeTeam) | grepl('M',HomeTeam))

Q31011NOV <- Q31011 %>% filter(Date >= "2010-11-01" & Date<= "2010-11-28" , grepl('A',HomeTeam) | grepl('M',HomeTeam))

Q31011DEC <- Q31011 %>% filter(Date >= "2010-12-04" & Date<= "2010-12-29" , grepl('A',HomeTeam) | grepl('M',HomeTeam))

Q31011JAN <- Q31011 %>% filter(Date >= "2011-01-01" & Date<= "2011-01-26" , grepl('A',HomeTeam) | grepl('M',HomeTeam))

#Now for each month we have to find the average home goals scored, but first we need to transform our columns to numeric ones

Q31011SEP$FinalTimeHomeGoal <- as.numeric(as.character(Q31011SEP$FinalTimeHomeGoal))

Q31011OCT$FinalTimeHomeGoal <- as.numeric(as.character(Q31011OCT$FinalTimeHomeGoal))

Q31011NOV$FinalTimeHomeGoal <- as.numeric(as.character(Q31011NOV$FinalTimeHomeGoal))

Q31011DEC$FinalTimeHomeGoal <- as.numeric(as.character(Q31011DEC$FinalTimeHomeGoal))

Q31011JAN$FinalTimeHomeGoal <- as.numeric(as.character(Q31011JAN$FinalTimeHomeGoal))

AVGSEP <- mean(Q31011SEP$FinalTimeHomeGoal)

AVGOCT <- mean(Q31011OCT$FinalTimeHomeGoal)

AVGNOV <- mean(Q31011NOV$FinalTimeHomeGoal)

AVGDEC <- mean(Q31011DEC$FinalTimeHomeGoal)

AVGJAN <- mean(Q31011JAN$FinalTimeHomeGoal)

#Next we are going to create a new dataframe based on how many teams were playing home matches , our average number and the month

Q3AVGFINAL <- data.frame(Month = c("September","October","November","December","January") , Numberofteams = c(6,8,10,8,9), Average = c(AVGSEP,AVGOCT,AVGNOV,AVGDEC,AVGJAN))

ggplot(Q3AVGFINAL, aes(x = Average, y = Numberofteams)) + geom_point()+ geom_label(aes(label = Month), size = 3) + scale_y_continuous(labels = as.character(Q3AVGFINAL$Numberofteams),breaks=Q3AVGFINAL$Numberofteams) + scale_x_continuous(labels = as.character(Q3AVGFINAL$Average),breaks=Q3AVGFINAL$Average)

#Question4 : Discuss the amount of yellow cards and red cards of home and away teams given from 2000 until 2010

#Firstly we need to obtain into seperate datasets the years from 2000 until 2010 , good news we have seperate datasets for this

Q40001 <- read.csv("Dataset/2000-01.csv", header=TRUE)

Q40102 <- read.csv("Dataset/2001-02.csv", header=TRUE)

Q40203 <- read.csv("Dataset/2002-03.csv", header=TRUE)

Q40304 <- read.csv("Dataset/2003-04.csv", header=TRUE)

Q40405 <- read.csv("Dataset/2004-05.csv", header=TRUE)

Q40506 <- read.csv("Dataset/2005-06.csv", header=TRUE)

Q40607 <- read.csv("Dataset/2006-07.csv", header=TRUE)

Q40708 <- read.csv("Dataset/2007-08.csv", header=TRUE)

Q40809 <- read.csv("Dataset/2008-09.csv", header=TRUE)

Q40910 <- read.csv("Dataset/2009-10.csv", header=TRUE)

#All we have to do is to pick from each year the HY(home yellow) AY(away yellow) HR(Home red) AR( Away red)

Q40001 <- Q40001 %>% select(c("HR","AR","HY","AY"))

Q40102 <- Q40102 %>% select(c("HR","AR","HY","AY"))

Q40203 <- Q40203 %>% select(c("HR","AR","HY","AY"))

Q40304 <- Q40304 %>% select(c("HR","AR","HY","AY"))

Q40405 <- Q40405 %>% select(c("HR","AR","HY","AY"))

Q40506 <- Q40506 %>% select(c("HR","AR","HY","AY"))

Q40607 <- Q40607 %>% select(c("HR","AR","HY","AY"))

Q40708 <- Q40708 %>% select(c("HR","AR","HY","AY"))

Q40809 <- Q40809 %>% select(c("HR","AR","HY","AY"))

Q40910 <- Q40910 %>% select(c("HR","AR","HY","AY"))

#Find the sums

Sumof00HR <- sum(as.numeric(as.character(Q40001$HR)))

Sumof00AR <- sum(as.numeric(as.character(Q40001$AR)))

Sumof00HY <- sum(as.numeric(as.character(Q40001$HY)))

Sumof00AY <- sum(as.numeric(as.character(Q40001$AY)))

Sumof01HR <- sum(as.numeric(as.character(Q40102$HR)))

Sumof01AR <- sum(as.numeric(as.character(Q40102$AR)))

Sumof01HY <- sum(as.numeric(as.character(Q40102$HY)))

Sumof01AY <- sum(as.numeric(as.character(Q40102$AY)))

Sumof02HR <- sum(as.numeric(as.character(Q40203$HR)))

Sumof02AR <- sum(as.numeric(as.character(Q40203$AR)))

Sumof02HY <- sum(as.numeric(as.character(Q40203$HY)))

Sumof02AY <- sum(as.numeric(as.character(Q40203$AY)))

Sumof03HR <- sum(as.numeric(as.character(Q40304$HR)))

Sumof03AR <- sum(as.numeric(as.character(Q40304$AR)))

Sumof03HY <- sum(as.numeric(as.character(Q40304$HY)))

Sumof03AY <- sum(as.numeric(as.character(Q40304$AY)))

Sumof04HR <- sum(as.numeric(as.character(Q40405$HR)))

Sumof04AR <- sum(as.numeric(as.character(Q40405$AR)))

Sumof04HY <- sum(as.numeric(as.character(Q40405$HY)))

Sumof04AY <- sum(as.numeric(as.character(Q40405$AY)))

Sumof05HR <- sum(as.numeric(as.character(Q40506$HR)))

Sumof05AR <- sum(as.numeric(as.character(Q40506$AR)))

Sumof05HY <- sum(as.numeric(as.character(Q40506$HY)))

Sumof05AY <- sum(as.numeric(as.character(Q40506$AY)))

Sumof06HR <- sum(as.numeric(as.character(Q40607$HR)))

Sumof06AR <- sum(as.numeric(as.character(Q40607$AR)))

Sumof06HY <- sum(as.numeric(as.character(Q40607$HY)))

Sumof06AY <- sum(as.numeric(as.character(Q40607$AY)))

Sumof07HR <- sum(as.numeric(as.character(Q40708$HR)))

Sumof07AR <- sum(as.numeric(as.character(Q40708$AR)))

Sumof07HY <- sum(as.numeric(as.character(Q40708$HY)))

Sumof07AY <- sum(as.numeric(as.character(Q40708$AY)))

Sumof08HR <- sum(as.numeric(as.character(Q40809$HR)))

Sumof08AR <- sum(as.numeric(as.character(Q40809$AR)))

Sumof08HY <- sum(as.numeric(as.character(Q40809$HY)))

Sumof08AY <- sum(as.numeric(as.character(Q40809$AY)))

Sumof09HR <- sum(as.numeric(as.character(Q40910$HR)))

Sumof09AR <- sum(as.numeric(as.character(Q40910$AR)))

Sumof09HY <- sum(as.numeric(as.character(Q40910$HY)))

Sumof09AY <- sum(as.numeric(as.character(Q40910$AY)))

#We create a data frame with the relevant information

LastDFofQ4 <- data.frame( Year = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009), HR = c(Sumof00HR,Sumof01HR,Sumof02HR,Sumof03HR,Sumof04HR,Sumof05HR,Sumof06HR,Sumof07HR,Sumof08HR,Sumof09HR),AR = c(Sumof00AR,Sumof01AR,Sumof02AR,Sumof03AR,Sumof04AR,Sumof05AR,Sumof06AR,Sumof07AR,Sumof08AR,Sumof09AR),HY = c(Sumof00HY,Sumof01HY,Sumof02HY,Sumof03HY,Sumof04HY,Sumof05HY,Sumof06HY,Sumof07HY,Sumof08HY,Sumof09HY),AY = c(Sumof00AY,Sumof01AY,Sumof02AY,Sumof03AY,Sumof04AY,Sumof05AY,Sumof06AY,Sumof07AY,Sumof08AY,Sumof09AY))

ggplot(data = LastDFofQ4 ) + geom_point(mapping = aes(x = HR, y = AR,size=6))  + facet_wrap(~ Year, nrow = 3) + scale_y_continuous(labels = as.character(LastDFofQ4$AR),breaks=LastDFofQ4$AR) + scale_x_continuous(labels = as.character(LastDFofQ4$HR),breaks=LastDFofQ4$HR)

ggplot(data = LastDFofQ4 ) + geom_point(mapping = aes(x = HY, y = AY,size=6))  + facet_wrap(~ Year, nrow =3)

#Question5: Show the overall progress of Liverpool team if we substract Home wins and losses game count all the 8 years from 2010-2018

#Lets import datasets from 2010 until 2018

Q51011 <- read.csv("Dataset/2010-11.csv", header=TRUE)

Q51112 <- read.csv("Dataset/2011-12.csv", header=TRUE)

Q51213 <- read.csv("Dataset/2012-13.csv", header=TRUE)

Q51314 <- read.csv("Dataset/2013-14.csv", header=TRUE)

Q51415 <- read.csv("Dataset/2014-15.csv", header=TRUE)

Q51516 <- read.csv("Dataset/2015-16.csv", header=TRUE)

Q51617 <- read.csv("Dataset/2016-17.csv", header=TRUE)

Q51718 <- read.csv("Dataset/2017-18.csv", header=TRUE)

#We need to choose again the columns we want

Q51011 <- Q51011 %>% select(c("HomeTeam","FTR"))

Q51112 <- Q51112 %>% select(c("HomeTeam","FTR"))

Q51213 <- Q51213 %>% select(c("HomeTeam","FTR"))

Q51314 <- Q51314 %>% select(c("HomeTeam","FTR"))

Q51415 <- Q51415 %>% select(c("HomeTeam","FTR"))

Q51516 <- Q51516 %>% select(c("HomeTeam","FTR"))

Q51617 <- Q51617 %>% select(c("HomeTeam","FTR"))

Q51718 <- Q51718 %>% select(c("HomeTeam","FTR"))

#Filter the data

Q51011 <- Q51011 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51112 <- Q51112 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51213 <- Q51213 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51314 <- Q51314 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51415 <- Q51415 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51516 <- Q51516 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51617 <- Q51617 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51718 <- Q51718 %>% filter(HomeTeam == "Liverpool" , FTR == "H" | FTR == "A")

#Count the loses and wins by observating if the FTR is H then is a win , otherwise is a lost.

Q51011DF <- data.frame(table(Q51011$FTR))

Q51112DF <- data.frame(table(Q51112$FTR))

Q51213DF <- data.frame(table(Q51213$FTR))

Q51314DF <- data.frame(table(Q51314$FTR))

Q51415DF <- data.frame(table(Q51415$FTR))

Q51516DF <- data.frame(table(Q51516$FTR))

Q51617DF <- data.frame(table(Q51617$FTR))

Q51718DF <- data.frame(table(Q51718$FTR))

#Store the wins and losses into different variables

FREQ1011W <- as.numeric(as.character(Q51011DF[3,"Freq"]))

FREQ1011L <- as.numeric(as.character(Q51011DF[1,"Freq"]))

FREQ1112W <- as.numeric(as.character(Q51112DF[3,"Freq"]))

FREQ1112L <- as.numeric(as.character(Q51112DF[1,"Freq"]))

FREQ1213W <- as.numeric(as.character(Q51213DF[3,"Freq"]))

FREQ1213L <- as.numeric(as.character(Q51213DF[1,"Freq"]))

FREQ1314W <- as.numeric(as.character(Q51314DF[3,"Freq"]))

FREQ1314L <- as.numeric(as.character(Q51314DF[1,"Freq"]))

FREQ1415W <- as.numeric(as.character(Q51415DF[3,"Freq"]))

FREQ1415L <- as.numeric(as.character(Q51415DF[1,"Freq"]))

FREQ1516W <- as.numeric(as.character(Q51516DF[3,"Freq"]))

FREQ1516L <- as.numeric(as.character(Q51516DF[1,"Freq"]))

FREQ1617W <- as.numeric(as.character(Q51617DF[3,"Freq"]))

FREQ1617L <- as.numeric(as.character(Q51617DF[1,"Freq"]))

FREQ1718W <- as.numeric(as.character(Q51718DF[3,"Freq"]))

FREQ1718L <- as.numeric(as.character(Q51718DF[1,"Freq"]))

FinalDatasetQ5 <-  data.frame(Year = c(2010,2011,2012,2013,2014,2015,2016,2017) , HomeWins = c(FREQ1011W,FREQ1112W,FREQ1213W,FREQ1314W,FREQ1415W,FREQ1516W,FREQ1617W,FREQ1718W) , HomeLosses = c(FREQ1011L,FREQ1112L,FREQ1213L,FREQ1314L,FREQ1415L,FREQ1516L,FREQ1617L,FREQ1718L))

Animated <- ggplot(FinalDatasetQ5, aes(x = HomeWins, y=HomeLosses, size = 2, colour = "RED")) + geom_point(show.legend = FALSE, alpha = 0.7) + ease_aes('cubic-in-out') +scale_color_viridis_d() +scale_size(range = c(2, 12)) + transition_time(as.integer(FinalDatasetQ5$Year)) + labs(title = "Year: {frame_time}") +  scale_y_continuous(labels = as.character(FinalDatasetQ5$HomeLosses),breaks=FinalDatasetQ5$HomeLosses) + scale_x_continuous(labels = as.character(FinalDatasetQ5$HomeWins),breaks=FinalDatasetQ5$HomeWins)

#We animate our plot

animate(Animated,renderer = gifski_renderer())

#Question 5 Refined : does this get any different if we take the away wins and loses?

#We import the data

Q51011P2 <- read.csv("Dataset/2010-11.csv", header=TRUE)

Q51112P2 <- read.csv("Dataset/2011-12.csv", header=TRUE)

Q51213P2 <- read.csv("Dataset/2012-13.csv", header=TRUE)

Q51314P2 <- read.csv("Dataset/2013-14.csv", header=TRUE)

Q51415P2 <- read.csv("Dataset/2014-15.csv", header=TRUE)

Q51516P2 <- read.csv("Dataset/2015-16.csv", header=TRUE)

Q51617P2 <- read.csv("Dataset/2016-17.csv", header=TRUE)

Q51718P2 <- read.csv("Dataset/2017-18.csv", header=TRUE)

#We choose the fields we want

Q51011P2 <- Q51011P2 %>% select(c("AwayTeam","FTR"))

Q51112P2 <- Q51112P2 %>% select(c("AwayTeam","FTR"))

Q51213P2 <- Q51213P2 %>% select(c("AwayTeam","FTR"))

Q51314P2 <- Q51314P2 %>% select(c("AwayTeam","FTR"))

Q51415P2 <- Q51415P2 %>% select(c("AwayTeam","FTR"))

Q51516P2 <- Q51516P2 %>% select(c("AwayTeam","FTR"))

Q51617P2 <- Q51617P2 %>% select(c("AwayTeam","FTR"))

Q51718P2 <- Q51718P2 %>% select(c("AwayTeam","FTR"))

#We filter our columns

Q51011P2 <- Q51011P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51112P2 <- Q51112P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51213P2 <- Q51213P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51314P2 <- Q51314P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51415P2 <- Q51415P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51516P2 <- Q51516P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51617P2 <- Q51617P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

Q51718P2 <- Q51718P2 %>% filter(AwayTeam == "Liverpool" , FTR == "H" | FTR == "A")

#We assign to dataframes the final results of the away games of liverpool for each season

Q51011DFP2 <- data.frame(table(Q51011P2$FTR))

Q51112DFP2 <- data.frame(table(Q51112P2$FTR))

Q51213DFP2 <- data.frame(table(Q51213P2$FTR))

Q51314DFP2 <- data.frame(table(Q51314P2$FTR))

Q51415DFP2 <- data.frame(table(Q51415P2$FTR))

Q51516DFP2 <- data.frame(table(Q51516P2$FTR))

Q51617DFP2 <- data.frame(table(Q51617P2$FTR))

Q51718DFP2 <- data.frame(table(Q51718P2$FTR))

#We store the wins and losses

FREQ1011AW <- as.numeric(as.character(Q51011DFP2[1,"Freq"]))

FREQ1011AL <- as.numeric(as.character(Q51011DFP2[3,"Freq"]))

FREQ1112AW <- as.numeric(as.character(Q51112DFP2[1,"Freq"]))

FREQ1112AL <- as.numeric(as.character(Q51112DFP2[3,"Freq"]))

FREQ1213AW <- as.numeric(as.character(Q51213DFP2[1,"Freq"]))

FREQ1213AL <- as.numeric(as.character(Q51213DFP2[3,"Freq"]))

FREQ1314AW <- as.numeric(as.character(Q51314DFP2[1,"Freq"]))

FREQ1314AL <- as.numeric(as.character(Q51314DFP2[3,"Freq"]))

FREQ1415AW <- as.numeric(as.character(Q51415DFP2[1,"Freq"]))

FREQ1415AL <- as.numeric(as.character(Q51415DFP2[3,"Freq"]))

FREQ1516AW <- as.numeric(as.character(Q51516DFP2[1,"Freq"]))

FREQ1516AL <- as.numeric(as.character(Q51516DFP2[3,"Freq"]))

FREQ1617AW <- as.numeric(as.character(Q51617DFP2[1,"Freq"]))

FREQ1617AL <- as.numeric(as.character(Q51617DFP2[3,"Freq"]))

FREQ1718AW <- as.numeric(as.character(Q51718DFP2[1,"Freq"]))

FREQ1718AL <- as.numeric(as.character(Q51718DFP2[3,"Freq"]))

#We create a dataframe to put things in the correct order

FinalDataFrameQ5Ref <-  data.frame(Year = c(2010,2010,2010,2010,2011,2011,2011,2011,2012,2012,2012,2012,2013,2013,2013,2013,2014,2014,2014,2014,2015,2015,2015,2015,2016,2016,2016,2016,2017,2017,2017,2017) , Number = c(FREQ1011W,FREQ1011L,FREQ1011AW,FREQ1011AL,FREQ1112W,FREQ1112L,FREQ1112AW,FREQ1112AL,FREQ1213W,FREQ1213L,FREQ1213AW,FREQ1213AL,FREQ1314W,FREQ1314L,FREQ1314AW,FREQ1314AL,FREQ1415W,FREQ1415L,FREQ1415AW,FREQ1415AL,FREQ1516W,FREQ1516L,FREQ1516AW,FREQ1516AL,FREQ1617W,FREQ1617L,FREQ1617AW,FREQ1617AL,FREQ1718W,FREQ1718L,FREQ1718AW,FREQ1718AL) , Type = c("HomeWins","HomeLosses","AwayWins","AwayLosses","HomeWins","HomeLosses","AwayWins","AwayLosses","HomeWins","HomeLosses","AwayWins","AwayLosses","HomeWins","HomeLosses","AwayWins","AwayLosses","HomeWins","HomeLosses","AwayWins","AwayLosses","HomeWins","HomeLosses","AwayWins","AwayLosses","HomeWins","HomeLosses","AwayWins","AwayLosses","HomeWins","HomeLosses","AwayWins","AwayLosses"))

#We create our ggplot

Q5Plot <- ggplot(FinalDataFrameQ5Ref,aes(Year, Number, group = Type, color = factor(Type))) +geom_line() +scale_color_viridis_d() +labs(x = "Year", y = "Number") +theme(legend.position = "top") + geom_point() + transition_reveal(Year) + scale_x_continuous(labels = as.character(FinalDataFrameQ5Ref$Year),breaks=FinalDataFrameQ5Ref$Year) + scale_y_continuous(labels = as.character(FinalDataFrameQ5Ref$Number),breaks=FinalDataFrameQ5Ref$Number) + scale_color_manual(values =  c("AwayLosses" = "Blue","AwayWins" = "Red","HomeLosses" = "Green","HomeWins" = "Orange"))

Q5Plot

#Further proposed question1 : Which team commited most fouls during the  game time (16:30) the month october of the 2019-20 season

#We first extract the data.
FPQ1920 <- read.csv("Dataset/2019-20.csv", header=TRUE)

#We extract the columns we want to work with.
FPQ1920 <- FPQ1920 %>% select(c("HF","AF","Date","Time","HomeTeam","AwayTeam"))

#We filter the data to collect only the timeslot 16:30 and the month october
FPQ1920$Date <- as.Date(FPQ1920$Date,format = "%d/%m/%Y")
FPQ1920 <- FPQ1920 %>% filter(Date >= "2019-10-05" & Date<= "2019-10-27")
FPQ1920 <- FPQ1920 %>% filter(Time == "16:30")

#We can see now from our dataframe which teams competed in this time , so we start observating the results.
ManUtdFouls <-  as.numeric(as.character(FPQ1920[1,"AF"])) + as.numeric(as.character(FPQ1920[2,"HF"])) + as.numeric(as.character(FPQ1920[5,"AF"]))
NewcastleFouls <- as.numeric(as.character(FPQ1920[1,"HF"]))
LiverpoolFouls <- as.numeric(as.character(FPQ1920[2,"AF"])) + as.numeric(as.character(FPQ1920[4,"HF"]))
ArsenalFouls <- as.numeric(as.character(FPQ1920[3,"HF"]))
CrystalPalaceFouls <- as.numeric(as.character(FPQ1920[3,"AF"]))
TottenhamFouls <- as.numeric(as.character(FPQ1920[4,"AF"]))
NorwichFouls <- as.numeric(as.character(FPQ1920[5,"HF"]))
FPQ1920Final<- data.frame( Team = (c("Man Utd","Newcastle","Liverpool","Arsenal","CrystalPalace","Tottenham","Norwich")), Fouls = c(ManUtdFouls,NewcastleFouls,LiverpoolFouls,ArsenalFouls,CrystalPalaceFouls,TottenhamFouls,NorwichFouls), Played=(c(3,1,2,1,1,1,1)))
scatterPlot <- FPQ1920Final %>% ggplot(aes(x = Team, y = Fouls,group=Played, color=factor(Played))) + geom_point(alpha=0.8) + labs(title = "Fouls commited by Premier league teams that competed during the month october at timeslot 16:30 ",size = 1)

#We create our interactive plot
ggplotly(scatterPlot)

#Further proposed question2 : Show by an animated graph which year between 2005-2010 had the worst difference of Shots and shots on target. For example if 2010-11 there were 50 shots and 40 of them was on target then we have 50-40=10 . If 2011-12 there were 60 shots and 40 of them on target we have 60-40= 20 which is worse because team's shots were less on target by missing 20 shots out of 60.

#We import datasets

FPQ0506 <- read.csv("Dataset/2005-06.csv", header=TRUE)

FPQ0607 <- read.csv("Dataset/2006-07.csv", header=TRUE)

FPQ0708 <- read.csv("Dataset/2007-08.csv", header=TRUE)

FPQ0809 <- read.csv("Dataset/2008-09.csv", header=TRUE)

FPQ0910 <- read.csv("Dataset/2009-10.csv", header=TRUE)

FPQ0506 <- FPQ0506 %>% select(c("HS","AS","HST","AST"))

FPQ0607 <- FPQ0607 %>% select(c("HS","AS","HST","AST"))

FPQ0708 <- FPQ0708 %>% select(c("HS","AS","HST","AST"))

FPQ0809 <- FPQ0809 %>% select(c("HS","AS","HST","AST"))

FPQ0910 <- FPQ0910 %>% select(c("HS","AS","HST","AST"))

#We store into variables the sums of the specific shot type.

FPQ0506SUMHomeShots <-  sum(as.numeric(as.character(FPQ0506$HS)))

FPQ0607SUMHomeShots <-  sum(as.numeric(as.character(FPQ0607$HS)))

FPQ0708SUMHomeShots <-  sum(as.numeric(as.character(FPQ0708$HS)))

FPQ0809SUMHomeShots <-  sum(as.numeric(as.character(FPQ0809$HS)))

FPQ0910SUMHomeShots <-  sum(as.numeric(as.character(FPQ0910$HS)))

FPQ0506SUMAwayShots <-  sum(as.numeric(as.character(FPQ0506$AS)))

FPQ0607SUMAwayShots <-  sum(as.numeric(as.character(FPQ0607$AS)))

FPQ0708SUMAwayShots <-  sum(as.numeric(as.character(FPQ0708$AS)))

FPQ0809SUMAwayShots <-  sum(as.numeric(as.character(FPQ0809$AS)))

FPQ0910SUMAwayShots <-  sum(as.numeric(as.character(FPQ0910$AS)))

FPQ0506SUMHomeShotsOT <-  sum(as.numeric(as.character(FPQ0506$HST)))

FPQ0607SUMHomeShotsOT <-  sum(as.numeric(as.character(FPQ0607$HST)))

FPQ0708SUMHomeShotsOT <-  sum(as.numeric(as.character(FPQ0708$HST)))

FPQ0809SUMHomeShotsOT <-  sum(as.numeric(as.character(FPQ0809$HST)))

FPQ0910SUMHomeShotsOT <-  sum(as.numeric(as.character(FPQ0910$HST)))

FPQ0506SUMAwayShotsOT <-  sum(as.numeric(as.character(FPQ0506$AST)))

FPQ0607SUMAwayShotsOT <-  sum(as.numeric(as.character(FPQ0607$AST)))

FPQ0708SUMAwayShotsOT <-  sum(as.numeric(as.character(FPQ0708$AST)))

FPQ0809SUMAwayShotsOT <-  sum(as.numeric(as.character(FPQ0809$AST)))

FPQ0910SUMAwayShotsOT <-  sum(as.numeric(as.character(FPQ0910$AST)))


#We take the sum of general shots and the sum of those on target

FPQ0506SUMGeneral <- FPQ0506SUMHomeShots + FPQ0506SUMAwayShots

FPQ0607SUMGeneral <- FPQ0607SUMHomeShots + FPQ0607SUMAwayShots

FPQ0708SUMGeneral <- FPQ0708SUMHomeShots + FPQ0708SUMAwayShots

FPQ0809SUMGeneral <- FPQ0809SUMHomeShots + FPQ0809SUMAwayShots

FPQ0910SUMGeneral <- FPQ0910SUMHomeShots + FPQ0910SUMAwayShots

FPQ0506SUMGeneralOT <- FPQ0506SUMHomeShotsOT + FPQ0506SUMAwayShotsOT

FPQ0607SUMGeneralOT <- FPQ0607SUMHomeShotsOT + FPQ0607SUMAwayShotsOT

FPQ0708SUMGeneralOT <- FPQ0708SUMHomeShotsOT + FPQ0708SUMAwayShotsOT

FPQ0809SUMGeneralOT <- FPQ0809SUMHomeShotsOT + FPQ0809SUMAwayShotsOT

FPQ0910SUMGeneralOT <- FPQ0910SUMHomeShotsOT + FPQ0910SUMAwayShotsOT

#We find the difference

FPQ0506Difference <- FPQ0506SUMGeneral - FPQ0506SUMGeneralOT

FPQ0607Difference <- FPQ0607SUMGeneral - FPQ0607SUMGeneralOT

FPQ0708Difference <- FPQ0708SUMGeneral - FPQ0708SUMGeneralOT

FPQ0809Difference <- FPQ0809SUMGeneral - FPQ0809SUMGeneralOT

FPQ0910Difference <- FPQ0910SUMGeneral - FPQ0910SUMGeneralOT

FinalPQDataFrame <- data.frame( Year = c(2005,2005,2005,2006,2006,2006,2007,2007,2007,2008,2008,2008,2009,2009,2009) , Shots=(c(FPQ0506SUMGeneral,FPQ0506SUMGeneralOT,FPQ0506Difference,FPQ0607SUMGeneral,FPQ0607SUMGeneralOT,FPQ0607Difference,FPQ0708SUMGeneral,FPQ0708SUMGeneralOT,FPQ0708Difference,FPQ0809SUMGeneral,FPQ0809SUMGeneralOT,FPQ0809Difference,FPQ0910SUMGeneral,FPQ0910SUMGeneralOT,FPQ0910Difference)) , Type = c("HomeAwaySum","HomeAwayOnTargetSum","Diference","HomeAwaySum","HomeAwayOnTargetSum","Diference","HomeAwaySum","HomeAwayOnTargetSum","Diference","HomeAwaySum","HomeAwayOnTargetSum","Diference","HomeAwaySum","HomeAwayOnTargetSum","Diference"))

ggplot(FinalPQDataFrame, aes(x = Year, y = Shots, fill = Type)) + geom_col(width = 0.2, position = position_dodge(0.3))  + transition_states(Year, wrap = FALSE) + shadow_mark() +  geom_text(aes(label= as.integer(Shots)))

