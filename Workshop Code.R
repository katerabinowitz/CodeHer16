#loading the R libraries we need today
require(dplyr)
require(ggplot2)

#read in our data
dcPop<-read.csv("https://raw.githubusercontent.com/katerabinowitz/CodeHer16/master/data/dcPopulation.csv")

#let's look at the structure of our data
str(dcPop)
#what happens when we don't use assign operator (<-)
read.csv("https://raw.githubusercontent.com/katerabinowitz/CodeHer16/master/data/dcPopulation.csv")

#let's call a single variable
dcPop$ward

#changing and then checking variable types
dcPop<- mutate(dcPop, ward = factor(ward), year=factor(year))
str(dcPop)

#get a quick statistical summary of our data
summary(dcPop)
#checking a single stat for a single variable
sum(dcPop$totalPop)

#how to group variables so you can get summarize statistics by categorical variables 
dcGroup <- group_by(dcPop, year)
totalPop <- summarise(dcGroup,
                      dcPop = sum(totalPop)
)

#plotting a bar graph
ggplot(totalPop, aes(x=year, y=dcPop))

ggplot(totalPop, aes(x=year, y=dcPop)) + 
  geom_bar(stat="identity")

#filtering data
dcPop9010 <- filter(dcPop, year != 2000)

#creating a line graph
ggplot(dcPop9010, aes(x=year, y=totalPop, group=ward, colour=ward)) + 
  geom_line()

#printing out your variable names
names(dcPop9010)

#creating a new variable
dcPop9010<-mutate(dcPop9010,btwn1865=100-perUnder18-perOver65)

#subsetting a data frame to only the columns you want and checking
dcWrkAge<-select(dcPop9010, ward, btwn1865, year)
names(dcWrkAge)

#reordering
arranged <- arrange(dcWrkAge,btwn1865)
#to get descending order: arrange(dcWrkAge,desc(btwn1865))
head(arranged)
tail(arranged)

#creating a multi bar plot
ggplot(dcPop9010, aes(x=ward, y=btwn1865, fill=year)) + 
  geom_bar(stat="identity",position=position_dodge())
