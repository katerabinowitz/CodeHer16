require(ggplot2)
require(dplyr)
require(reshape2)
dcPop<-read.csv("https://raw.githubusercontent.com/katerabinowitz/CodeHer16/master/data/source%20data/ninfoDCPop.csv")
dcWB<-read.csv("https://raw.githubusercontent.com/katerabinowitz/CodeHer16/master/data/source%20data/ninfoDCWB.csv")
dcHouse<-read.csv("https://raw.githubusercontent.com/katerabinowitz/CodeHer16/master/data/source%20data/ninfoDCHousing.csv")
dcSchools<-read.csv("https://raw.githubusercontent.com/katerabinowitz/CodeHer16/master/data/source%20data/ninfoDCSchools.csv")

colnames(dcPop)
dcPop<-dcPop[c(1,3:5,10:12,17:19,23:34)]
colnames(dcPop)
dcPop<-filter(dcPop, WARD2012!="")
dcPop90<-select(dcPop, WARD2012,contains("1990"))
dcPop90$year<-rep(1990,8)
dcPop00<-select(dcPop,WARD2012,contains("2000"))
dcPop00$year<-rep(2000,8)
dcPop10<-select(dcPop,WARD2012,contains("2010"))
dcPop10$year<-rep(2010,8)

listDF <- list(dcPop90, dcPop00, dcPop10)
new_col_name <- c("ward","totalPop", "perUnder18","perOver65","perBlack","perWhite","perHisp","perAsianPI","year")
newDF<-lapply(listDF, setNames, nm = new_col_name)
dcPop<-bind_rows(newDF)
dcPop<-mutate(dcPop,ward=gsub("Ward ","",ward))
dcPop$ward<-as.numeric(dcPop$ward)
rm(dcPop90,dcPop00,dcPop10)

colnames(dcWB)
dcWB<-dcWB[c(1,3:23,28:31)]
colnames(dcWB)
dcWB<-filter(dcWB, WARD2012!="")
dc90<-select(dcWB, WARD2012,contains("1990"))
dc90$year<-rep(1990,8)
dc00<-select(dcWB,WARD2012,contains("2000"))
dc00$year<-rep(2000,8)
dc10<-select(dcWB,WARD2012,contains("2010"))
dc10$year<-rep(2010,8)
colnames(dc10)

listDF <- list(dc90, dc00, dc10)
new_col_name <- c("ward","perPoor", "perPoorChild","perPoorElder","perUnemp","perOveremp","perNoHS","AvgFamIncome","year")
newDF<-lapply(listDF, setNames, nm = new_col_name)
dcWB<-bind_rows(newDF)
rm(dc90,dc00,dc10)

colnames(dcHouse)
dcHouseSale<-dcHouse[c(1,16:36)]
dcHousePrice<-dcHouse[c(1,37:57)]

dchs<-melt(dcHouseSale, id.vars=c("WARD2012"))
dchs$year<-c(rep(1995,9),rep(1996,9),rep(1997,9),rep(1998,9),rep(1999,9),rep(2000,9),rep(2001,9),
            rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
            rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9), rep(2014,9),rep(2015,9))
dcp<-melt(dcHousePrice, id.vars=c("WARD2012"))
dcp$year<-c(rep(1995,9),rep(1996,9),rep(1997,9),rep(1998,9),rep(1999,9),rep(2000,9),rep(2001,9),
             rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
             rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9), rep(2014,9),rep(2015,9))
dcHouse<-merge(dchs,dcp,by=c("WARD2012","year"))
dcHouse<-filter(dcHouse, WARD2012!="")
dcHouse<-select(dcHouse,WARD2012,year,contains("value"))
colnames(dcHouse)<-c("ward","year","houseSales","housePrice")
rm(dcp,dchs,dcHouseSale,dcHousePrice)

colnames(dcSchools)
schoolN<-dcSchools[c(1,3:15)]
m1<-melt(schoolN, id.vars=c("WARD2012"))
m1$year<-c(rep(2001,9),rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
            rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9))

dcpsN<-dcSchools[c(1,17:29)]
m2<-melt(dcpsN, id.vars=c("WARD2012"))
m2$year<-c(rep(2001,9),rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
           rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9))

charterN<-dcSchools[c(1,31:43)]
m3<-melt(charterN, id.vars=c("WARD2012"))
m3$year<-c(rep(2001,9),rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
           rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9))

studentN<-dcSchools[c(1,44:56)]
m4<-melt(studentN, id.vars=c("WARD2012"))
m4$year<-c(rep(2001,9),rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
           rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9))

dcpsSN<-dcSchools[c(1,57:69)]
m5<-melt(dcpsSN, id.vars=c("WARD2012"))
m5$year<-c(rep(2001,9),rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
           rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9))

cSN<-dcSchools[c(1,70:82)]
m6<-melt(cSN, id.vars=c("WARD2012"))
m6$year<-c(rep(2001,9),rep(2002,9),rep(2003,9),rep(2004,9),rep(2005,9),rep(2006,9),rep(2007,9),rep(2008,9),
           rep(2009,9),rep(2010,9),rep(2011,9),rep(2012,9),rep(2013,9))

dcSchools<-Reduce(function(x, y) merge(x, y, by=c("WARD2012","year")), list(m1,m2,m3,m4,m5,m6))
colnames(dcSchools)<-c("ward","year","drop","schools","drop1","dcpsSchools","drop2","charterSchools","drop3",
                       "students","drop4","dcpsStudents","drop5","charterStudents")

dcSchools<-filter(dcSchools, ward!="")
dcSchools<-select(dcSchools,-contains("drop"))
rm(m1,m2,m3,m4,m5,m6,charterN,cSN,dcpsN,dcpsSN,schoolN,studentN)

write.csv(dcHouse,"/Users/katerabinowitz/Documents/Talks/CodeHer16/data/dcHomeSales.csv", row.names=FALSE)
write.csv(dcPop,"/Users/katerabinowitz/Documents/Talks/CodeHer16/data/dcPopulation.csv", row.names=FALSE)
write.csv(dcSchools,"/Users/katerabinowitz/Documents/Talks/CodeHer16/data/dcSchools.csv", row.names=FALSE)
write.csv(dcWB,"/Users/katerabinowitz/Documents/Talks/CodeHer16/data/dcIncome.csv", row.names=FALSE)
