library(WDI)
library(plyr)
library(lubridate)
library(ggplot2)

#Import ransomware tracker dataset, clean and filter it to only 2016 records
ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)
ransomware <- ransomware[!(is.na(ransomware$Country) | ransomware$Country==""), ]
ransomware2016 <- ransomware[year(ransomware$X..Firstseen..UTC.) == 2016,]

#finding total hosts per country
ransomware2016$Country <- substring(ransomware2016$Country,0,2)
total2016 <- count(ransomware2016, "Country")
colnames(total2016) <- c("Ccode","No_of_hosts")

#finding ol rate
#ransomOl2016 <- ransomware2016[ransomware2016$Status == "online", ]
#totalOl2016 <- count(ransomOl2016, "Country")
#colnames(totalOl2016) <- c("Ccode","No_of_ol_hosts")
#totalOl2016 <- merge(total2016,totalOl2016, by="Ccode")
#totalOl2016$Olrate <- totalOl2016$No_of_ol_hosts/totalOl2016$No_of_hosts

#finding off rate
ransomOff2016 <- ransomware2016[ransomware2016$Status == "offline", ]
totalOff2016 <- count(ransomOff2016, "Country")
colnames(totalOff2016) <- c("Ccode","No_of_off_hosts")
totalOff2016 <- merge(total2016,totalOff2016, by="Ccode")
totalOff2016$Offrate <- totalOff2016$No_of_off_hosts/totalOff2016$No_of_hosts

#import other dataset (GCI & WDI)
gci2017 <- read.csv("gci-2017.csv",header = TRUE,sep = ",",skip = 2)
wdi <- WDI(indicator=c('IT.NET.USER.ZS', 'SP.POP.TOTL', 'IT.NET.SECR'), start=2016, end=2016)

#merge Ransomware tracker dataset and GCI
totalOffXgci <- merge(totalOff2016, gci2017, by="Ccode")
totalOffXgci_woUS <- totalOffXgci[totalOffXgci$Ccode != "US",] #US remove since its datapoint is an outlier

#Hypothesis 1: correlation between gci_index and number of host in country
ggplot(totalOffXgci, aes(x=GCI_Index, y=No_of_hosts)) + geom_point() + geom_smooth(method='lm',formula=y ~ x)
ggplot(totalOffXgci_woUS, aes(x=GCI_Index, y=No_of_hosts)) + geom_point() + geom_smooth(method='lm',formula=y ~ x) #if US is removed
lmH1 <- lm(No_of_hosts ~ GCI_Index,totalOffXgci_woUS)

#Hypothesis 2: correlation between gci_index and ratio of offline host in country

ggplot(totalOffXgci, aes(x=GCI_Index, y=No_of_off_hosts)) + geom_point() + geom_smooth(method='lm',formula=y ~ x)
ggplot(totalOffXgci_woUS, aes(x=GCI_Index, y=No_of_off_hosts)) + geom_point() + geom_smooth(method='lm',formula=y ~ x) #if US is removed
ggplot(totalOffXgci, aes(x=GCI_Index, y=Offrate)) + geom_point() + geom_smooth(method='lm',formula=y ~ x)
lmH2 <- lm(Offrate ~ GCI_Index,totalOffXgci)

#others
wdi2016 <- rename(wdi, c("iso2c"="Ccode"))
totalXgciXwdi <- merge(totalOffXgci, wdi2016, by="Ccode")
totalXgciXwdi$InetUsers <- (as.numeric(totalXgciXwdi$SP.POP.TOTL) * as.numeric(totalXgciXwdi$IT.NET.USER.ZS) / 100)
totalXgciXwdi_NoCNINUS <- totalXgciXwdi[totalXgciXwdi$Ccode != "US" & totalXgciXwdi$Ccode != "CN" & totalXgciXwdi$Ccode != "IN",]
ggplot(totalXgciXwdi_NoCNINUS, aes(x=InetUsers, y=No_of_hosts)) + geom_point() + geom_smooth(method='lm',formula=y ~ x)
lmX <- lm(No_of_hosts ~ InetUsers, totalXgciXwdi_NoCNINUS)
#============ARCHIVE================
# #Hypothesis 2: Correlation between gci_index and ratio between hosts and internet users
# wdi2016 <- rename(wdi, c("iso2c"="Ccode"))
# totalXgciXwdi <- merge(totalXgci, wdi2016, by="Ccode")
# 
# totalXgciXwdi$InetUsers <- (as.numeric(totalXgciXwdi$SP.POP.TOTL) * as.numeric(totalXgciXwdi$IT.NET.USER.ZS) / 100)
# totalXgciXwdi$HostPerInetUsersRatio <- as.numeric(totalXgciXwdi$No_of_hosts/totalXgciXwdi$InetUsers)
# ggplot(totalXgciXwdi, aes(x=GCI_Index, y=HostPerInetUsersRatio)) + geom_point() + geom_smooth(method='lm',formula=y ~ x + I(x^2))
# 
# lmH2 <- lm(HostPerInetUsersRatio ~ GCI_Index,totalXgciXwdi)
# 
# #Hypothesis 3: correlation between gci_index and ratio of online host in country
# totalOlXgci <- merge(totalOl2016, gci2017, by="Ccode")
# ggplot(totalOlXgci, aes(x=GCI_Index, y=No_of_ol_hosts)) + geom_point() + geom_smooth(method='lm',formula=y ~ x + I(x^2))
# ggplot(totalOlXgci, aes(x=GCI_Index, y=Olrate)) + geom_point() + geom_smooth(method='lm',formula=y ~ x)
# 
# totalOlXgci_woUS <- totalOlXgci[totalOlXgci$Ccode != "US",] #US remove since its datapoint is an outlier
# ggplot(totalOlXgci_woUS, aes(x=GCI_Index, y=No_of_ol_hosts)) + geom_point() + geom_smooth(method='lm',formula=y ~ x + I(x^2))
# 
# lmH3 <- lm(No_of_ol_hosts ~ GCI_Index,totalOlXgci)
