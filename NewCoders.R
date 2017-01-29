library(ggplot2)
library(plyr)
library(dplyr)
library(formattable)
library(RColorBrewer)
library(viridis)
library(ggthemes)

data <- read.csv("~/2016NCS.csv", header = TRUE, sep=",")

fix(data)

summary(data)

names(data)

str(data)

# sapply function to count missing values
sapply(data, function(x) sum(is.na(x))) 

# Barplot to show distribution of Gender
countsgend <- table(data$Gender)
par(mfrow=c(1,1), las=1, mar=c(5,9,4,2))
barplot(countsgend,col= brewer.pal(3,"Set2"), cex.names=0.6, main="Genders", xlab="freq")

# Recoding Gender to group non-binary options
attach(data)
data$Gendercat[Gender=='male'] <- 'Male'
data$Gendercat[Gender=='female'] <- 'Female'
data$Gendercat[Gender=='agender' | Gender=='trans' | Gender=='genderqueer'] <- 'Non-binary'
detach(data)
data$Gendercat <- factor(data$Gendercat)
summary(data$Gendercat)

# Selected numeric variables are grouped to allow for analysis
numvars <- c('MonthsProgramming', 'MoneyForLearning', 'Income', 'HoursLearning', 'ExpectedEarning', 'Age')
numdata <- data[numvars]
summary(numdata)

# Creating a stacked barplot, adapted from code by Kaggle user @HanY
data%>%select(Age, Gendercat)%>%
  group_by(Age, Gendercat)%>%
  summarize(count=n())%>%
  ggplot(aes(Age,count,fill=Gendercat))+
  geom_bar(stat='identity',position='stack',color='white')+xlim(c(10,70))+theme_light()+
  scale_fill_viridis(discrete=T)

# Using plyr to get a list of countries with counts
cl <- plyr::count(data, 'CountryLive')
cl
plyr::arrange(cl, desc(freq))

countsmarstat <- table(data$MaritalStatus)
par(mfrow=c(1,2), las=2, mar=c(5,8,4,2))
barplot(countsmarstat,col= brewer.pal(3,"Set2"), horiz=TRUE, cex.names=0.6, main="Marital Status", xlab="freq")
hist(data$ChildrenNumber,breaks=20, col=brewer.pal(3,"Set2"),main="Number of Children", xlab="No. of Children", xlim=c(0,10))

median(data$ChildrenNumber, na.rm=TRUE)

countsempstat <- table(data$EmploymentStatus)
countsempfield <- table(data$EmploymentField)
par(mfrow=c(1,2), las=2, mar=c(5,10,4,2))
barplot(countsempstat,col= brewer.pal(3,"Set3"), horiz=TRUE, cex.names=0.6, main="Employment Status", xlab="freq")
barplot(countsempfield,col= brewer.pal(3,"Set3"), horiz=TRUE, cex.names=0.6, main="Employment Field", xlab="freq")

par(mfrow=c(1,2), las=1, mar=c(5,4,4,2))
hist(data$HoursLearning,breaks=10, col=brewer.pal(3,"Set1"),main="Hours spent learning per week", xlab="No. of Hours")
hist(data$MoneyForLearning, breaks=160,col=brewer.pal(3,"Set1"),main="Amount spent learning to code", 
     xlab="Amount in $", xlim=c(0,20000))

# Visual representations of online, podcast and event resources, adapted from Kaggle user @HanY
online <- data%>%select(matches('Resource'))%>%
  select(-ResourceOther)
data.frame(name=substring(names(online),9),
           count=colSums(online,na.rm=TRUE),row.names=NULL)%>%
  arrange(desc(count))%>%
  formattable(list(count = color_bar("lightgreen")),
              align = 'l')

codeevent=data%>%select(matches('CodeEvent'))%>%
  select(-CodeEventOther)
data.frame(name=substring(names(codeevent),10), #extract names
           count=colSums(codeevent,na.rm=TRUE),row.names=NULL)%>%
  arrange(desc(count))%>%
  formattable(list(count = color_bar("lightblue")),
              align = 'l')

podcast=data%>%select(matches('Podcast'))%>%
  select(-PodcastOther)
data.frame(name=substring(names(podcast),8), #extract names
           count=colSums(podcast,na.rm=TRUE),row.names=NULL)%>%
  arrange(desc(count))%>%
  formattable(list(count = color_bar("pink")),
              align = 'l')

par(mfrow=c(1,1),mar=c(5,10,4,2),cex.lab=0.6, cex.axis=0.6)
boxplot(data$HoursLearning~data$EmploymentStatus,col=topo.colors(3), horizontal=TRUE,
        main="Hours worked by employment type", xlab="Hours per Month")

par(mfrow=c(1,2),mar=c(5,4,4,2),cex.lab=1, cex.axis=1)
hist(data$Income,breaks=10, col=brewer.pal(3,"Set3"),main="Current Income", xlab="Income in $")
hist(data$ExpectedEarning,breaks=10, col=brewer.pal(3,"Set3"),main="Expected Income", xlab="Expected in $")

summary(data$Income)
summary(data$ExpectedEarning)

countsrole <- table(data$JobRoleInterest)
par(mfrow=c(1,1), las=2, mar=c(5,9,4,2))
barplot(countsrole,col= brewer.pal(3,"Set2"), horiz=TRUE, cex.names=0.6, main="Desired job role", xlab="freq")

# Comparing numeric variable for potential correlation
pairs(numdata)
cor(numdata, use="complete.obs", method="pearson")
