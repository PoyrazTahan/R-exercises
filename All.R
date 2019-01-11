# Defines  a vector 
WeekdaySales <- c(30,42,34,23,45)
Weekdays <- c("M","Tu","W","Th","F")
WeekendSales <- c(12,15)
Weekend <- c("Sa","Su")
# Merging two vectors
Sales <- c(WeekdaySales,WeekendSales)
Days <- c(Weekdays,Weekend)
# Basic operations / get info about vector
summary(Sales)
summary(Days)
# Calculation on a vector
SalesInTl <- (Sales*5)
# Vector size?
length(Days)
# a vector 1 to 7
DayNum <- 1:7
# Array Operations
Sales[3]
Sales > 30 # returns true if value of i is greater than 30
Days[Sales>30] # gets the vlues that are true
# get info
mean(SalesInTl)
max(SalesInTl)
sd(SalesInTl) # standart deviation
# deletes one variable in the vector
Sales[3] <- NA
SalesInTl <- Sales*5
mean(SalesInTl) # notice error
max(SalesInTl)  # notice error
sd(SalesInTl)   # notice error
# Creates a Dataframe
Sales.df <- data.frame(DayNum,Days,Sales,SalesInTl,stringsAsFactors = FALSE)
# Dataframe Operations
Sales.df[4,2]
Sales.df[,2] # column
Sales.df[4,] # row
summary(Sales.df)
##
write.csv(Sales.df,row.names = FALSE) # creates a csv file - but you need to assign
# Defining a function
t <- function(x,y){
  (sqrt(x^2+y^2))
}
t(3,4)

####### ####### #######  Part 2 ####### ####### ####### 
## required: Upload the data set of Daily from csv ##

summary(daily)
summary(daily$hum)
summary(daily$hum,digits=2)
# Table Operations
table(daily$season) # counts occurance pf each season
options(digits=2)
prop.table(table(daily$season)) # gets their probability table
table(daily$season,daily$yr)
# get info 
mean(daily$cnt)
median(daily$cnt)
sd(daily$cnt)
quantile(daily$cnt, probs = c(0.05,0.95))
# Visiulizing - Histogram (Follow incremental changes) 
hist(daily$cnt)
hist(daily$cnt,breaks=30)
hist(daily$cnt,breaks=30,freq = FALSE)
lines(density(daily$cnt,bw= 1000, type="1", col="darkblue",lwd=1)) # bw controls the smoothnes of the line
# Visiulizing - Boxplot (Follow incremental changes) 
boxplot(daily$hum)
boxplot(daily$hum~daily$season)
# Visiulizing - Histogram
qplot(x=hum, data=daily) 
qplot(x=hum, data=daily) + facet_wrap(~season)

# Mean of the humidty depending on the season?
options(digits=4)
by(daily$hum,daily$season, mean) # returns answers individully
aggregate(daily$hum,by=list(daily=daily$season), mean) # returns table

####### ####### #######  Part 3 - CLUSTERING ####### ####### ####### 
## required: Load data with the name cldata ##
install.packages("NbClust")
library(NbClust)
library(cluster)

summary(cldata)

# Scale data
testdata <- cldata
testdata <- scale(testdata)

# Determine number of clusters. Option 1: visual rule
wss <- (nrow(testdata)-1)*sum(apply(testdata,2,var)) # creates a error variable
for (i in 2:15) wss[i] <- sum(kmeans(testdata,centers=i)$withinss)  # records the error depending on the cluster points 1 to 15
# creates the basic plot of the vector
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Determine number of clusters. Option 2: more frequent optimal number
res <- NbClust(cldata, diss=NULL, distance = "euclidean",min.nc=2, max.nc=12,method = "kmeans", index = "all") # try clustirng 2to 15
res$Best.partition # partition gives the class of the node in the best case in terms of class number vs error

# K-Means Cluster Analysis (based on the proposed number by NbCluster)
options(digits = 2)
fit <- kmeans(testdata, 3) # applies k-means algo right away
table(fit$cluster)
# Calculate average for each cluster
aggregate(cldata,by=list(fit$cluster),FUN=mean)
# Add segmentation to dataset
cldata.w.cluster <- data.frame(cldata, fit$cluster) # creates a dataframe from "cldata with cluster"

#EXTRA STEP
clusplot(cldata, fit$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main="K-means cluster plot")

#Hierarchical Clustering
cldata.dist <- dist(cldata)
cldata.hc <- hclust(cldata.dist, method="complete")

plot(cldata.hc)
rect.hclust(cldata.hc, k=4, border="red")

cldata.hc.segment <- cutree(cldata.hc, k=4)     # membership vector for 4 groups
table(cldata.hc.segment)



####### ####### #######  Part 4 - DECISION TREES ####### ####### ####### 
## required: Load data with the name W6 - Traincar as W6_Car !! CLICK HEADING YES !! ## 
## required: Load data with the name W6 - Testcar as test !! CLICK HEADING YES !! ## 
require(lattice)
require(ggplot2)

# Change the data by factoring
W6_Car$X1 <- factor(W6_Car$X1)
W6_Car$X2 <- factor(W6_Car$X2)
W6_Car$X5 <- factor(W6_Car$X5)
W6_Car$X6 <- factor(W6_Car$X6)
W6_Car$X7 <- factor(W6_Car$X7)

cols <- character(nrow(W6_Car)) # creates a vec int the length of the W6 data tha contains string
cols[] <- "black" # writes black to all
cols[W6_Car$X7 == "acc"] <- "blue" # changes the color depending on their X7 column 
cols[W6_Car$X7 == "good"] <- "red"
cols[W6_Car$X7 == "unacc"] <- "green"
# Plots the interacting od the columns, represented in colors
pairs(W6_Car[1:6],col=cols) 
plot(jitter(W6_Car$X3) ~ jitter(W6_Car$X4), pch = 15,col=cols)
plot(jitter(as.integer(W6_Car$X1)) ~ jitter(as.integer(W6_Car$X2)), pch = 15,col=alpha(cols, 0.5)) # gives error

library(rpart)
library(rpart.plot)
library(caret)

options(digits=2)
train <- W6_Car
fit1 <- rpart(X7 ~ . , data=train, method="class",control=rpart.control(minsplit=1))
rpart.plot(fit1) # prints the decision tree

# Now let's make predictions
predicttrain1 <- predict(fit1, train, type = "class")
confusionMatrix(predicttrain1,train$X7)
prediction1 <- predict(fit1, test, type = "class")
confusionMatrix(prediction1,test$X7)

#Repeat with smaller training set
set.seed(1)
train_s <- train[sample(1:nrow(train), 200, replace=FALSE),]

fits <- rpart(X7 ~ . , data=train_s, method="class",
              control=rpart.control(minsplit=1))
rpart.plot(fits)

predicttrains <- predict(fits, train_s, type = "class")
confusionMatrix(predicttrains,train_s$X7)
predictions <- predict(fits, test, type = "class")
confusionMatrix(predictions,test$X7)

####### ####### #######  Part 5 - LOGISTIC REGRESSION ####### ####### #######
## required: Load data with the name 6-Regression Data.csv as admission ## 
b0 <- -1.5514
b1 <- 0.19031

prob_x <- function(x) { 
  exp(b0+b1*x) / (1 + exp(b0+b1*x))
}

odds_x <- function(x) {
  x / (1-x)
}

prob_x(0) 
prob_x(1)

odds_x(prob_x(0))
odds_x(prob_x(1))

log(odds_x(prob_x(1)) / odds_x(prob_x(0)))

prob_x(29)
prob_x(30)

odds_x(prob_x(29))
odds_x(prob_x(30))

log(odds_x(prob_x(30)) / odds_x(prob_x(29)))

summary(admission)
sapply(admission, sd) # returns the given argument in second parameter depending on the column in a table

with(admission, table(admit, rank)) # pivot table counts 

admission$rank <- factor(admission$rank)

ggplot(admission, aes(x = gpa, y = admit)) + geom_point(alpha=.25)

# creates a logistic regresission model !! since it is glm function and I get the output binomial
mylogit0 <- glm(admit ~ gpa, data = admission, family = "binomial")
summary(mylogit0)
anova(mylogit0, test="Chisq")

mylogit <- glm(admit ~ gre + gpa + rank, data = admission, family = "binomial")
summary(mylogit)
confint(mylogit)
anova(mylogit, test="Chisq")

newdata1 <- with(admission, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(admission, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                                 4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata2$rankP <- predict(mylogit, newdata = newdata2, type = "response")
ggplot(newdata2, aes(x = gre, y = rankP)) + geom_line(aes(colour = rank), size = 1)

newdata3 <- with(admission, data.frame(gre = mean(gre), gpa=rep(seq(from = 0, to = 4, length.out = 100),
                                                                4), rank = factor(rep(1:4, each = 100))))

newdata3$rankP <- predict(mylogit, newdata = newdata3, type = "response")

ggplot(newdata3, aes(x = gpa, y = rankP)) + geom_line(aes(colour = rank), size = 1)

#RECREATING LOGISTIC REGRESSION PARAMETERS
trialdata <- with(admission, data.frame(gre = 500, gpa = 3, rank = factor(1)))
p1 <-  predict(mylogit, newdata = trialdata, type = "response")
trialdata <- with(admission, data.frame(gre = 501, gpa = 3, rank = factor(1)))
p2 <- predict(mylogit, newdata = trialdata, type = "response")
log(odds_x(p2) / odds_x(p1))   
# Compute AUC for predicting Class with the model
library(ROCR)
prob <- predict(mylogit, newdata=admission, type="response")
pred <- prediction(prob, admission$admit)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)


####### ####### #######  Part 6 - CLUSTERING / USCITIES ####### ####### #######
## required: Load data with the name US.cities as US_Cities ## 

library(NbClust)

# Scale data
# testdata <- US_Cities
US_Cities <- subset(US_Cities,City!="Honolulu")
testdata <- scale(US_Cities[2:6])

# Determine number of clusters. Option 1: visual rule
# set.seed(42)
wss <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(testdata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Determine number of clusters. Option 2: more frequent optimal number
res <- NbClust(testdata, diss=NULL, distance = "euclidean",
               min.nc=2, max.nc=12,
               method = "kmeans", index = "all")
res$Best.partition

# K-Means Cluster Analysis (based on the proposed number by NbCluster)
options(digits = 2)
fit <- kmeans(testdata, 10)
table(fit$cluster)
# Calculate average for each cluster
aggregate(US_Cities[2:6],by=list(fit$cluster),FUN=mean)
# Add segmentation to dataset
US.w.cluster <- data.frame(US_Cities, fit$cluster)

library(cluster)
#EXTRA STEP
clusplot(testdata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")

#Hierarchical Clustering
cldata.dist <- dist(testdata)
cldata.hc <- hclust(cldata.dist, method="complete")

plot(cldata.hc)
rect.hclust(cldata.hc, k=3, border="red")

cldata.hc.segment <- cutree(cldata.hc, k=3)     # membership vector for 4 groups
table(cldata.hc.segment)

####### ####### #######  Part 7 - RFM  ####### ####### #######
## required: Load data with the name RFM Example History.xlsx from excel ## 

hist <- RFM_Example_History
# open a variable to aggreate upon
cust <- NA
cust <- aggregate(hist$Date, by=list(hist$Customer), max) # works here as filter / takes the recent date
cust$RecentDate <- as.Date(cust$x,format="%Y-%m-%d") # change format
cust <- cust[-c(2)] # remove unnecessary column
names(cust)[1]<-"CustID" # names the column
# RECENCY
temp <- aggregate(hist$Date, by=list(hist$Customer), min) # gets the first date he cames
cust$FirstDate <- as.Date(temp$x,format="%Y-%m-%d")
present <- as.Date("2014-01-01",format="%Y-%m-%d") # gets how many days has it been since the first arrival
cust$tenure <- (as.numeric(present)-as.numeric(cust$FirstDate))/365 # how many years has it been ince the first arrival
# MONETARY
temp <- aggregate(hist$Amount, by=list(hist$Customer), sum) # Gets the total number of spending for each customer
cust$Monetary <- temp$x / cust$tenure # Calculates how much the customer spends per year
# FREQUENCY
cust$Freq <- temp$x / cust$tenure
# Orders the Recency, Monetary and Frequecy columns
cust$orderR[order(cust$RecentDate)] <- 1:nrow(cust)
cust$orderM[order(cust$Monetary)] <- 1:nrow(cust)
cust$orderF[order(cust$Freq)] <- 1:nrow(cust)
# scales it from 1 to 5
cust$R <- floor((cust$orderR-1)/1000)+1
cust$M <- floor((cust$orderM-1)/1000)+1
cust$F <- floor((cust$orderF-1)/1000)+1

resp <- RFM_Example_Response

cust2 <- NA


cust2 <- aggregate(resp$Response, by=list(cust$R, cust$M,cust$F), sum)
names(cust2)[1]<-"R"
names(cust2)[2]<-"M"
names(cust2)[3]<-"F"
names(cust2)[4]<-"Response"
temp <- aggregate(resp$Customer, by=list(cust$R, cust$M,cust$F), length)
cust2$contacted <- temp$x
cust2$ResponseRate <- with(cust2,Response/contacted)

####### ####### #######  Part 7 - ASSOCIETIVE RULE  ####### ####### #######
## Don't forget the change path ##


library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
retail <- Online_Retail
retail <- read_excel('Online Retail.xlsx') # You need to change the ath from console
retail <- retail[complete.cases(retail), ] # removes incomplete rows
retail$Description <-  as.factor(retail$Description)
retail$Country <- as.factor(retail$Country)
retail$Date <- as.Date(retail$InvoiceDate)
retail$InvoiceNo <- as.numeric(retail$InvoiceNo)
glimpse(retail)

#HOURLY SHOPPING TRAFFIC
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)
ggplot(aes(x = Time),data = retail) + 
  geom_histogram(stat="count")

#Examine number of unique items per transaction
detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = n()) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  coord_cartesian(xlim=c(0,80))

#Find out top selling items
tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#GET READY FOR APRIORI ANALYSIS
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=10, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")

####### ####### #######  Part 8 - Churn  ####### ####### #######
## Don't forget the change path ##

library(corrplot)
library(ggplot2)
library(gridExtra)

sapply(churn, function(x) sum(is.na(x)))

churn <- churn[complete.cases(churn), ]

churn$OnlineSecurity[churn$OnlineSecurity == "No internet service"] <- "No"
churn$OnlineBackup[churn$OnlineBackup == "No internet service"] <- "No"
churn$DeviceProtection[churn$DeviceProtection == "No internet service"] <- "No"
churn$TechSupport[churn$TechSupport == "No internet service"] <- "No"
churn$StreamingTV[churn$StreamingTV == "No internet service"] <- "No"
churn$StreamingMovies[churn$StreamingMovies == "No internet service"] <- "No"
churn$MultipleLines[churn$MultipleLines == "No phone service"] <- "No"

min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)
churn$customerID <- NULL
churn$tenure <- NULL

numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

churn$TotalCharges <- NULL

p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)


set.seed(1)
churn$ID <- seq.int(nrow(churn))
training <- churn[sample(1:nrow(churn), 5600, replace=FALSE),]
testing <- churn[!(churn$ID %in% training$ID), ]

LogModel <- glm(Churn ~ .,family=binomial,data=training)
summary(LogModel)

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)
