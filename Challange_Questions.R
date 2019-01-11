#WHEN ANSWERING EXAM QUESTIONS
#USE R CODE AS MUCH AS POSSIBLE TO GET FULL CREDIT


#PROBLEM 1 
#1A DECISION TREES (10) You would like to figure out how high school graduates decide which field to choose for
#their bachelor's degree (engineering, law, medicine etc.). What kind of data would you collect to model this as a decision tree?
##########
#Ans1
# Theis course score in dypes of classes like math, sicence or history
# What path did they choose in highschool whether mf, tm, ts, dil 
# What gender 
# Economic Income of their familiy
# Whre do they live (city)
# What are their parent's occupation
##########
#1B LOGISTIC REGRESSION (10) How would you use a logistic regression model to examine your probability of passing this class? 
#Define your dependent and independent variables.
##########
# ANS 1b
# dependent: wheter will I be able to pass or not (the prediction)
# independent: Quiz score / If you find good questions / scale of interest 0 - 10 / How much time spent on HW / How many HW completed


##########
#PROBLEM 2
#Read the data set P2.csv

#A What would be the best set of initial centroids for k-means with k=3? Why? (10)
##########
library(NbClust)

testdata <- P2
testdata
testdata <- scale(testdata)
testdata

wss <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(testdata,
                                     centers=i)$withinss)
plot(2:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


res <- NbClust(P2, diss=NULL, distance = "euclidean",
               min.nc=2, max.nc=12,
               method = "kmeans", index = "all")
res$Best.partition # I have gotten the near best solution with this way 
# if I start near this that would be good becuase it is the best and it will converge quickly
# but still I would have gotten random sets just in case since te thing I find is a local minima 
# If I am looking for the global min I would put anywhere but these points. Since if this is the global max what ever
# it will converge will have greate min or it will converge to these points

fit <- kmeans(testdata, 3)

P2.w.cluster <- data.frame(P2, fit$cluster)
P2.w.cluster[,3]

Xmean.df <- aggregate(P2.w.cluster$X,
          by=list(P2.w.cluster$fit.cluster), mean)
Ymean.df <- aggregate(P2.w.cluster$Y,
                      by=list(P2.w.cluster$fit.cluster), mean)
setofInitials1 <- c(Xmean.df[1,2],Ymean.df[1,2])
setofInitials2 <- c(Xmean.df[2,2],Ymean.df[2,2])
setofInitials3 <- c(Xmean.df[3,2],Ymean.df[3,2])
setofInitials1
setofInitials2
setofInitials3


######################

#B Create a graph showing 'within groups sum of squares' for k-means clustering for k=1 to 10. (5)

########
mywss <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 2:10) mywss[i] <- sum(kmeans(testdata,
                                     centers=i)$withinss)
plot(1:10, mywss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#########

#C If you employed hierarchical clustering, what would be the first cluster to be created? 
#Why? (HINT: You may need to use the dist and/or the plot functions) (10)


##########
summary(P2)

library(cluster)
p2.dist <- dist(P2) # ??
p2.hc <- hclust(p2.dist, method="complete") #hieraricical clustirng

plot(p2.hc)
rect.hclust(p2.hc, k=2, border="red") # these two clusters are first to be created
# these are the seected clusters at first stage according the algorithm

p2.hc.segment <- cutree(p2.hc, k=2)     # membership vector for 3 groups
table(p2.hc.segment)

###########


#PROBLEM 3
#You are the manager of a supermarket. You have created a marketing campaign at the beginning of January 2018 
#to attract more customers and offered a random discount amount to people who shopped in your store in December 2017 .
#At the end of January, you collected data on which December shoppers came back to the store in a file titled P3.csv
#and now would like to figure the factors that influenced people's January visit.

#3A If we can pick a random person, what is the probability of getting a male customer? (5)
#HINT: First delete data points where Gender is not Male or Female using the subset function

########
prob<- sum(P3$Gender == "Male")/(sum(P3$Gender == "Female")+sum(P3$Gender == "Male"))
prob
## I don't need to subset the function with this method
#########

#3B Based on part A, what are the odds of getting a male customer? (2.5)

odds_x <- function(x) {
  x / (1-x)
}
oddGenderM <-odds_x(prob)
oddGenderM

#3C Create logistic regression model to examine the probability of a customer coming back in January 18. 
#HINT: Your first model need not be the best model to use. (10)
summary(P3)



mylogit0 <- glm(JanuaryShop ~ DecSpend, data = P3, family = "binomial")


#3D Create and interpret regression output and ANOVA output for the final model from Step C (5)



summary(mylogit0)
anova(mylogit0, test="Chisq")



#3E What is the probability of a 35 year old male customer coming back to the your store in January
#if he spent $200 in December 2017 and was offered a discount of 5% or
#if she spent $2000 in December 2017 and was offered a discount of 5%
#(5)


newdata1 <- with(P3, data.frame(Age = 35, DecSpend = 200))

newdata1$rankP <- predict(mylogit0, newdata = newdata1, type = "response")
newdata1


newdata2 <- with(P3, data.frame(Age = 35, DecSpend = 2000))

newdata2$rankP <- predict(mylogit0, newdata = newdata2, type = "response")
newdata2



#3F How could you explain the difference you found in part E? (2.5)

## It can be explained by the the choosen attributes effect on the model. 
# If there is difference that means the attribute at hand as a signaficant important
# and this attribute could be use a determimng factor
# If there is no difference between answers that means this attribute does not really effect the model
# the first is 80% the second is 46%, this atribute has a negative correlation this might be because
# a person who spend a lot already would not care 5% discount anyway so giving that to him/her is meaningless on the other hand
# giving a person who does not usually comes can make a big change in his/her decision to chose your supermarket



#PROBLEM 4
#Read the data set P4.csv
#HINT: It may help to find how to execute Excel's COUNTIF function in R

#4A Find the GINI Index for the Class column (7.5)

# there are 2 types in Class // C1 C2

p3Class <- P4[,1]
p3Class
C1cnt <-sum(P4$Class == "C1")
C2cnt <-sum(P4$Class == "C2")
sum <- C1cnt + C2cnt
Gini <- 1- (C1cnt/(C1cnt+C2cnt))^2 - (C2cnt/(C1cnt+C2cnt))^2
Gini

#4B Find the Entropy for the Class column (7.5)
Entropy <- - (C1cnt/sum)*log2((C1cnt/sum)) - (C2cnt/sum)*log2((C2cnt/sum))
Entropy

#4C If we split the data into two at age 30 - one branch for less than or equal to 30 and one branch for more than 30 
#find the GINI Index of the split (10)

less30 <-sum(P4$Age <= 30)
less30
more30 <-sum(P4$Age > 30)
more30
sum <- less30+more30



P4.less30 <-subset(P4, Age<=30)
P4.more30 <-subset(P4, Age>30)

C1cntless <-sum(P4.less30$Class == "C1")
C2cntless <-sum(P4.less30$Class == "C2")
sum <- C1cntless + C2cntless

GiniLess <- 1- (C1cntless/sum)^2 - (C2cntless/sum)^2
GiniLess




C1cntMore <-sum(P4.more30$Class == "C1")
C2cntMore <-sum(P4.more30$Class == "C2")
sum <- C1cntMore + C2cntMore

GiniMore <- 1- (C1cntMore/sum)^2 - (C2cntMore/sum)^2
GiniMore
