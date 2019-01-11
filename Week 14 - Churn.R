install.packages('corrplot')
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

churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)
churn$Churn <- as.factor(churn$Churn)

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
