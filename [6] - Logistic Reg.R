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


library(ggplot2)

summary(admission)

sapply(admission, sd)

by(daily$hum,daily$season, mean)

with(admission, table(admit, rank))

admission$rank <- factor(admission$rank)

ggplot(admission, aes(x = gpa, y = admit)) + geom_point(alpha=.25)


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
