# Chapter 4 Code

leukemia <- read.csv(file = "Data/leukemia.csv")

# Logistic regression ---------------------------------------------------

fit1 <- glm(tumor ~ ., data = leukemia, family = binomial)
summary(fit1)

contrasts(leukemia$tumor) #dummy is 1 for AML, so probs are for AML over ALL

probs1 <- predict(fit1, type = "response")
head(probs1)

pred1 <-  rep("ALL", 72)
pred1[probs1 > 0.5] <- "AML" #if prob > 0.5 predict AML


table(pred1, leukemia$tumor) #confusion matrix

pred1 == leukemia$tumor
mean(pred1 == leukemia$tumor)

library(pROC)
plot.roc(tumor ~ probs1, asp = NA, data = leukemia)



# Discriminant analysis --------------------------------------------------

# lda
library(MASS)
fit2 <- lda(tumor ~ ., data = leukemia)
summary(fit2)
fit2

pred2 <- predict(fit2, leukemia)
table(pred2$class, leukemia$tumor)

mean(pred2$class == leukemia$tumor) #same as logistic

# qda
fit3 <- qda(tumor ~ ., data = leukemia)
summary(fit3)
fit3

pred3 <- predict(fit3, leukemia)
table(pred3$class, leukemia$tumor)

mean(pred3$class == leukemia$tumor) #better

plot.roc(tumor ~ pred3$posterior[,1], asp = NA, col = "blue", add = TRUE, data = leukemia)



# Training and test data -----------------------------------------------

dim(leukemia)
set.seed(1)
train <- sample(1:72, 58)

train_leukemia <- leukemia[train, ]
test_leukemia <- leukemia[-train, ]

# logistic
fit4 <- glm(tumor ~ ., data = train_leukemia, family = binomial)
probs4 <- predict(fit4, test_leukemia, type = "response")
pred4 <-  rep("ALL", 14)
pred4[probs4 > 0.5] <- "AML"
table(pred4, test_leukemia$tumor)

# qda
fit5 <- qda(tumor ~ ., data = train_leukemia)
pred5 <- predict(fit5, test_leukemia)
table(pred5$class, test_leukemia$tumor)

# knn
library(class)

train_leukemia_x <- train_leukemia[, 1:10]
train_leukemia_y <- train_leukemia[, 11]
test_leukemia_x <- test_leukemia[, 1:10]
test_leukemia_y <- test_leukemia[, 11]

fit6 <- knn(train_leukemia_x, test_leukemia_x, train_leukemia_y, k = 5)
table(fit6, test_leukemia_y)
