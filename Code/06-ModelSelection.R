# Chapter 6 Code

wine <- read.csv(file = "Data/wine.csv")
park <- read.csv(file = "Data/park.csv")
gene <- read.csv(file = "Data/gene.csv")

# Subset selection ---------------------------------------------------

library(leaps)

#best subset
fit.sub <- regsubsets(quality ~ ., data = wine, nvmax = 10)	
summary(fit.sub)

summary(fit.sub)$adjr2
summary(fit.sub)$cp
summary(fit.sub)$bic

plot(fit.sub, scale = "bic")

reg.summary <- summary(fit.sub)

par(mfrow = c(2,2)) 

plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(7, reg.summary$adjr2[7], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l") 
which.min(reg.summary$cp) 
points(6, reg.summary$cp[6], col = "red", cex = 2, pch = 20) 

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l") 
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)


#forwards/backwards stepwise
fit.for <- regsubsets(quality ~ ., data = wine, nvmax = 10, method = "forward")
summary(fit.for)

fit.back <- regsubsets(quality ~ ., data = wine, nvmax = 10, method = "backward")
summary(fit.back)

par(mfrow=c(1,1))



# Regularization -----------------------------------------------------

library(glmnet)
set.seed(1)
x <- model.matrix(quality ~ ., wine)[, -1]
y <- wine$quality
grid <- 10^seq(10, -2, length = 100)


#ridge
fit.Ridge <- glmnet(x, y, alpha = 0 , lambda = grid)

fit.Ridge$lambda[75]
coef(fit.Ridge)[, 75]
coef(fit.Ridge, s = fit.Ridge$lambda[75])

predict(fit.Ridge, s = 10, type = "coefficients")

ridge.pred1 <- predict(fit.Ridge, s = fit.Ridge$lambda[75], newx = x)
mean((ridge.pred1 - y)^2)

ridge.pred2 <- predict(fit.Ridge, s = 10, newx = x)
mean((ridge.pred2 - y)^2)
coef(fit.Ridge, s = 10)

ridge.pred3 <- predict(fit.Ridge, newx = x)
dim(ridge.pred3)
head(ridge.pred3)

mean(ridge.pred3[, 75] == ridge.pred1)

summary(ridge.pred1)


#lasso
fit.Lasso <- glmnet(x, y, alpha = 1, lambda = grid)
plot(fit.Lasso)

coef(fit.Lasso, s = fit.Lasso$lambda[75])
coef(fit.Lasso, s = fit.Lasso$lambda[90])
predict(fit.Lasso, s = 0.05, type = "coefficients")
coef(fit.Lasso, s = 0.05)

lasso.pred1 <- predict(fit.Lasso, s = 0.05, newx = x)
mean((lasso.pred1 - y)^2)
summary(lasso.pred1)

lasso.pred2 <- predict(fit.Lasso, s = fit.Lasso$lambda[90], newx = x)
mean((lasso.pred2 - y)^2)
summary(lasso.pred2)



# Dimension reduction ------------------------------------------------

library(pls)
set.seed(100)

fit.PCR <- pcr(quality ~ ., data = wine, scale = TRUE, validation = "CV")
summary(fit.PCR)
validationplot(fit.PCR, val.type = "MSEP")

pred.PCR <- predict(fit.PCR, wine, ncomp = 2)
mean((pred.PCR - wine$quality)^2)



# CV and train/test --------------------------------------------------

#cv with glmnet
set.seed(234)

x <- model.matrix(out ~ ., gene)[, -1]
y <- gene$out

fit.gene <- cv.glmnet(x, y, alpha = 1, nfolds = 5)
fit.gene$lambda
fit.gene$lambda.min
fit.gene$lambda.1se
coef(fit.gene, s = c(fit.gene$lambda.min, fit.gene$lambda.1se))
predict(fit.gene, newx = x, s = "lambda.min")


#train/test
set.seed(1)
train <- sample(1:dim(park)[1], 0.75*dim(park)[1])

x <- model.matrix(total_UPDRS ~ ., park)[, -1]
y <- park$total_UPDRS

fit.r <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(fit.r)
bestlam.r <- fit.r$lambda.min
pred.ridge <- predict(fit.r, s = bestlam.r, newx = x[-train, ])
mean((pred.ridge - y[-train])^2)

fit.l <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(fit.l)
bestlam.l <- fit.l$lambda.min
pred.lasso <- predict(fit.l, s = bestlam.l, newx = x[-train, ])
mean((pred.lasso - y[-train])^2)

fit.reg <- lm(total_UPDRS ~ ., data = park, subset = train)
pred.reg <- predict(fit.reg, park[-train, ])
mean((pred.reg - park$total_UPDRS[-train])^2)

best.fit <- glmnet(x, y, alpha = 1)
predict(best.fit, s = bestlam.l, type = "coefficients")






