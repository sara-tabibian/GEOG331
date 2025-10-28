###script to practice using linear regression using iris data#####

rm(list = ls())

#subset the virginica species to create new dataframe

flower <- iris[iris$Species == "virginica",]


#make a scatter plot to look at relationship between sepal length vs petal length
plot(flower$Sepal.Length, flower$Petal.Length, pch = 19, 
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Iris virginica")

#fit a regression model 
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)

#plot the residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab = "Sepal Length", ylab = "Residuals")
abline(h=0)

#check normality of residuals
hist(summary(fit)$residuals, col="red",
     main = "Residual Distribution", xlab = "Residuals")

## qqnorm of qqline can provide another visual check
qqnorm(summary(fit)$residuals, pch=19)
qqline(summary(fit)$residuals, pch=19)

#use shapiro wilks test to check normality
shapiro.test(summary(fit)$residuals)

#testing a commit