library(ggplot2)
library(psych)
library(car)
library(olsrr)
install.packages("olsrr")

fevdata<-read.csv(file.choose(), header=T)
head(fevdata)
describe(fevdata$Age)
describe(fevdata$FEV)
describe(fevdata$Height)
describe(fevdata$Sex)
describe(fevdata$Smoke)

fullfev<- lm(FEV ~ Age + Height + Sex + Smoke, 
            data = fevdata)
summary(fullfev)

confint(fullfev, "Age", level = 0.95)
confint(fullfev, "(Intercept)", level = 0.95)
confint(fullfev, "Height", level = 0.95)
confint(fullfev, "Sex", level = 0.95)
confint(fullfev, "Smoke", level = 0.95)

modelcompare<- ols_step_all_possible(fullfev)
View(modelcompare)

finalfev<- lm(FEV ~ Age + Height + Sex, 
             data = fevdata)
summary(finalfev)

out1 = fortify(finalfev)
#residual
ggplot(out1, aes(x=.fitted, y=.stdresid))+ 
  geom_point(shape=16, size=3) + 
  labs(x = "Predicted FEV",
       y = "Studentized Residuals",
       title = "Studentized Residual Plot")+
  geom_hline(yintercept=0)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))
#qqplot
qqnorm(out1$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", 
       ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(out1$.resid)
#histogram of residuals
hist(out1$.resid, 
     main="Histogram of Residuals", xlab="Residuals")




