Sys.getlocale()
Sys.setlocale("LC_ALL","English")

install.packages("faraway")
install.packages("knitr")
install.packages("pander")
install.packages("ggannotate")
install.packages("gt")
install.packages("gtsummary")

library(faraway)
library(dplyr)
data("teengamb")

str(teengamb)
head(teengamb)
?teengamb

# Fit a regression model with the expenditure on gambling as the response 
# and the sex, status, income and verbal score as predictors.

# (a) What percentage of variation in the response is explained by these predictors?
fit_tg <- lm(gamble ~ ., data = teengamb)
fit_tgs <- summary(fit_tg)
lm(gamble ~ sex, data = teengamb)

# (b) Which observation has the largest (positive) residual? Give the case number.
resi <- data.frame(index = c(1:47), res = fit_tg$residuals)
arrange(resi, desc(res))[1,]

names(fit_tg)
names(fit_tgs)

fit_tg$residuals == fit_tgs$residuals


# (c) Compute the mean and median of the residuals.
mean(fit_tg$residuals)
median(fit_tg$residuals)
res <- data.frame(residuals = fit_tg$residuals) 
res %>% summarise(mean = mean(residuals), median = median(residuals))

# (d) Compute the correlation of the residuals with the fitted values.
fitval <- fit_tg$fitted.values
cor(res, fitval)

# (e) Compute the correlation of the residuals with the income.
cor(res, teengamb$income)

# (f) For all other predictors held constant, what would be the difference in predicted
# expenditure on gambling for a male compared to a female?
(mean_male <- filter(teengamb, sex == 0)%>%summarise_all(mean)) 
(mean_female<- filter(teengamb, sex == 1)%>%summarise_all(mean)) 
PM <- predict(fit_tg, mean_male, interval = "confidence") 
PF <- predict(fit_tg, mean_female, interval = "confidence")
PM[,1]-PF[,1] 


library(gt)
data.frame(explained = fit_tgs$r.squared) %>% gt()
