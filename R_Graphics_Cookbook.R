Sys.getlocale()
Sys.setlocale("LC_ALL","English")

library(ggplot2)
library(gcookbook)

# Chapter 2

# Scatter plot
data(mtcars)
plot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Line graph
data(pressure)
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "red")

qplot(temperature, pressure, data = pressure, geom = c("line","point"))
ggplot(data = pressure, aes(x = temperature, y = pressure)) +
  geom_line() + geom_point()

# Bar graph
data(BOD)
barplot(BOD$demand, names.arg = BOD$Time)
qplot(demand, data = BOD, geom = "bar")
ggplot(data = BOD, aes(x = Time, y = demand)) +
  geom_bar(stat = "identity") # stat="identity"는 geom_bar()의 STAT 값을 원데이터셋에 있는 값으로 쓰겠다는 것을 설정하는 의미


barplot(table(mtcars$cyl)) #히스토그램 같은 기능의 Barplot 그리기
qplot(cyl, data = mtcars, geom = "bar")
ggplot(data = mtcars, aes(x = cyl)) + 
  geom_bar(stat = "count")

# Histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) #bins 숫자 특정
qplot(mpg, data = mtcars)
qplot(mpg, data = mtcars, binwidth = 4)

# Box plot
data("ToothGrowth")
plot(ToothGrowth$supp, ToothGrowth$len) #x에 factor가 와서 자동으로 box plot 생성
boxplot(len ~ supp, data = ToothGrowth)
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(data = ToothGrowth, aes(x = supp, y = len)) +
  geom_boxplot()

# Box plot w/ interaction
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(data = ToothGrowth, aes(x = interaction(supp, dose), y = len)) +
  geom_boxplot() #x에 변수 2개 넣으려면 interaction

# Function curve
curve(x^3 - 5*x, from = -4, to = 4)
myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}
curve(myfun(x), from = 0, to = 20)
curve(1 - myfun(x), add = TRUE, col = "red") # Add a line
ggplot(data.frame(x=c(0, 20)), aes(x=x)) + 
  stat_function(fun = myfun, geom="line")

# Chapter 3
ggplot(pg_mean, aes(x = group, y = weight)) + 
  geom_bar(stat="identity")
args(geom_bar)

BOD
str(BOD)
ggplot(BOD, aes(x = Time, y = demand)) +
  geom_bar(stat = "identity")
ggplot(BOD, aes(x = factor(Time), y = demand)) +
  geom_bar(stat = "identity") #x를 이산형으로 처리하기 위하여 factor화
ggplot(BOD, aes(x = factor(Time), y = demand)) +
  geom_bar(stat = "identity", fill = "brown", colour = "black")

cabbage_exp # Grouping bars
ggplot(data = cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", position = "Dodge", colour = "black") +
  scale_fill_brewer(palette="Pastel1") #이산형 변수에서 사용하는 팔레트

ggplot(data = cabbage_exp[1:5,], aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", position = "Dodge", colour = "black") +
  scale_fill_brewer(palette="Pastel1") #이산형 변수에서 사용하는 팔레트

diamonds
ggplot(diamonds, aes(x = cut)) +
  geom_bar(stat = "count")
ggplot(diamonds, aes(x = carat)) +
  geom_bar(stat = "bin") #x가 연속형일 때 geom_bar로 히스토그램 그리기

upc <- uspopchange %>% 
  filter(rank(Change) > 40)
ggplot(data = upc, aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_manual(values = c("#669933", "#FFCC66")) +
  xlab("State") #X축 순서변경은 reorder

climate
csub <- climate %>% 
  filter(Source == "Berkeley", Year >= 1900) %>%
  mutate(pos = Anomaly10y >= 0)
ggplot(data = csub, aes(x = Year, y = Anomaly10y, fill = pos)) +
  geom_bar(stat = "identity", colour = "black", size = 0.25) +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = F) #선 굵기는 size로 지정

ggplot(pg_mean, aes(x = group, y = weight)) + 
  geom_bar(stat="identity", width = 1) #bar 너비 조정은 width로

ggplot(data = cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7)) +
  scale_fill_brewer(palette="Pastel1") #dodge 간격 조정은 position_dodge()

data("cabbage_exp")

ggplot(data = cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Pastel1") +
  guides(fill = guide_legend(reverse = FALSE)) #범례 순서 변경은 guides에서

ggplot(data = cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar, order = desc(Cultivar))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Pastel1") #Stack 순서 변경은 aes의 order 로 조정

library(plyr)
library(dplyr)

ce <- cabbage_exp %>% 
  group_by(Date) %>% 
  mutate(percent_weight = Weight / sum(Weight) * 100) # ddply 기능을 dplyr에서 활용

ggplot(data = ce, aes(x = Date, y = percent_weight, fill = Cultivar, order = Cultivar)) +
  geom_bar(stat = "identity")

ggplot(data = cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Weight), vjust = -0.5, colour = "black") # 데이터레이블 추가

library(survival)
library(survminer)
library(ISwR)

data(ovarian)
glimpse(ovarian)
str(ovarian)
head(ovarian)


data(melanom)
glimpse(melanom)
melanom$sex <- factor(melanom$sex,
                      levels = c("1", "2"),
                      labels = c("female", "male"))
melanom$ulc <- factor(melanom$ulc,
                      levels = c("1", "2"),
                      labels =c("present", "absent"))

arrange(melanom, days)
?melanom
surv_obj <- Surv(melanom$days, melanom$status == 1) #Surv(time, event)
fit1 <- survfit(surv_obj ~ sex, data = melanom)
fit2 <- survfit(surv_obj ~ ulc, data = melanom)
summary(fit1)
ggsurvplot(fit1, data = melanom, conf.int = TRUE, pval = TRUE)
ggsurvplot(fit2, data = melanom, pval = TRUE)

fit.cox <- coxph(surv_obj ~ ulc + thick + sex, data = melanom)
ggforest(fit.cox, data = melanom)



library(survival)
library(survminer)
library(ISwR)
library(ggplot2)
library(gridExtra)

data("graft.vs.host")
surv_gh <- Surv(graft.vs.host$time, graft.vs.host$dead)
fit_gvhd <- survfit(surv_gh ~ gvhd, data = graft.vs.host)
summary(fit_gvhd)
surv_plot <- ggsurvplot(fit_gvhd, data = graft.vs.host, pval = TRUE,
                        risk.table = TRUE, 
                        title = "Kaplan–Meier plots for graft.vs.host",
                        subtitle = "(Grouped by GVHD)",
                        legend.title = "GVHD", legend.labs = c("No", "Yes"))
surv_plot$plot <- surv_plot$plot + 
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5))
print(surv_plot)
survdiff(surv_obj ~ gvhd, data = graft.vs.host)

cox_gvhd <- coxph(surv_obj ~ gvhd, data = graft.vs.host)
cox_all <- coxph(surv_obj ~ gvhd + index + donage + rcpage + preg, data = graft.vs.host)

summary(cox_gvhd)
summary(cox_all)

cox_gvhd_plot <- ggforest(cox_gvhd, data = graft.vs.host,
                          main = "Hazard ratio (GVHD only)")
cox_all_plot <- ggforest(cox_all, data = graft.vs.host,
                         main = "Hazard ratio (All)")
grid.arrange(cox_gvhd_plot, cox_all_plot, nrow = 1, ncol = 2)

ggcoxzph(cox.zph(cox_gvhd))
ggcoxzph(cox.zph(cox_all))



data("stroke")
surv_str <- Surv(stroke$obsmonths, stroke$dead)
cox_sexage <- coxph(surv_str ~ sex + age, data = stroke)
cox_sex <- coxph(surv_str ~ sex, data = stroke)

summary(cox_sexage)
summary(cox_sex)

cox_sexage_plot <- ggforest(cox_sexage, data = stroke,
                            main = "Hazard ratio (Sex + Age)")
cox_sex_plot <- ggforest(cox_sex, data = stroke,
                         main = "Hazard ratio (Sex only)")

grid.arrange(cox_sexage_plot, cox_sex_plot, nrow = 1, ncol = 2)

ggplot(data = stroke, aes(x = sex, y = age, fill = sex)) + 
  ggtitle(label = "Boxplot of age by sex") +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel1") +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 1.5, face = "bold"),
        axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15, vjust = -1),
        axis.title.y = element_text(size = 15)) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")

fit_sex <- survfit(surv_str ~ sex, data = stroke)
ch_plot <- ggsurvplot(fit_sex, data = stroke, fun = "cumhaz",
                      title = "Cumulative hazard plots for stroke",
                      subtitle = "(Grouped by sex)",
                      legend.title = "sex", legend.labs = c("Female", "Male"))
ch_plot$plot <- ch_plot$plot + 
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5))
print(ch_plot)


library(dplyr)
stroke %>% 
  group_by(sex) %>% 
  summarise(avg_age = mean(age))

stroke <- stroke %>% 
  mutate(age2 = ifelse(age >= 65, "age >= 65", "age < 65"))

fit_age <- survfit(surv_str ~ age2, data = stroke)
summary(fit_age)
ch_plot2 <- ggsurvplot(fit_age, data = stroke, fun = "cumhaz",
                      title = "Cumulative hazard plots for stroke",
                      subtitle = "(Grouped by age 65)",
                      legend.title = "Age", legend.labs = c(">=65", "Under 65"))
ch_plot2$plot <- ch_plot2$plot + 
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5))
print(ch_plot2)

table(stroke$age2)

cox_sexage2 <- coxph(surv_str ~ sex + age2, data = stroke)
ggforest(cox_sexage2, data = stroke, main = "Hazard ratio (Sex + Age2)")
