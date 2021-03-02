#Let's load two raw data file diff_wage and LFP
#diff_wage stores the wage difference between men and women;
#LFP stores the percentage of women in the labor force 
library(moments)
library(dplyr)
diff_wage <- read.csv("Wage_gap_GP.csv" , header= TRUE)
LFP <- read.csv("LFPFemale.csv", header=TRUE)
colnames(diff_wage)[colnames(diff_wage) == "LES1252881900Q_LES1252882800Q"] <- "Wage gap"
names(diff_wage)
#It is important to note that the wage gap is the difference in wage of men - women 
colnames(LFP)[colnames(LFP) == "LNS11300002"] <- "percent_women"
names(LFP)
#Where percent women means the percentage of women in the work force
#Datatotal is the merged version of both diff_wage and LFP 
Datatotal<-merge(diff_wage, LFP, by=c("DATE"))
Datatotal
#Let's compute some statistics about the raw data 
#Stats for diff_wage 
### a) counting the number of variables in the data set 
observations <- dim(diff_wage) [1]
variables <- dim(diff_wage) [2]
#here observations are the number of rows and variables are the number of columns
### b) reporting the quartiles of diff_wage 
min(diff_wage$`Wage gap`)
#The minimum wage gap is $61
max(diff_wage$`Wage gap`)
#The maximum wage gap is $157
quantile(diff_wage$`Wage gap`)
#The quartiles in wage gap are displayed with the quantile function 
### c) creating a graph to represent the data 
diff_wage$Date3 <- as.Date(diff_wage$DATE)
diff_wage$year <- as.numeric(format(diff_wage$DATE3,`%Y`))
diff_wage$year<-substr(diff_wage$DATE,1,4)
diff_wage$month <- substr(diff_wage$DATE,6,7)
diff_wage$quarter<-ifelse(diff_wage$month=="01",1,ifelse(diff_wage$month=="04",2,ifelse(diff_wage$month=="07",3,4)))
Graph1<- plot(diff_wage$Date3, diff_wage$`Wage gap`, xlab = "Year" , ylab="Difference in Wage", main = "Difference in wage over time")
#Stats for LFP
# for tom diff_wage$crisis2001<-ifelse(diff_wage$year==2001,1,0)
### d) counting the number of variables in the data set 
observation.1 <- dim(LFP) [1]
variables.1 <- dim(LFP) [2]
### e) reporting the quartiles for LFP
m_percentwomen <- mean(Datatotal$percent_women)
min(LFP$percent_women)
max(LFP$percent_women)
quantile(LFP$percent_women)
### f) creating a graph to represent date
LFP$Date3 <- as.Date(LFP$DATE)
LFP$year<-substr(LFP$DATE,1,4)
LFP$month <- substr(LFP$DATE,6,7)
LFP$quarter<-ifelse(LFP$month=="01",1,ifelse(LFP$month=="04",2,ifelse(LFP$month=="07",3,4)))
Graph2<-plot(LFP$Date3, LFP$percent_women, xlab = "Year", ylab= "Percentage of women in workforce", main="Percentage of women in workforce over time")
### g) Conducting t-test 
#We split the data into two parts based on the percentage of women in the workforce
# Values >50% were 0 and <50% were 1
#### NULL HYPOTHESIS H0= m_women0 = m_women 1 ####
#### ALTERNATE HYPOTHESIS H1= m_women != m_women 1 ####
Datatotal$womendummy<-ifelse(Datatotal$percent_women>55,1,0)
n0 <- length(Datatotal$`Wage gap`[Datatotal$womendummy==0])
n1 <- length(Datatotal$`Wage gap`[Datatotal$womendummy==1])
t_test <-t.test(Datatotal$`Wage gap`[Datatotal$womendummy==0],Datatotal$`Wage gap`[Datatotal$womendummy==1])
t_test
#From the t-test we can reject H0 implying that the number of women in the labor force has been increasing over time 
# and the wage gap has been decreasing 
#To revise the t-test we conducted a 95% confidence interval test 
#### NULL HYPOTHESIS H0= m_women0 = m_women 1 ####
#### ALTERNATE HYPOTHESIS H1= m_women != m_women 1 ####
### h) Creating a confidence Interval 
m_women0 <- mean(Datatotal$`Wage gap`[Datatotal$womendummy==0])
m_women1 <- mean(Datatotal$`Wage gap`[Datatotal$womendummy==1])
var_women0 <- var(Datatotal$`Wage gap`[Datatotal$womendummy==0])/n0
var_women1 <- var(Datatotal$`Wage gap`[Datatotal$womendummy==1])/n1
diff_mean <- m_women0 - m_women1
CI_women <- diff_mean + sqrt(var_women1 + var_women0)* c(-1.96,1.96)
#Since diff_mean falls inbtween CI_women we can be confident in the t-test results 
### i) Calculating heteroskedascity 
ols.fit <- lm(`Wage gap` ~ percent_women, data=Datatotal)
summary(ols.fit)
Graph3 <- plot(Datatotal$percent_women, Datatotal$`Wage gap`, main="Correlation between Wage gap and women participation" , xlab="%women in labor force", ylab="Wage gap", type='p', cex=1, col='black')
Graph4 <- abline(ols.fit, col='red')
library(sandwich)
library(lmtest)
het_Datatotal <- coeftest(ols.fit, vcov= vcovHC(ols.fit, "HC1"))
het_Datatotal
### j) conducting a confidence interval to see if the data is statistically significant 
confint(het_Datatotal, level=0.99)
CI <- (ols.fit$coefficients[2]/het_Datatotal[[4]])
abs(CI)<qnorm(0.995)
### k) findind the p-value 
pvalue <- 2*(1-pnorm(abs(CI)))