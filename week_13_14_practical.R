library(tidyverse)
library(lubridate)
library(MASS)

setwd("~/Desktop/MBIO612-Fun R/week 13/data")
rawdata = tibble(read.csv("YERockfish.csv",header=T))
rawdata 


rawdata2 = rawdata %>% mutate(rawdata, DateR = as.POSIXct(x=rawdata$date, format= "%m/%d/%Y")
    )
# column is now of type dttm (date and time)
head(rawdata2,10)

# Count the number of lines in your file
str(rawdata2)

#Plot the count of observations per year
rawdata3 = rawdata2 %>% mutate (rawdata2, year=year(rawdata2$DateR))
rawdata3 %>% group_by(year) %>% summarise(n = n())
rawdata3

#Remove a year for which there are less than 5 entries
rawdata4 = rawdata3 %>% group_by(year) %>% filter(n()>5)
#Count the number of entries
rawdata4 %>% group_by(year) %>% summarise(n = n())

#Model the fish maturity
#clean data
rawdata5 = rawdata4 %>% filter(!is.na(maturity))
#clean data
rawdata5 = rawdata5 %>% mutate( 
              dblmaturity = ifelse(maturity == "Immature", 0, 1)) 
head(rawdata5,20)
# plot
ggplot()+
  geom_point(aes(x=rawdata5$length, y=rawdata5$dblmaturity)) +
  geom_hline(yintercept=0.5,,linetype="dashed", alpha=0.6, size=1)+
  xlab("lenght") +
  ylab("maturity") 

# linear regression model
model = lm(rawdata5$dblmaturity~rawdata5$length)
summary(model)  

ggplot()+ 
  geom_point(aes(rawdata5$length, y=rawdata5$dblmaturity)) + 
  geom_hline(yintercept=0.5, , linetype="dashed", alpha=0.6, size =1) + 
  geom_abline(intercept =  -0.523428, slope = 0.028744, color ="red", size =1) + 
  xlab("length") +
  ylab("maturity") +
  theme(text = element_text(size = 22))   


# logistic regression model 
logistic_model = glm(data=rawdata5, dblmaturity~length, family="binomial")
summary(logistic_model)

beta_0 = logistic_model$coefficients[1]
beta_1 = logistic_model$coefficients[2]


x_axis = seq(min(rawdata5$length)-3, max(rawdata5$length)+3, 0.05)
g_x = 1 / (1+exp(-(beta_0 + beta_1 * x_axis)))
ggplot()+ 
  geom_point(aes(x=rawdata5$length, y=rawdata5$dblmaturity)) + 
  geom_line(aes(x_axis, g_x)) +
  geom_vline(aes(xintercept = 38.7)) +
  xlab("Length") +
  ylab("Maturity") +
  theme(text = element_text(size = 22))

#What is the length at which the probability of picking a mature fish is 0.5?
# length around 38.7

rawdata6 = rawdata5 %>% mutate(
                         era = if_else(year<2002, "pre_2000", "era2002_and_after"))
rawdata6 = rawdata6 %>% mutate(
                         era_dbl = if_else(year<2002, 0, 1))
head(rawdata6,10)

#era and length with maturity as outcome
# logistic regression model 
logistic_model2 = glm(data=rawdata6, era_dbl~length, family="binomial")
summary(logistic_model2)

beta_0 = logistic_model2$coefficients[1]
beta_1 = logistic_model2$coefficients[2]


x_axis = seq(min(rawdata6$length)-3, max(rawdata6$length)+3, 0.05)
g_x = 1 / (1+exp(-(beta_0 + beta_1 * x_axis)))
ggplot()+ 
  geom_point(aes(x=rawdata6$length, y=rawdata6$era_dbl)) + 
  geom_line(aes(x_axis, g_x)) +
  geom_vline(aes(xintercept = 38.7)) +
  xlab("Length") +
  ylab("Era") +
  theme(text = element_text(size = 22))

#ANOVA
aov = aov( formula = dblmaturity ~ length + era_dbl, data = rawdata6 )
summary(aov)

#Does the maturity differ between the two eras? 
# According to ANOVA maturity is not different between two era