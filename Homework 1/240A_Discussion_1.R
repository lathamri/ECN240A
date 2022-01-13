#  240A Discussion 1
#  January 7, 2022

rm(list=ls())    # clear memory
setwd("E:/Dropbox/Teaching/ARE240A/Winter 2022/Discussion")

#install.packages(c("tidyverse","ggplot2")
library(tidyverse,ggplot2)

cps <- read_tsv("https://www.ssc.wisc.edu/~bhansen/econometrics/cps09mar.txt",
                col_names=c("age","female","hisp","education","earnings","hours","week","union","uncov","region","race","marital"))
colnames(cps)

cps <- mutate(cps,l_earnings=log(earnings/(hours*week)))    # create new variable log earnings per hour

# do regression of log earnings on education
lm(formula=l_earnings~education, data=cps)    


# do regression of log earnings on female and education, save, and then summarise
reg <- lm(formula=l_earnings~female+education, data=cps)   # run and save regression in object reg
summary(reg)         # summarize regression


# do regression of log earnings on female and education, and save summary
reg_sum <- cps %>%
  lm(formula=l_earnings~female+education) %>%   # run and save regression in object reg
  summary()         # summarize regression

reg_sum    # print summary that we saved


## Material below is for future weeks.We cover ghraphics in Week 4

#  Make scatter plots (see Ch 2.3 in Hansen)
scatter_plot1 <- cps %>%
  mutate(female=ifelse(female==1,"female","male")) %>%
  ggplot(aes(x=education,y=l_earnings,color=factor(female,levels=c("female","male")))) +
    geom_point() +
    theme_minimal() +
    labs(y="Log earnings per hour",x="Education",color="Gender")

scatter_plot1


scatter_plot2 <- cps %>%
  mutate(female=ifelse(female==1,"female","male")) %>%
  group_by(education,female) %>%
  summarise(l_earnings=mean(l_earnings)) %>%
  ggplot(aes(x=education,y=l_earnings,color=factor(female,levels=c("female","male")))) +
    geom_point(size=3) +
    theme_minimal() +
    labs(y="Log earnings per hour",x="Education",color="Gender")

scatter_plot2
