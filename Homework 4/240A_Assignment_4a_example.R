rm(list=ls())

library(tidyverse)
library(broom)
library(sandwich)

set.seed(2022)

## number of repetitions
num_reps<-500


## Define a simulation function 
sim_func <- function(rep,n,J,b1){ 
  df <-tibble(err = (rexp(n=n, rate=J)-1/J),  # errors are exponential(J).  Mean =0, std dev = 1/J
                  x1=rnorm(n=n,mean=0,sd=1),
                  y = x1*b1+err)
  reg <- lm(y~x1, data=df)
  results <- list(beta1=coefficients(reg)[2],
                  var_beta1=vcovHC(reg, type="const")[2,2]) 
  return(results)  
}

## Parameter Settings
param_list <- expand.grid(rep=1:num_reps, n=c(25,100), J=c(.5,5), b1=seq(0,1,0.05)) 

## Run simulation
sim_out <- param_list %>% 
  mutate(results = pmap(param_list, sim_func)) %>%
  unnest_wider(results) %>%
  group_by(n,J,b1) %>%
  mutate(test_b1=abs(beta1/sqrt(var_beta1)>1.96)) %>%
  mutate(prob_reject=mean(test_b1)) %>%
  mutate(asy_power=pnorm(-1.96-sqrt(n)*b1/(1/J))+pnorm(sqrt(n)*b1/(1/J)-1.96)) %>%
  ungroup() %>%
  select(n,J,b1,prob_reject,asy_power) %>%
  distinct()



## Create plot for question 1
pl_power_q1 <- sim_out %>%
  filter(J==0.5) %>%
  mutate(sample= case_when( n==25 ~ "n=25", n==100 ~ "n=100")) %>%
  mutate(asy_power=pnorm(-1.96-sqrt(n)*b1/(1/J))+pnorm(sqrt(n)*b1/(1/J)-1.96))%>%
  ggplot() +
    geom_line(aes(x=b1,y=prob_reject,group=sample,color=sample)) + 
    geom_line(aes(x=b1,y=asy_power,group=sample,color="Asymptotic"))  +
    theme_minimal()
  

pl_power_q1


## Create plot for question 2
pl_power_q2 <- sim_out %>%
  filter(n==25) %>%
  mutate(sample= case_when( J==.5 ~ "J=0.5", J==5 ~ "J=5")) %>%
  mutate(asy_power=pnorm(-1.96-sqrt(n)*b1/(1/J))+pnorm(sqrt(n)*b1/(1/J)-1.96)) %>%
  ggplot() +
    geom_line(aes(x=b1,y=prob_reject,group=sample,color=sample)) + 
    geom_line(aes(x=b1,y=asy_power,group=sample,color="Asymptotic"))  +
    theme_minimal() 
  
pl_power_q2

