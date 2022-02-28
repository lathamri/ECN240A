rm(list=ls())

# Bootstrap Wald test of H0: b1*b2=theta0


library(tidyverse)
library(broom)
library(rsample)
library(sandwich)
library(furrr)
future::plan(multiprocess)

set.seed(2022)

## number of repetitions
num_reps<-1000
num_boots<-200

## Define a function to apply to the bootstrap data
bootstrap_func <- function(splits) {
  reg <- lm(y~x1+x2,analysis(splits))
  boot_stat <- list(b_boot_1=coefficients(reg)[2],
                    b_boot_2=coefficients(reg)[3],
                    v_boot_11=vcovHC(reg, type="const")[2,2],
                    v_boot_12=vcovHC(reg, type="const")[2,3],
                    v_boot_22=vcovHC(reg, type="const")[3,3])
  return(boot_stat)
}

## Define a simulation function 
sim_func <- function(rep,n,beta2,sig){ 
  # generate data
  df <-tibble(x1 = rnorm(n=n,mean=0,sd=1),
              x2 = rnorm(n=n,mean=0,sd=1),
              y = x1+x2*beta2+rnorm(n=n,mean=0,sd=sig))

  # Run regression
  reg <- lm(y~x1+x2,data=df)
    bhat <- matrix(coefficients(reg),nrow=3,ncol=1)
    R_nl <- matrix(c(0,bhat[3,1],bhat[2,1]),nrow=1,ncol=3)
    W_nl <- (bhat[2,1]*bhat[3,1]-beta2) %*% solve(R_nl%*%vcovHC(reg, type="const")%*%t(R_nl)) %*% (bhat[2,1]*bhat[3,1]-beta2)

  # Run bootstrap
  boots <- bootstraps(df, times = num_boots) %>%
    mutate(boot_results = map(splits, bootstrap_func)) %>%
    unnest_wider(boot_results)   %>%
    mutate(Wald_nl_boot=((b_boot_1*b_boot_2-bhat[3,1])^2)/(v_boot_11*b_boot_2^2+v_boot_22*b_boot_1^2+2*v_boot_22*b_boot_1*b_boot_2))  %>%
    select(Wald_nl_boot)

  # Make list of results to save
  results <- list(W_nl_ChiSq=W_nl>2.71,
                  ptile_W_nl=W_nl>quantile(boots$Wald_nl_boot,probs=0.9),
                  W_nl=W_nl)
  return(results)  
}


## Parameter Settings
param_list <- expand.grid(rep=1:num_reps, n=c(25), beta2=c(0.5,1,5), sig=1) 


## Run simulation
sim_out <- param_list %>% 
  mutate(results = future_pmap(param_list, sim_func)) %>%
  unnest_wider(results)

## Print result
sim_print <- group_by(sim_out,n,beta2) %>%
  mutate(W_nl   =mean(W_nl_ChiSq)) %>%
  mutate(W_nl_b =mean(ptile_W_nl)) %>%
  ungroup() %>%
  select(n,beta2,W_nl,W_nl_b) %>%
  distinct() 
sim_print



## Create plot
pl_density <- sim_out %>%
  select(n,beta2,W_nl) %>%
  #filter(beta2==.5) %>%
  pivot_longer(cols = starts_with("W_"),names_to="Test", values_to="Statistic") %>%
  ggplot(aes(x=Statistic,fill=factor(beta2)))+
  geom_density(alpha=0.5)+
  labs(fill="beta2")+
  xlim(0,10)
pl_density  


## Result Summary
# Delta method works for nonlinear statistic when beta2 is small
