
#-------------------------------------------------
#install.packages('perm')
library(perm)
rm(list = ls())
#setwd("")
#install.packages('np')
library("np")


#-------------------------------------------------
perms <- chooseMatrix(8, 4)
A <- matrix(c(0.462, 0.731, 0.571, 0.923, 0.333, 0.750, 0.893, 0.692), nrow=8, ncol=1, byrow=TRUE)
treatment_avg <- (1/4)*perms%*%A
control_avg <- (1/4)*(1-perms)%*%A
test_statistic <- abs(treatment_avg-control_avg)
rownumber <- apply(apply(perms, 1, 
                         function(x) (x == c(0, 1, 0, 0, 0, 1, 1, 1))), 
                   2, sum)
rownumber <- (rownumber == 8)
observed_test <- test_statistic[rownumber == TRUE]

larger_than_observed <- (test_statistic >= observed_test)
#numbers in which the statistic exceeds the value in the observed date
sum(larger_than_observed)

df <- data.frame(perms,control_avg,treatment_avg,test_statistic)


#-------------------------------------------------
simul_stat <- as.vector(NULL)
schools <- read.csv('teachers_final.csv')
set.seed(1001)
for(i in 1:100) {
  print(i)
  schools$rand <- runif(100,min=0,max=1)
  schools$treatment_rand <- as.numeric(rank(schools$rand)<=49)
  schools$control_rand = 1-schools$treatment_rand
  simul_stat <-append(simul_stat,
            sum(schools$treatment_rand*schools$open)/sum(schools$treatment_rand) 
            - sum(schools$control_rand*schools$open)/sum(schools$control_rand))
}

schools$control = 1-schools$treatment
actual_stat <- sum(schools$treatment*schools$open)/sum(schools$treatment) - sum(schools$control*schools$open)/sum(schools$control)

sum(abs(simul_stat) >= actual_stat)/NROW(simul_stat)


#---------------------------------------------------
#Printing the ATE
ate <- mean(actual_stat)
ate

control_mean <- sum(schools$control*schools$open)/sum(schools$control)
treatment_mean <- sum(schools$treatment*schools$open)/sum(schools$treatment)

s_c <- (1/(sum(schools$control)-1))*sum(((schools$open-control_mean)*schools$control)^2)
s_t <- (1/(sum(schools$treatment)-1))*sum(((schools$open-treatment_mean)*schools$treatment)^2)

Vneyman <- (s_c/sum(schools$control) + s_t/sum(schools$treatment))
print(sqrt(Vneyman))
t_statistic <- (actual_stat-0)/sqrt(Vneyman) #t-stat, standardized

print(actual_stat-1.96*sqrt(Vneyman))
print(actual_stat+1.96*sqrt(Vneyman))
p_value <- 2*pnorm(-1*t_statistic,0,1)


ate/2


op_char <- 0.1
sig_lvl <- 0.05
the_gamma <- 0.5
tau <- ate/2
avg_var <- (s_c*(51/100) + s_t*(49/100))
num_samples_numerator <- (qnorm(1-op_char,0,1) + qnorm(1-sig_lvl/2,0,1))^2
num_samples_denom <- ((tau^2)/(avg_var)) * the_gamma*(1-the_gamma)
num_samples <- num_samples_numerator/num_samples_denom
num_samples


#---------------------------------------------------
attach(schools)
plot <- npreg(xdat=open, ydat= pctpostwritten, bws=.04,bandwidth.compute=FALSE)
plot(plot)

# Preliminaries
#-------------------------------------------------

#rm(list = ls())
#library(ggplot2)
#library(tidyverse)
#require(cowplot)

# Read file
schools <- read.csv('teachers_final.csv')

# Obtain treatment values
treatment_values <- filter(schools, treatment==1)

# Obtain controlled values
controlled_values <- filter(schools, treatment==0)

# Perform plot
ggplot(treatment_values, aes(open))+
  stat_ecdf(data=treatment_values, aes(open), color="darkblue")+
  stat_ecdf(data=controlled_values, aes(open), color="darkred")+
  xlab("Open")+
  ylab("CDF(X)")
#ggsave("treatmentControlledFOSD.pdf")

