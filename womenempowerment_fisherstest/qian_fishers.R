qian <- read.csv("qian.csv")


mean(qian$biryr)
quantile(qian$sex, c(0.50,0.75))
max(qian$teasown)

qian$post = qian$biryr >=1979
qian$teaspost = qian$teasown * qian$post
sum(qian$teaspost)
mean(qian$teaspost)


q25 <- lm(sex ~ teasown + post + teaspost, data=qian)
summary(q25)

q26 <- lm(sex ~ post + teaspost, data=qian)
summary(q26)

### FISHERS TEST
perms <- chooseMatrix(8,4)
A <- matrix(c(0.85, 0.99, 1, .76, .26, 0.45, 0.97, 0.72), nrow=8, ncol=1, byrow=TRUE)
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
