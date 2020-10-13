rm(list = ls())
all_data <- read.csv("paxson_corrected.csv")

#install.packages("dummies")
#install.packages("car")
#library(dummies)
#library(car)

r <- dummy(all_data$region)
r <- r[,2:ncol(r)] #Taking out region 2..?? 
rgs <- colnames(r)

y <- dummy(all_data$year)
y <- y[,2:ncol(y)] #Only want it for years 81 and 86 
yrs <- colnames(y)

ir_dat <- data.frame(all_data[,c("inc","dev1","dev2","dev3", "dev4","dvsq1","dvsq2","dvsq3","dvsq4","p0to5", "m12","m18","m18elem","m18sec","m18pos","m65", "f12","f18","f18elem","f18sec","f18pos","f65", "of0","of1","of2","of3","of4","of5")],y,r)
ir <- lm(inc ~ .,data=ir_dat)

betas <- summary(ir)
sr_dat <- data.frame(all_data[,c("save2","p0to5","p6to11","p12to17", "p18to64","p65","sd1","sd2","sd3","sd4")],y)
permvars <- c("p0to5","m12","m18","m18elem","m18sec","m18pos", "m65","f12","f18","f18elem","f18sec","f18pos", "f65","of0","of1","of2","of3","of4","of5",yrs,rgs)
num_pv <- length(permvars)
sr_dat$incperm <- 0

for (i in 1:num_pv) {
  thisvar <- permvars[i]
  sr_dat$incperm <- sr_dat$incperm +
    coef(betas)[thisvar,"Estimate"]*ir_dat[,c(thisvar)]
}

sd(sr_dat$incperm)
transvars <- c("dev1","dev2","dev3","dev4","dvsq1","dvsq2", "dvsq3","dvsq4",yrs)
num_tv <- length(transvars)
sr_dat$inctrans <- 0

for (i in 1:num_tv) {
  thisvar <- transvars[i]
  sr_dat$inctrans <- sr_dat$inctrans +
    coef(betas)[thisvar,"Estimate"]*ir_dat[,c(thisvar)]
  
}

sd(sr_dat$inctrans)
sr_dat$incunexp = all_data$inc - sr_dat$incperm - sr_dat$inctrans
sd(sr_dat$incunexp)
q8 <- lm(save2 ~ ., data = sr_dat )
coef(summary(q8))[c("incperm","inctrans"),]
linearHypothesis(q8,"inctrans=incperm")