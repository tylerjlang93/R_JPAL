#install.packages("sandwich")
#install.packages("lmtest")
#library(lmtest)
#library(sandwich)
#-------------------------ALTERNATIVE PACKAGE---------
#install.packages("multiwayvcov")
#library(multiwayvcov)
#------------------------------------------------------
rm(list = ls())
get_CL_vcov <- function(model, cluster){
  
  #A function to return the variance/covariance matrix
  
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(model),2,function(x) tapply(x,cluster,sum))
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}

get_CL_df <- function(model, cluster){
  
  #A program returning the number of degrees of freedom
  
  M <- length(unique(cluster))
  df=M-1
  return(df)
}


data <- read.csv("mitaData_corrected.csv")

#constructing polynomials
data$x_2 = data$x * data$x
data$y_2 = data$y * data$y
data$xy = data$x * data$y
data$x_3 = data$x * data$x * data$x
data$y_3 = data$y * data$y * data$y
data$x2_y = data$x * data$x * data$y
data$x_y2 = data$x * data$y * data$y

# Polynomials distance to Potosi
data$dpot_2 = data$dpot * data$dpot
data$dpot_3 = data$dpot * data$dpot * data$dpot

#Subset by distance to border 
data_100km <- subset(data, d_bnd<=100)
data_75km <- subset(data, d_bnd<=75)
data_50km <- subset(data, d_bnd<=50)



# Run regressions, 100km
m1_100km <- lm(lhhequiv ~ pothuan_mita + x + y + x_2 + y_2 + xy + x_3 + y_3 + x2_y + x_y2 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_100km)
m1_100km_vcov=get_CL_vcov(m1_100km,data_100km$district)
m1_100km_df=get_CL_df(m1_100km,data_100km$district)
coeftest(m1_100km, m1_100km_vcov, df=m1_100km_df)


# Run regressions, 75km
m1_75km <- lm(lhhequiv ~ pothuan_mita + x + y + x_2 + y_2 + xy + x_3 + y_3 + x2_y + x_y2 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_75km)
m1_75km_vcov=get_CL_vcov(m1_75km,data_75km$district)
m1_75km_df=get_CL_df(m1_75km,data_75km$district)
coeftest(m1_75km, m1_75km_vcov, df=m1_75km_df)

# Run regressions, 50km
m1_50km <- lm(lhhequiv ~ pothuan_mita + x + y + x_2 + y_2 + xy + x_3 + y_3 + x2_y + x_y2 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_50km)
m1_50km_vcov=get_CL_vcov(m1_50km,data_50km$district)
m1_50km_df=get_CL_df(m1_50km,data_50km$district)
coeftest(m1_50km, m1_50km_vcov, df=m1_50km_df)

#100 Km, Potosi
m1_dpot_100km <- lm(lhhequiv ~ pothuan_mita + dpot + dpot_2 + dpot_3 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_100km)
m1_dpot_100km_vcov=get_CL_vcov(m1_dpot_100km,data_100km$district)
m1_dpot_100km_df=get_CL_df(m1_dpot_100km,data_100km$district)
coeftest(m1_dpot_100km, m1_dpot_100km_vcov, df=m1_dpot_100km_df)

#75 Km, Potosi
m1_dpot_75km <- lm(lhhequiv ~ pothuan_mita + dpot + dpot_2 + dpot_3 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_75km)
m1_dpot_75km_vcov=get_CL_vcov(m1_dpot_75km,data_75km$district)
m1_dpot_75km_df=get_CL_df(m1_dpot_75km,data_75km$district)
coeftest(m1_dpot_75km, m1_dpot_75km_vcov, df=m1_dpot_75km_df)

#50 Km, Potosi
m1_dpot_50km <- lm(lhhequiv ~ pothuan_mita + dpot + dpot_2 + dpot_3 + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data=data_50km)
m1_dpot_50km_vcov=get_CL_vcov(m1_dpot_50km,data_50km$district)
m1_dpot_50km_df=get_CL_df(m1_dpot_50km,data_50km$district)
coeftest(m1_dpot_50km, m1_dpot_50km_vcov, df=m1_dpot_50km_df)