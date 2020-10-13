rm(list = ls())
census <- read.csv("census80.csv")


summary(census$workedm)
summary(census$weeksm)
summary(census$hispm)
summary(census$ageq2nd)


census$mult_preg_2nd[census$ageq2nd == census$ageq3rd] = 1
census$mult_preg_2nd[census$ageq2nd != census$ageq3rd] = 0
census$mult_preg_2nd[is.na(census$mult_preg_2nd)] = 0
summary(census$mult_preg_2nd)

census$sex_same[census$sex1st == census$sex2nd] = 1
census$sex_same[census$sex1st != census$sex2nd] = 0
summary(census$sex_same)

census$kids3 <- census$numberkids == 3
#Using the dummy workedm as the Y
lab_supply <- lm(workedm ~ kids3 + blackm + hispm + othracem, data=census)
summary(lab_supply)

#Using the amount of weeks worked as the Y
lab_supply <- lm(weeksm ~ kids3 + blackm + hispm + othracem, data=census)
summary(lab_supply)


iv_mult <- lm(kids3 ~ mult_preg_2nd + blackm + hispm + othracem, data=census)
summary(iv_mult)

iv_samesex <- lm(kids3 ~ sex_same + blackm + hispm + othracem, data=census)
summary(iv_samesex)

wald_multpreg <- ivreg(workedm ~ kids3 + blackm + hispm + othracem | mult_preg_2nd + blackm + hispm + othracem, data=census)
summary(wald_multpreg)

wald_samegen <- ivreg(workedm ~ kids3 + blackm + hispm + othracem | sex_same + blackm + hispm + othracem, data=census)
summary(wald_samegen)
