library(data.table)
ZS <- fread("C:/Users/MrStylee/Desktop/Advanced Economic/ZS.csv")
cndalym <- fread("C:/Users/MrStylee/Desktop/Advanced Economic/TRD_Cndalym.csv")
nrrate <- fread("C:/Users/MrStylee/Desktop/Advanced Economic/TRD_Nrrate.csv")
dalyr <- fread("C:/Users/MrStylee/Desktop/Advanced Economic/TRD_Dalyr.csv")

setnames(nrrate, 1, c("Trddt"))
setnames(ZS, 4, c("Deadline"))


dt <- dalyr[nrrate, on = "Trddt", nomatch = 0] # merge two table by Trddt
dt2 <- dt[cndalym, on = "Trddt", nomatch = 0] 
capm <- dt2[ZS, on = "Stkcd", nomatch = 0, allow.cartesian = TRUE] # merge two table allow cartesian
capm <- capm[, rstock := ifelse((as.Date(Trddt) - as.Date(Deadline) <= 90), Dretwd - Nrrdaydt, NA)][, riskpre := ifelse((as.Date(Trddt) - as.Date(Deadline) <= 90), Cdretwdeq - Nrrdaydt, NA)] # calculate the date and filtrate the data
capm <- capm[, dumperspt := ifelse(Perspt>=0.5, 1, 0)] # value a variable that if perspt>=0.5 then give 1
capm <- capm[!is.na(rstock)]

capmcoef <- capm[, as.list(coef(lm(rstock ~ riskpre, .SD))), by = Stkcd]
dalyrmean <- dalyr[, as.list(mean(Dretwd)), by = Stkcd]
setnames(dalyrmean, 2 ,c("dretwdmean"))
capmcoef <- capmcoef[ZS, on = "Stkcd"] # merge two table by Stkcd
capmcoef <- capmcoef[dalyrmean, on = "Stkcd"]

library(dplyr)
capmcoef1 <- capmcoef[, rankperspt := ntile(Perspt, 5)][, rankriskpre := ntile(riskpre, 5)]
order1 <- capmcoef1[, as.list(mean(dretwdmean)), keyby = .(rankperspt, rankriskpre)]

capmcoef2 <- capmcoef[, rankperspt := ntile(Perspt, 3)][, rankriskpre := ntile(riskpre, 3)]
order2 <- capmcoef2[, as.list(mean(dretwdmean)), keyby = .(rankperspt, rankriskpre)]


reg1 <- summary(lm(rstock ~ riskpre + Perspt, data = capm))
reg2 <- summary(lm(rstock ~ riskpre, data = capm))
reg3 <- summary(lm(riskpre ~ Perspt, data = capm))
reg4 <- summary(lm(rstock ~ riskpre + Perspt + dumperspt, data = capm))

#library(broom)
#setwd("C:/Users/MrStylee/Desktop")
#write.table(reg1, "reg1.csv", sep = ",")
#write.table(reg2, "reg2.csv", sep = ",")
#write.table(reg3, "reg3.csv", sep = ",")
#write.table(reg4, "reg4.csv", sep = ",")
