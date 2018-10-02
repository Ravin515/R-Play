library(data.table)
S1 <- fread("C:/Users/MrStylee/Desktop/Advanced Economic/CAPM.csv")
#dt[is.na(a) & is.na(b), ":="(a = 0)]
#dt[is.na(a), a := 0]
S1 <- S1[, Mretwd := ifelse(is.na(Mretwd), mean(Mretwd, na.rm = T), Mretwd), by = Stkcd] # fill the mean of Mretwd into Mretwd group by Stkcd
S1 <- S1[, Mretwd := ifelse(is.na(Mretwd), mean(Mretwd, na.rm = T), Mretwd)] # fill the mean of Mretwd of whole sample into Mretwd
S1 <- S1[, RStock := ifelse(is.na(RStock), Mretwd - Nrrmtdt ,RStock)]
CAPMcoef <- S1[, as.list(coef(lm(RStock ~ RiskPre, .SD))), by = Stkcd] # Regression by grouping and exporting betas
AverageR <- S1[, as.list(mean(RStock)), by = Stkcd] # Average by grouping and exporting data
setnames(AverageR, 2, c("AverageR")) # Rename the column
CAPM2 <- merge(CAPMcoef, AverageR, by = "Stkcd")
#CAPM2 <- na.omit(CAPM2)# row delete
    

S2 <- fread("C:/Users/MrStylee/Desktop/Advanced Economic/Given.csv")
S2 <- S2[is.na(Perspt), Perspt := 0] # fill the zero into Perspt where is NA

setnames(Given, 1, c("Stkcd"))

Total <- merge(CAPM2,Given,by="Stkcd")


Total1 <- Total[order(Total$Perspt,Total$RiskPre1),]

library(dplyr)
Rank.Perspt <- ntile(Total1$Perspt,10)
Rank.RiskPre1 <- ntile(Total1$RiskPre1,10)
Total2 <- data.table(Total1,Rank.Perspt,Rank.RiskPre1)
Beta.R1 <- Total2[, as.list(mean(AverageR)), keyby = Rank.Perspt]
Beta.R2 <- Total2[, as.list(mean(AverageR)), keyby = .(Rank.Perspt, Rank.RiskPre1)]
Beta.R3 <- Beta.R2[,as.list(mean(V1)), keyby = Rank.RiskPre1]
names(Beta.R3)[2] <- c("mean.r")
#setwd("D:\\")
#write.table(Beta.R1,"RankPerspt.csv",sep=",")
#write.table(Beta.R3,"RankRiskPre.csv",sep=",")


Mretwd1 <- S1[,as.list(mean(Mretwd)),by=Stkcd]
reg.capm <- merge(CAPMcoef,Mretwd1,by="Stkcd")
names(reg.capm)[4] <- c("Mretwd")

capm.sec1 <- summary(lm(Mretwd ~ RiskPre1,data=reg.capm))

S2 <- S2[!duplicated(S2$Stkcd), ]

reg.capm <- merge(S2,reg.capm,by="Stkcd")


gc(rm(Perspt))
gc(rm(Mretwd))
attach(reg.capm)
Perspt[is.na(Perspt)]<-0
Mretwd[is.na(Mretwd)]<-mean(Mretwd,na.rm=T)
reg.capm<-data.table(reg.capm$Stkcd,reg.capm$RiskPre1,Perspt,Mretwd)
names(reg.capm)[1:2]<-c("Stkcd","RiskPre1")
capm.sec2 <- summary(lm(Mretwd ~ RiskPre1 + Perspt, data=reg.capm))
capm.sec3 <- summary(lm(RiskPre1 ~ Perspt, data=reg.capm))
library(broom)
tidy(capm.sec1)
tidy(capm.sec2)
tidy(capm.sec3)
