library(data.table)
library(stringr)
library(lubridate)
library(dplyr)

fama <- fread("FamaXiao.csv", encoding = "UTF-8")
setnames(fama, 2, "date")
fama <- fama[, date := str_c(date, "-01") %>% as.Date("%Y-%m-%d")
    ][, Stkcd := str_pad(Stkcd, 6, "left", "0")
    ][, tag := year(date)%>% as.double(), keyby = Stkcd
    ][order(date, Stkcd), tag := tag - 1995
    ][!is.na(rt) & !is.na(HML) & !is.na(SMB) & !is.na(MOM) & !is.na(EMR)]

reg.roll <- list()
for (i in 5:20) {
    reg.roll[[i]] <- fama[tag >= i - 4 & tag <= i, {
        I <- lm(rt ~ HML + SMB + MOM + EMR) %>% coef() %>% c(tag = tag[i]) %>% as.list()
    },
    keyby = .(Stkcd)] 
    rbindlist(reg.roll)
}

roll <- data.table(year = 1:20, reg.roll)[5:20]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = year
    ] %>% na.omit()
reg.cof[, ':='(year = year + 1995, tag = NULL)]

fwrite(reg.cof, 'reg.cof.csv')