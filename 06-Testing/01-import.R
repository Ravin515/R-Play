library(readstata13)
info <- read.dta13("C:/Users/MrStylee/Desktop/panelid_1998-2007.3.dta")
info <- as.data.table(info)
library(lfe)
est <- info[, felm(lwage ~ gdp | id_in_panel + procde)]
summary(est)
fe <- getfe(est)

iprt <- fread("C:/Users/MrStylee/Desktop/意大利出口美国数据.csv", header = T)
a<- iprt[, grp := str_extract(code, "\\d{2,2}")
    ][, ":="(sumc = sum(china), sumi = sum(italy)), keyby = .(grp)
    ][, ":="(surc = china / sumc, suri = italy / sumi), keyby = .(grp)
    ][, minsur := min(surc, suri), keyby = .(code)
    ][, summin := sum(minsur), keyby = .(grp)]