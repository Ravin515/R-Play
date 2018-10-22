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


a <- fread('C:/Users/MrStylee/Desktop/gupiao.csv', header = T, encoding = "UTF-8")
setnames(a, 4:5, c("start", "end"))
a[, start := gsub('([0-9]{4})([0-9]{2})([0-9{2}])', '\\1-\\2-\\3', start)
    ][, end := gsub('([0-9]{4})([0-9]{2})([0-9{2}])', '\\1-\\2-\\3', end)
    ][, start := as.Date(start, "%Y-%m-%d")
    ][, end := as.Date(end, "%Y-%m-%d")
    ][, startquarter := as.Date(cut(start, "quarter"))
    ][, endquarter := as.Date(cut(end, "quarter"))]
    #][, startquarter := lubridate::quarter(start, with_year = TRUE)
    #][, endquarter := lubridate::quarter(end, with_year = TRUE)]


int <- a[a[, .(date = seq(min(startquarter), max(endquarter), by = "quarter")), keyby = .(基金经理, 证券代码)], on = .(基金经理, 证券代码), nomatch = NA]

a <- fread('C:/Users/MrStylee/Desktop/shida.csv', header = T, encoding = "UTF-8")