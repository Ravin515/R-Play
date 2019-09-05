library(bit64)
library(data.table)
library(stringr)
library(pryr)

# 股票日均收益率导入
data <- data.table(year = list.files(pattern = "*.csv"))
data[, csv := lapply(year, function(x) {
    fread(x, sep = '\t', fill = T, encoding = "UTF-8", integer64 = "integer64", header = T)
})]
stkcd.ret.daily <- data[, rbindlist(.SD[['csv']], fill = T, idcol = "year")]
stkcd.ret.daily[, Trddt := as.Date(Trddt, "%Y-%m-%d")]
rm(data)

# 因子数据导入
factors <- fread("C:/Users/Ray Stylee/source/repos/R-Play/00-Testing/ThreeFactors/STK_MKT_ThrfacDay.csv", header = T)
factors <- factors[, TradingDate := as.Date(TradingDate, "%Y-%m-%d")
    ][TradingDate >= as.Date("2013-12-31", "%Y-%m-%d") 
    ][MarkettypeID == "P9706"]
setnames(factors, "TradingDate", "Trddt")

# 市场收益率导入
mrkt.ret.daily <- fread("C:/Users/Ray Stylee/source/repos/R-Play/00-Testing/ThreeFactors/TRD_Cndalym.csv", header = T)
mrkt.ret.daily <- mrkt.ret.daily[, Trddt := as.Date(Trddt, "%Y-%m-%d")
    ][Trddt >= as.Date("2013-12-31", "%Y-%m-%d")
    ][Markettype == 5]

# 三张表格合并
mrg <- stkcd.ret.daily[factors, on = .(Trddt)]
mrg <- mrkt.ret.daily[mrg, on = .(Trddt)]

# 导入事件
illegal <- fread("C:/Users/Ray Stylee/source/repos/R-Play/00-Testing/ThreeFactors/违规情况.csv", header = T, encoding = "UTF-8")
broker <- fread("C:/Users/Ray Stylee/source/repos/R-Play/00-Testing/ThreeFactors/券商流通数据.csv", header = T, encoding = "UTF-8")

time.flag <- illegal[broker, on = .(dealershort, ystart)]
time.flag <- time.flag[!is.na(a) | !is.na(b)
    ][, 1:12
    ][!is.na(value)]

setnames(time.flag, "stock", "Stkcd")
time.flag <- time.flag[, Stkcd := str_extract(Stkcd, "\\d*") %>% as.integer()
    ][, .(dealershort, time, a, b, Stkcd, amount, value, proportion)]

unique.flag <- time.flag[, .(time = min(time)), keyby = .(Stkcd)]
flag <- time.flag[unique.flag, on = .(Stkcd, time), nomatch = 0]
flag <- flag[order(dealershort, Stkcd, time)
    ][, .SD[1], by = .(Stkcd, time)
    ]
rm(illegal, broker, unique.flag, time.flag)

# 合并四张表
mrg.list <- flag[mrg, on = .(Stkcd)]
mrg.list <- mrg.list[, event.flg := ifelse(time == Trddt, 1, 0)
    ][!is.na(event.flg)
    ][order(Stkcd, Trddt)]
fwrite(mrg.list, "mrg.list.csv")

# CAR
c1 <- 1
c2 <- 1
m1 <- 10
m2 <- 5
do_car <- function(n, Dretwd, Cdretwdos, Trddt) {
    stopifnot(m1 > m2)
    if (n - m1 < 0) {
        cat("n =", n, "is too small \n")
    } else if (n + c2 > length(Dretwd)) {
        cat("n =", n, "is too large \n")
    } else {
        i1 <- max(1, n - m1)
        i2 <- n - m2
        i3 <- n - c1
        i4 <- n + c2
        r.model <- Dretwd[i1:i2]
        rm.model <- Cdretwdos[i1:i2]
        r.car <- Dretwd[i3:i4]
        rm.car <- Cdretwdos[i3:i4]
        model <- lm(r.model ~ I(r.model - rm.model))
        coef <- coef(model)
        ars <- r.car - predict(model, list(r.model = r.car, rm.model = rm.car))
        list(date = Trddt[n], coef = list(coef), ars = list(ars))
    }
}

car <- mrg.list[, {
    ns <- which(event.flg == 1);
    lapply(ns, partial(do_car, Dretwd = Dretwd, Cdretwdos = Cdretwdos, Trddt = Trddt)) %>% rbindlist()
},
    by = Stkcd]