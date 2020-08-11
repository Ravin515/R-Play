library(data.table)
library(stingr)
load("Point1.Rdata")
tag <- point1[is.na(price) | is.nan(price), .(month, permno)] %>% unique()
point <- tag[, tag := 1
    ][point1, on = .(month, permno)]
rm(point1, tag)
point1 <- point[is.na(tag)]
rm(point)
point1 <- point1[, tag := NULL
    ][, ret := price[.N] / price[1] - 1, by = .(permno, month)
    ][!(price == 0 | is.infinite(ret) | is.nan(ret))
    ][, .SD[.N], by = .(month, permno)
    ][, tag := ifelse(cap >= decile10 * 1e6, "top", "bottom")
    ][, max_ret := cummax(ret), by = .(permno)
    ][order(tag, month, max_ret)
    ][, decile10 := NULL]

decile <- point1[, .(decile = quantile(max_ret, probs = seq(0.1, 1, 0.1)), flag = paste0("decile", 1:10)), by = .(tag, month)
    ] %>% dcast(month + tag ~ flag, value.var = "decile")

point1 <- decile[point1, on = .(month, tag)]
key1 <- point1[, score := rowSums(point1[, 3:12] > max_ret)
    ][score != 0, .(.N), by = .(score, tag, month)
    ][, .(avr_num = mean(N)), by = .(score, tag)]
sv(point1)