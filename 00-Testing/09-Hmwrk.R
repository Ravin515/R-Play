library(data.table)
library(magrittr)
library(stringr)
stck <- fread("沪深300股票数据new.txt", encoding = "UTF-8")
# Q1&Q2
stck[, close.lag := shift(close, type = "lag"), by = .(code)
    ][, day.ret := log(close / close.lag)]
#Q3
stck.mg <- stck[sec_name != "上证综指"
    ][stck[sec_name == "上证综指", .(datetime, month, sz.day.ret = day.ret)
    ], on = .(datetime, month)]
month.var <- stck.mg[!is.na(day.ret), .(beta = coef(lm(sz.day.ret ~ day.ret))[2], ma.ev = mean(ev), month.ret = sum(day.ret), ma.pb = mean(pb), sum.turn = sum(turn)), keyby = .(code, month)]
rm(stck.mg)
#Q4
reg.coef1 <- month.var[month == "2018-11", .(beta = coef(lm(month.ret ~ beta + log(ma.ev) + ma.pb + sum.turn))[-1], adj.R2 = lm(month.ret ~ beta + log(ma.ev) + ma.pb + sum.turn) %>% summary() %>% list() %>% lapply(`[[`, "adj.r.squared") %>% unlist(), var.names = colnames(month.var[, - c(1, 2, 5)]))] %>% dcast(adj.R2 ~ var.names, value.var = "beta")
#Q5
reg.coef2 <- month.var[, .(beta = coef(lm(month.ret ~ beta + log(ma.ev) + ma.pb + sum.turn))[-1], adj.R2 = lm(month.ret ~ beta + log(ma.ev) + ma.pb + sum.turn) %>% summary() %>% list() %>% lapply(`[[`, "adj.r.squared") %>% unlist(), var.names = str_c("beta", c(1:4))), keyby = .(month)] %>% dcast(month + adj.R2~ var.names, value.var = "beta")
#Q6
rst.test <- reg.coef2[, str_c(colnames(reg.coef2[, 2:6]), "_m") := lapply(.SD[, 2:6], mean)
    ][, str_c(colnames(reg.coef2[, 2:6]), "_t") := lapply(.SD[, 2:6], t.test) %>% lapply(`[[`, "statistic")
    ][, lapply(.SD[, 7:16], unique)]

