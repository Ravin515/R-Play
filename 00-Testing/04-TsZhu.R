library(data.table)
library(stringr)
library(forecast)
library(magrittr)

sample <- fread("fxishrvmonth.csv", fill = T, na.string = "", encoding = "UTF-8")
sample[nchar(date) > 6, date := str_replace(date, "/", "-")
    ][nchar(date) < 7, date := str_replace(date, "/", "-0")
    ][, date := str_c(date, "-01")
    ][, date := as.Date(date, format = '%Y-%m-%d')]

fit <- vector()
for (i in 81:160) {
    fitt <- sample[, myts := ts(rvfxi, start = c(2005, 3), end = c(2018, 6), frequency = 12)
    ][n < i + 1 & n > i - 81, arima(rvfxi, order = c(1, 0, 0)) %>% fitted()][81]
    fit[i] <- fitt
}

fit <- fit %>% as.data.table() %>% setnames(".", "fitted.rvfxi")
fit[, n := sequence(160)]

int <- fit[sample, on = "n", nomatch = NA]

ros <- int[, dev := (rvfxi - fitted.rvfxi) ^ 2
    ][!is.na(dev), sum.dev := sum(dev)
    ][, sum.mean := cummean(rvfxi)
    ][!is.na(dev), dev.dnt := sum((rvfxi - sum.mean) ^ 2)
    ][, 1 - (sum.dev / dev.dnt)]