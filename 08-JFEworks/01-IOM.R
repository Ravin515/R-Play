library(data.table)
fama <- fread("F-F_Research_Data_5_Factors_2x3_daily.csv", encoding = "UTF-8")
fama[, setnames(fama, 1, "date")
    ][, date := as.Date(as.character(date), "%Y%m%d")
    ][, month := format(date, "%Y-%m")]

mebrk <- fread("ME_Breakpoints.csv", encoding = "UTF-8")
mebrk[, setnames(mebrk, 1, "month")
    ][ , month := format(as.Date(paste0(month, "01"), "%Y%m%d"), "%Y-%m")
    ][, setnames(mebrk, 3:22, paste0("decile", 1:20))
    ]
mebrk <- mebrk[month >= "1990-01", .SD, .SDcol = c("decile10", "month")]

load("assignment_data18.RData")
setDT(data)[, month := format(date, "%Y-%m")]
point1 <- mebrk[data, on = "month"]
sv(point1) # `sv` is a function that I programed for Rdata importing