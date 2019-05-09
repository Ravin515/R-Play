data <- data.table(name = list.files(pattern = "*.csv"))
data <- data[, .SD[-.N]]
data[, csv := lapply(name, function(x) {
    fread(x, sep = ',', fill = T, encoding = "UTF-8", na.strings = "") %>% setnames(1, "项目")
})]
d <- rbindlist(list(data[1:2, csv := lapply(csv, setnames, 3, "2018年度")][1:2], data[3:6, csv := lapply(csv, setnames, 2, "2018年度")][3:6]))

flat.data <- data[, rbindlist(.SD[['csv']], fill = T)]
flat.data <- flat.data[, c(1, 3, 11, 12, 14, 15, 16)
    ][, setnames(.SD, 3:7, c("增减额", "增减率", "增减变动", "增减变动率", "差异"))
    ][, `项目` := as.character(`项目`) %>% str_trim()]
a <- flat.data[is.na(flat.data)]
library(xlsx)
write.xlsx(flat.data, "data.xlsx")
