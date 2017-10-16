library(readxl)
# DZH数据所在目录
dir <- "//211-PC/R-Play/02-MergeHedgeFund/HedgeFundData/DZH/"
# Before 2015 ----
dir.before.2015 <- paste0(dir, "DZH-开始-2015.xlsx")
jhlc.info.before.2015 <- read_excel(dir.before.2015, sheet = "集合理财-info-至2015")
jhlc.nv.2015 <- read_excel(dir.before.2015, sheet = "集合理财-nv-2015")
jhlc.nv.before.2014 <- read_excel(dir.before.2015, sheet = "集合理财-nv-开始-2014")

ygsm.info.before.2015 <- read_excel(dir.before.2015, sheet = "阳光私募-info-至2015")
ygsm.nv.before.2015 <- read_excel(dir.before.2015, sheet = "阳光私募-nv-至2015")

# After 2015 ----
dir.after.2015 <- paste0(dir, "DZH-2016-2017.xlsx")
jhlc.info.after.2015 <- read_excel(dir.after.2015, sheet = "集合理财-info-2016-2017")
jhlc.nv.2016 <- read_excel(dir.after.2015, sheet = "集合理财-nv-2016")
jhlc.nv.2017 <- read_excel(dir.after.2015, sheet = "集合理财-nv-2017")

ygsm.info.after.2015 <- read_excel(dir.after.2015, sheet = "阳光私募-info-2016-2017")
ygsm.nv.after.2015 <- read_excel(dir.after.2015, sheet = "阳光私募-nv-2016-2017")


# 合并以上文件，最后生成两个数据集：dzh.info, dzh.nv ----
# 合并info
f.dzh.info <- rbindlist(list(jhlc.info.after.2015, jhlc.info.before.2015, ygsm.info.after.2015, ygsm.info.before.2015), use.names = T, fill = T) %>% unique(by = c("fund.id"))
setnames(f.dzh.info, names(f.dzh.info), str_c("dzh.", names(f.dzh.info)))
setnames(f.dzh.info, names(f.dzh.info), str_replace_all(names(f.dzh.info), "_", "."))
sv(f.dzh.info)

# 合并nv
f.dzh.nv <- rbindlist(list(jhlc.nv.2015, jhlc.nv.2016, jhlc.nv.2017, jhlc.nv.before.2014, ygsm.nv.before.2015, ygsm.nv.after.2015), use.names = T, fill = T) %>% unique(by = c("fund.id", "date"))
setnames(f.dzh.nv, names(f.dzh.nv), str_c("dzh.", names(f.dzh.nv)))
setnames(f.dzh.nv, names(f.dzh.nv), str_replace_all(names(f.dzh.nv), "_", "."))
sv(f.dzh.nv)

