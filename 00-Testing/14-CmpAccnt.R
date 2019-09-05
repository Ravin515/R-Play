library(readxl)
library(data.table)
library(stringr)
library(zoo)

# 预算表清洗
a1 <- read_excel("C:/Users/Ray Stylee/Desktop/incomecost.xlsx", sheet = "预算表") %>% as.data.table()
a1[, colnames(a1)[1:5] := lapply(.SD, na.locf, na.rm = F), .SDcol = 1:5]
setnames(a1, str_c("a", 1:63))
a1 <- a1[str_count(a5) < 3 | is.na(a5)]
a1[, colnames(a1)[6] := lapply(.SD, na.locf, na.rm = F), .SDcol = 6]
setnames(a1, 1:7, c("gov_cat_num_1st", "gov_cat_num_2nd", "gov_cat_text", "deprt_cat_num_1st", "deprt_cat_num_2nd", "deprt_cat_text_1st", "deprt_cat_text_2nd"))
d1 <- a1[, .SD[2:5], .SDcol = 8:63] %>% transpose()
d1[, colnames(d1)[1:3] := lapply(.SD, na.locf, na.rm = F), .SDcol = 1:3]
d1 <- d1 %>% sapply(str_replace_na) %>% as.data.table()
d1[51:56, 2] <- NA
index1 <- d1 %>% sapply(str_replace_na) %>% as.data.table() %>% transpose() %>% sapply(str_c, collapse = ",") %>% unlist() %>% as.vector()
a1 <- setnames(a1, 8:63, index1)[-(1:5)]
a1 <- melt(a1, id.vars = colnames(a1[, 1:7]), measure.vars = colnames(a1[, 8:63]), value.name = "money")
a1[, money := ifelse(is.na(money), 0, money)
    ][, money := round(as.numeric(money), 2)]

# 收入表清洗
a2 <- read_excel("C:/Users/Ray Stylee/Desktop/incomecost.xlsx", sheet = "收入数") %>% as.data.table()
a2 <- a2[-1]
b2 <- a2[, .(`功能分类`, `部门经济分类`, `指标总金额`)] %>% setnames(c("gnfl", "bmjjfl", "je"))
b2[, tag := "收入"
    ][, c("gnfl_num", "gnfl_text") := tstrsplit(gnfl, " ", fixed = T)
    ][, c("bmjjfl_num", "bmjjfl_text") := tstrsplit(bmjjfl, " ", fixed = T)
    ][, gnflje := sum(je), keyby = .(gnfl_text)]
d2 <- unique(b2[, .(gnflje, tag, gnfl, gnfl_text)])

# 支出表清洗
a3 <- read_excel("C:/Users/Ray Stylee/Desktop/incomecost.xlsx", sheet = "支出表") %>% as.data.table()
index3 <- a3[, .SD[6:7]] %>% sapply(str_replace_na) %>% as.data.table() %>% sapply(str_c, collapse = "") %>% unlist() %>% as.vector() %>% str_replace_all("NA", "")
a3 <- setnames(a3, index3)[-(1:7)]
a3[, `支出功能分类` := na.locf(`支出功能分类`, na.rm = F)]
b3 <- a3[, .(`支出功能分类`, `辅助项：[支出经济分类]`, `期末余额借方`)] %>% setnames(c("gnfl", "bmjjfl", "je"))
b3[, tag := "支出"
    ][, c("gnfl_num", "gnfl_text") := tstrsplit(gnfl, "_", fixed = T)
    ][, c("bmjjfl_num", "bmjjfl_text") := tstrsplit(bmjjfl, "_", fixed = T)
    ][, gnflje := sum(as.numeric(je)), keyby = .(gnfl_text)]
d3 <- unique(b3[, .(gnflje, tag, gnfl, gnfl_text)])

# 支出表与收入表合并画图
complie <- rbindlist(list(d2, d3), use.names = T, fill = T)
complie[, lab := round(gnflje/10000, 2)]
options(scipen = 200)
g <- ggplot(complie, aes(x = gnfl_text, y = lab, fill = tag)) +
    geom_bar(stat = 'identity', position = "dodge") +
    geom_text(aes(label = lab), vjust = -1, position = position_dodge(.9), size = 3) +
    labs(title = "统计表", x = "", y = "金额(万元)") +
    theme_classic() +
    theme(axis.text = element_text(size = 15)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    #scale_fill_discrete(name = "") +
    scale_fill_manual(name = "", values = c("black", "grey"))
