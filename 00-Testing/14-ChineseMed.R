library(readtext)
library(data.table)
library(stringr)
library(zoo)

prscrp <- fread("prescription.csv", header = F, encoding = "UTF-8") %>% setnames(1:3, c("name", "purpose", "content"))
prscrp <- prscrp[, .(mtrls = unlist(str_split(content, " |）"))), by = .(name, purpose, content)]
prscrp <- prscrp[mtrls != ""]
prscrp[, comment := str_extract(mtrls, "（.+")
    ][, comment := str_replace_all(comment, "（", "")
    ][, mtrls := str_replace_all(mtrls, "（.+", "")]

prscrp[, tag := str_detect(comment, "各")
    ][, tag2 := str_detect(comment, "一|二|三|四|五|六|七|八|九|十")]
prscrp[tag==F & tag2 == F, comment := NA]
prscrp[, comment := na.locf(comment, fromLast = T)]
prscrp <- prscrp[, .(comment = unlist(str_split(comment, "，"))), by = .(name, purpose, content, mtrls)]

prscrp[, tag2 := str_detect(comment, "一|二|三|四|五|六|七|八|九|十")]
prscrp <- prscrp[tag2 == T]
prscrp[, scales := str_replace_all(comment, "以|上|各", "")
    ][, ':='(comment = NULL, tag2 = NULL)]
fwrite(prscrp, "prscrp.csv")