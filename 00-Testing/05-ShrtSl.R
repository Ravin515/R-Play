library(readtext)
library(data.table)
library(stringr)
library(readstata13)
library(dplyr)

# controllers data
cntrshr <- fread("HLD_Contrshr.txt", header = T, encoding = "UTF-8", fill = T, sep = "\t")
cntrshr <- cntrshr[, - c(3:6)]
cntrshr <- cntrshr[!duplicated(cntrshr)
    ][, Reptdt := as.Date(Reptdt, format = "%Y-%m-%d")
    ][S0701b != 0
    ][, Stkcd := str_pad(Stkcd, 6, "left", "0")
    ][!is.null(S0702b)]
cntrshr[S0701b == "N/A"] <- NA
cntrshr <- cntrshr[!is.na(Stkcd)
    ][order(Stkcd, Reptdt)]
fwrite(cntrshr, "cntrshr.csv")

# shareholder
shrhldr <- fread("HLD_Shareholders.txt", header = T, encoding = "UTF-8", fill = T, sep = "\t")
shrhldr <- shrhldr[, Reptdt := as.Date(Reptdt, "%Y-%m-%d")
    ][S0306a == 1
    ][, Stkcd := str_pad(Stkcd, 6, "left", "0")
    ][, S0306a := NULL
    ][order(Stkcd, Reptdt)]
fwrite(shrhldr, "shrhldr.csv")

# announcements of short-selling list
d <- readtext("*.pdf")
d <- as.data.table(d)

d[, text := str_replace_all(text, "\r\n", "")
    ][, text := str_replace_all(text, " ", "")
    ][, stck := str_extract_all(text, "\\d{7,9}")
    ][, date := str_extract(doc_id, "\\d{4}.\\d{1,2}.\\d{1,2}")
    ][, date := as.Date(date, "%Y.%m.%d")
    ][, ":="(doc_id = NULL, text = NULL)]
shrt.lst <- d[, .(d = unlist(list(stck))), by = date]
shrt.lst[, stck := str_extract(d, "\\d{6}$")
    ][, d := NULL]

a <- fread("subject.csv")
shrt <- melt(a, id.vars = colnames(a)[1:2], measure.vars = colnames(a)[3:38])
setnames(shrt, 1:4, c("stck", "stcknm", "date", "shrtlst"))
shrt[, date := as.Date(date, "%Y/%m/%d")]
shrt[, stck := str_extract(stck, "^\\d{6}")]
shrt.lst <- shrt[shrtlst == "ÊÇ"]
fwrite(shrt, "shrt.csv")

# industry of short-selling enterprises
industry <- fread("industry.csv", encoding = "UTF-8")
industry <- melt(industry, id.vars = colnames(industry)[1:2], measure.vars = colnames(industry)[3:38])
setnames(industry, 1:4, c("stck", "stcknm", "date", "indst"))
industry <- industry[, date := as.Date(date, "%Y/%m/%d")
    ][order(date, stck)]
shrt <- fread("shrt.csv")
shrt[, date := as.Date(date, "%Y-%m-%d")]
shrt.lst <- shrt[industry, on = .(date, stcknm), nomatch = 0]
shrt.lst <- shrt.lst[!is.na(stck)
    ][, stck := NULL
    ][shrtlst == "ÊÇ"
    ][, shrtlst := "Yes"]
fwrite(shrt.lst, "shrt.lst.csv")


shrt.lst <- fread("shrt.lst.csv", encoding = "UTF-8")
hb <- read.dta13("hebing.dta") %>% as.data.table()
hb[, id := str_pad(id, 6, "left", "0")
    ][, date := as.Date(date, "%Y-%m-%d")]

shrt.lst <- shrt.lst[, date := as.Date(date, "%Y-%m-%d")-1
    ][, id := str_extract(i.stck, "\\d{6}")
    ][, shrt.lst := 1L 
    ][, .(date, shrt.lst, id)]

hebing <- shrt.lst[hb, on = .(date, id)]
hebing[is.na(shrt.lst), shrt.lst := 0L
    ][length(shrt.lst == 1L)>0, post := 1L, by = .(id)
    ][,]
a <- hebing[, .(post)]