politician <- fread("C:/Users/MrStylee/Desktop/Politician.csv", header = TRUE, encoding = "UTF-8")
politician <- data.table(read.table("C:/Users/MrStylee/Desktop/Politician.csv", header = T, sep = ",", fill = T))

mayor <- fread("C:/Users/MrStylee/Desktop/市长名单.csv", header = TRUE, encoding = "UTF-8", na.strings = "")
setnames(mayor, c(7), "name")
mayor <- mayor[!is.na(name)
    ][, .(name)]
fwrite(mayor, "mayor.csv")

scrt <- fread("C:/Users/MrStylee/Desktop/市委书记名单.csv", header = T, encoding = "UTF-8", na.string = "")
setnames(scrt, c(5), "name")
scrt <- scrt[!is.na(name)
    ][, .(name)]
fwrite(scrt, "scrt.csv")

mayor <- fread("mayor.csv", header = T)
scrt <- fread("scrt.csv", header = T)
mayor.info <- data.table(read.table("mayor_info.csv", header = T, sep = ",", fill = T, na.strings = ""))
srct.info <- data.table(read.table("scrt_info.csv", header = T, sep = ",", fill = T, na.strings = ""))
mayor.num <- mayor.info[, unique(name)]
srct.num <- srct.info[, unique(name)]

