library(data.table)
library(dplyr)
library(stringr)
# a <- list.files(pattern = "*.csv")
data <- data.table(year = list.files(pattern = "*.csv"))
data[,  csv := lapply(year, function(x) {
    fread(x, sep = ',', fill = T, encoding = "UTF-8", na.strings = "", integer64 = "integer64")
})]
flat.data <- data[, rbindlist(.SD[['csv']], fill = T, idcol = "year")
    ][, year := year + 2010]
fwrite(flat.data, "2011-2013.csv")

library(RODBC)
mydb <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=D:/code/r-play/07-indstrinc/2003.mdb")
sqlTables(mydb, tableName = "qy03")
res <- sqlFetch(mydb, "qy03") %>% as.data.table()

library(DBI)