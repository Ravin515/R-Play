dir <- "//211-PC/R-Play/02-MergeHedgeFund/HedgeFundData/DZH/"


test <- read_excel(str_c(dir, "集合理财-info-开始-2015.xlsx"))

test <-  fread(str_c(dir, "集合理财-info-开始-2015.csv"))