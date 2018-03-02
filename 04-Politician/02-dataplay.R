anhui <- fread("C:/Users/MrStylee/Desktop/data/安徽.csv")
guangdong <- fread("C:/Users/MrStylee/Desktop/data/广东.csv")
guangxi <- fread("C:/Users/MrStylee/Desktop/data/广西.csv")
henan <- fread("C:/Users/MrStylee/Desktop/data/河南.csv")
hubei <- fread("C:/Users/MrStylee/Desktop/data/湖北.csv")
jiangsu <- fread("C:/Users/MrStylee/Desktop/data/江苏.csv")
jiangxi <- fread("C:/Users/MrStylee/Desktop/data/江西.csv")
shandong <- fread("C:/Users/MrStylee/Desktop/data/山东.csv")
shan1xi <- fread("C:/Users/MrStylee/Desktop/data/山西.csv")
shan3xi <- fread("C:/Users/MrStylee/Desktop/data/陕西.csv")
sichuan <- fread("C:/Users/MrStylee/Desktop/data/四川.csv")
yunnan <- fread("C:/Users/MrStylee/Desktop/data/云南.csv")
zhejiang <- fread("C:/Users/MrStylee/Desktop/data/浙江.csv")
chongqing <- fread("C:/Users/MrStylee/Desktop/data/重庆.csv")

anhui[, province := 1]
guangdong[, province := 2]
guangxi[, province := 3]
henan[, province := 4]
hubei[, province := 5]
jiangsu[, province :=6]
jiangxi[, province := 7]
shandong[, province := 8]
shan1xi[, province := 9]
shan3xi[, province := 10]
sichuan[, province := 11]
yunnan[, province := 12]
zhejiang[, province := 13]
chongqing[, province := 14]

total <- rbind(anhui, chongqing, guangdong, guangxi, henan, hubei, jiangsu, jiangxi, shan1xi, shan3xi, shandong, sichuan, yunnan, zhejiang)

total[is.na(total)] <- 0

fwrite(total, file = "total.csv")

numeric <- c(1, 2.5, 4.5)
names(numeric) <- c("point")
attributes(numeric)