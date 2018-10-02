a <- fread("C:/Users/Ray Stylee/Desktop/original data.csv", header = T, encoding = "UTF-8", na.string = "")
a <- a[order(`Pyrolysis T`)]