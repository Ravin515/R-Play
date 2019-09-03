library(rvest)
library(data.table)
library(stringr)
# pull request
url <- 'http://car.auto.ifeng.com/'
urlpage <- read_html(url)
# The first layer
brand <- list()
for (i in 1:22) {
    main.brand <- html_nodes(urlpage, xpath = str_c('//div[@class="w1000"]/div[position()=', i+1, ']/dl/dt/a[@class="brand"]')) %>% html_text()
    main.brand.num <- length(main.brand)
    sub.brand.list <- list()
    for (j in 1: main.brand.num) {
        sub.brand <- html_nodes(urlpage, xpath = str_c('//div[@class="w1000"]/div[position()=', i+1, ']/dl[position()=', j, ']/dd/div/a')) %>% html_text()
        sub.brand.num <- length(sub.brand)
        name.list <- list()
        for (k in 1: sub.brand.num) {
            name <- html_nodes(urlpage, xpath = str_c('//div[@class="w1000"]/div[position()=', i+1, ']/dl[position()=', j, ']/dd/ul/li/a')) %>% html_text()
            name.list[[k]] <- data.table(name = name, sub.brand = sub.brand[[k]], main.brand = main.brand[[j]])
        }
        sub.brand.list[[j]]<- rbindlist(name.list)
    }
    brand[[i]] <- rbindlist(sub.brand.list)
}
brand.list <- rbindlist(brand)
rm(list = ls()[-2])
