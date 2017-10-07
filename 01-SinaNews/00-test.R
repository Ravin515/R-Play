library(mongolite)
conn <- mongo(collection = 'CrawlerSinaNews', db = 'SinaNews', url = "mongodb://192.168.1.54:27017")

# bach size = 1e7
system.time({
    iter <- conn$iterate(query = '{}', field = '{"_id":0, "holdings":0, "error_message":0, "error_status":0, "created_at":0, "updated_at":0, "prev_bebalancing_id":0, "new_buy_count":0, "diff":0, "exe_strategy":0}')
    r.cube.rb <- data.table()
    while (!is.null(res <- iter$batch(size = 1e7))) {
        chunk <- rbindlist(res, use.names = T, fill = T)
        r.cube.rb <- rbindlist(list(r.cube.ret, chunk), fill = T, use.names = T)
    }
})



#----------------
library(mongolite)
conn <- mongo(collection = 'CrawlerSinaNews', db = 'SinaNews', url = "mongodb://192.168.1.54:27017")

iter <- conn$iterate(query = '{}', field = '{"_id":0, "reply.reply_content":0}')
flat_list <- function(nest.list) {
    lapply(rapply(nest.list, enquote, how = "unlist"), eval)
}
news <- data.table()
while (!is.null(res <- iter$batch(size = 1e5))) {
    chunk <- lapply(res, flat_list) %>% rbindlist(use.names = T, fill = T)
    news <- rbindlist(list(news, chunk), use.names = T, fill = T)
}



l <- list(a = 1, b = list(c = 1, d = 4))
rapply(l, list, how = "unlist")
lapply(rapply(l, enquote, how = "unlist"), eval) %>% setDT()