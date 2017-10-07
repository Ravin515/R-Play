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

iter <- conn$iterate(query = '{}', field = '{"_id":0, "cmt_id":1, "channel":1, "reply":1, "title":1, "type":1, "time":1, "news_id":1, "source":1, "author":1, "keywords":1, "tags":1, "news_create_time":1, "news_publish_time":1, "url":1, "content":1}')
res <- iter$batch(size = 1e3)
channel <- rbindlist(lapply(res, `[[`, "channel"), use.names = T, fill = T, idcol = "rid")
cmt_id <- rbindlist(lapply(res, `[[`, "cmt_id"), use.names = T, fill = T, idcol = "rid")

get_other <- function(x) {
    x[c("title", "time", "news_id", "source", "author", "keywords", "tags", "news_create_time", "news_publish_time", "url", "content")] %>% as.data.table()
}
other <- lapply(res, get_other) %>% rbindlist(use.names = T, fill = T, idcol = "rid")

get_replycontent <- function(x) {
    x[["reply"]][["reply_content"]]
}
reply <- lapply(res, get_replycontent)

lapply(seq_along(reply), function(i) {is.na(reply[[i]])})

#z <- res[[14]][["reply"]][["reply_content"]]
#rbindlist(reply, use.names = T, fill = T)
z <- lapply(reply, na.omit)


flatlist <- function(mylist) {
    lapply(rapply(mylist, enquote, how = "unlist"), eval)
}

z <- lapply(res, flatlist) %>% rbindlist(use.names = T, fill = T)


get_replynum <- function(x) {
    x[["reply"]][c("replynum", "hotness", "qreply")]
}
replynum <- lapply(res, get_replynum) %>% rbindlist(use.names = T, fill = T, idcol = "rid")




l <- list(a = 1, b = 8)

l[c("a", "b")]