library(mongolite)
# news：包含除了回复正文以外的所有变量 ----
# 使用1）iterate模式读取，2）使用rapply进行展平，速度比传统方式提高很多
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
rm(iter, res, chunk)


# reply：包含 news_id 与 reply 正文 ----
conn <- mongo(collection = 'CrawlerSinaNews', db = 'SinaNews', url = "mongodb://192.168.1.54:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0, "reply.reply_content":1, "news_id":1}')
reply <- data.table()
while (!is.null(res <- iter$batch(size = 1e5))) {
    chunk <- rbindlist(lapply(res, `[[`, "reply"))[["reply_content"]] %>% rbindlist(use.names = T, fill = T)
    reply <- rbindlist(list(reply, chunk), use.names = T, fill = T)
}
rm(iter, res, chunk)



