library(mongolite)
# news：包含除了回复正文以外的所有变量 ----
# 使用1）iterate模式读取，2）使用rapply进行展平，速度比传统方式提高很多
conn <- mongo(collection = 'CrawlerSinaNews', db = 'SinaNews', url = "mongodb://192.168.1.54:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0, "reply.reply_content":0}')
flat_list <- function(nest.list) {
    lapply(rapply(nest.list, enquote, how = "unlist"), eval)
}
news <- data.table()
while (!is.null(res <- iter$batch(size = 1e2))) {
    chunk <- lapply(res, flat_list) %>% rbindlist(use.names = T, fill = T)
    news <- rbindlist(list(news, chunk), use.names = T, fill = T)
}
news <- news[, lapply(.SD, char2utf8)]
rm(iter, res, chunk)
# 使用fwrite写入csv文件
fwrite(news, file = "news.csv")


# reply：包含 news_id 与 reply 正文 ----
conn <- mongo(collection = 'CrawlerSinaNews', db = 'SinaNews', url = "mongodb://192.168.1.54:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0, "reply.reply_content":1, "news_id":1}')
reply <- data.table()
while (!is.null(res <- iter$batch(size = 1e5))) {
    chunk <- rbindlist(lapply(res, `[[`, "reply"))[["reply_content"]] %>% rbindlist(use.names = T, fill = T)
    reply <- rbindlist(list(reply, chunk), use.names = T, fill = T)
}
reply <- reply[, lapply(.SD, char2utf8)]
rm(iter, res, chunk)
# 使用fwrite写入csv文件
fwrite(reply, file = "reply.csv")



news.cj <- news[cmt_id.channel == "cj"]

i <- 1
n <- 10000
while ((i - 1) * n < nrow(news.cj)) {
    fwrite(news.cj[((i - 1) * n):min(i * n, nrow(news.cj))], file = str_c("news.cj.", i, ".csv"))
    i <- i + 1
}
fwrite(news.cj[1:1000], file = "news.cj1.csv")

news.cj.sample <- news.cj[1:1000]
sv(news.cj.sample)
save(news.cj.sample, file = c("news.cj.sample.Rdata"))

