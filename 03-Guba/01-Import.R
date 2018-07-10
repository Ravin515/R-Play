library(mongolite)
#import Gubaposts data without reply
conn <- mongo(collection = 'CrawlerGuba', db = 'test', url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0, "guba_url":0, "reply":0}')
flat_list <- function(nest.list) {
    lapply(rapply(nest.list, enquote, how = "unlist"), eval)
}
#copy all the unlisted elements and make the list replaced by the enquote
posts <- data.table()
while (!is.null(res <- iter$batch(size = 1e2))) {
    chunk <- lapply(res, flat_list) %>% rbindlist(use.names = T, fill = T)
    posts <- rbindlist(list(posts, chunk), use.names = T, fill = T)
}
posts <- posts[, lapply(.SD, char2utf8)]
rm(iter, res, chunk)
fwrite(posts, file = "posts.csv")


#import Gubapost reply

conn <- mongo(collection = 'CrawlerGuba', db = 'test', url = 'mongodb://localhost:27017')
iter <- conn$iterate(query = '{}', field = '{"_id":0, "reply":1, "post_id":1}')

flat_reply <- function(ele) {
    if (!identical(ele[['reply']], list())) {
        reply <- ele[['reply']]
        if (!is.null(reply)) {
            reply <- rbindlist(reply, use.names = T, fill = T)[, ':='(tag = 1L)]
            ele[['reply']] <- NULL
            post.id <- ele
            post.id <- rbindlist(list(post.id), use.names = T, fill = T)[, ':='(tag = 1L)]
            pool <- reply[post.id, on = .(tag), nomatch = NA]
        }
    }
}

reply <- data.table()
while (!is.null(res <- iter$batch(size = 1e4))) {
    #chunk <- z[!(lapply(res, flat_reply) %>% lapply(is.null) %>% unlist())] %>% rbindlist(use.names = T, fill = T)
    chunk <- lapply(res,flat_reply) %>% rbindlist(use.names = T, fill = T)
    reply <- rbindlist(list(chunk, reply), use.names = T, fill = T)
}

rm(iter, res, chunk)
# 使用fwrite写入csv文件
fwrite(reply, file = "replys.csv")


#import Guba UserInfo
conn <- mongo(collection = 'CrawlerGubaUserInfo', db = 'test', url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0}')

user.info <- data.table()
while (!is.null(res <- iter$batch(size = 1000))) {
    chunk <- rbindlist(res, use.names = T, fill = T)
    user.info <- rbindlist(list(user.info, chunk), use.names = T, fill = T)
}




