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
conn <- mongo(collection = 'CrawlerGuba', db = 'test', url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0, "post_id":1, "reply":1}')

set_empty_to_na <- function(x, null.as) {
    # null.as 要么是 NA， 要么是 list()
    if (length(x) == 0) {
        if (is.null(x)) {
            x <- null.as
        }
    }
    x
}

#batch
make_post_reply <- function(batch) {
    reply <-lapply(batch, `[[`, "reply") %>% lapply(set_empty_to_na, null.as = list()) %>% lapply(rbindlist, fill = T, use.names = T) %>% rbindlist(use.names = T, fill = T)
    post <- lapply(batch, `[[`, "post_id") %>% sapply(set_empty_to_na, null.as = NA, USE.NAMES = F)

    if (!is.null(post)) {
        if (nrow(reply) == 0) {
            data.table(post.id = post)
        } else {
            data.table(post.id = post, reply)
        }
    } else {
        data.table()
    }
}

##one
#make_post_reply <- function(one) {
    #reply <- one["reply"] %>% lapply(set_empty_to_na, null.as = list()) %>% lapply(rbindlist, fill = T, use.names = T) %>% rbindlist(use.names = T, fill = T)
    #post <- one$post_id %>% sapply(set_empty_to_na, null.as = NA, USE.NAMES = F)

    #if (!is.null(post)) {
        #if (nrow(reply) == 0) {
            #data.table(post.id = post)
        #} else {
            #data.table(post.id = post, reply)
        #}
    #} else {
        #data.table()
    #}
#}

replys <- data.table()
res <- iter$batch(1e3)
replys <- make_post_reply(res)
while (!is.null(res <- iter$batch())) {
    chunk <- make_post_reply(res)
    replys <- rbindlist(list(chunk, replys), use.names = T, fill = T)
}

replys <- replys[, lapply(.SD, char2utf8)]
rm(iter, res, chunk)
# 使用fwrite写入csv文件
fwrite(replys, file = "replys.csv")


#import Guba UserInfo
conn <- mongo(collection = 'CrawlerGubaUserInfo', db = 'test', url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0}')
flat_list <- function(nest.list) {
    lapply(rapply(nest.list, enquote, how = "unlist"), eval)
}
userinfo <- data.table()
while (!is.null(res <- iter$batch(size = 1e2))) {
    chunk <- lapply(res, flat_list) %>% rbindlist(use.names = T, fill = T)
    userinfo <- rbindlist(list(userinfo, chunk), use.names = T, fill = T)
}
userinfo <- userinfo[, lapply(.SD, char2utf8)]
rm(iter, res, chunk)
fwrite(userinfo, file = "userinfo.csv")



