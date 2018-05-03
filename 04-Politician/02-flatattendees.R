library(mongolite)
conn <- mongo(collection = 'CrawlerVippear', db = 'test', url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0}')
flat_list <- function(nest.list) {
    lapply(rapply(nest.list, enquote, how = "unlist"), eval)
}

vippear <- data.table()

while (!is.null(res <- iter$batch(size = 1e2))) {
    chunk <- lapply(res, flat_list) %>% rbindlist(use.names = T, fill = T)
    vippear <- rbindlist(list(vippear, chunk), use.names = T, fill = T)
}

vippear <- vippear[, lapply(.SD, char2utf8)]

z <- copy(vippear)[, .(new = unlist(str_split(attendees, ','))), keyby = .(name_pinyin, activity, date, topics)]