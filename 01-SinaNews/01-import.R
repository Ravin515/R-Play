library(mongolite)
conn <- mongo(collection = 'CrawlerSinaNews', db = 'SinaNews')

# 获得所有news_id的唯一值
system.time({
news_id <- conn$aggregate(pipeline = '[
    {"$group":{"_id":"$news_id", "n":{"$sum":1}}}
]')
})
setnames(setDT(news_id), names(news_id), c("nid", "n"))
nids <- news_id$nid

# 按照nid逐个读入
n <- 1e1
N <- as.integer(length(nids) / n + 1)
r.news <- data.table()
for (i in (1:1)) {
    print(sprintf('i:%s', i))
    start <- (i - 1) * n + 1
    end <- min(c(i * n, length(nids)))
    l <- list()
    for (j in start:end) {
        nid <- nids[j]
        query <- sprintf('{"news_id": "%s"}', nid)
        field <- '{"_id":0, "pic":0}'
        l[[j]] <- conn$find(query = query, field = field)
        dt.name <- str_c("dt", i)
        assign(dt.name,
                value = rbindlist(l, use.names = TRUE, fill = TRUE))
        r.news <- rbindlist(list(r.news, get(dt.name)))
    }
}
rm(i, j, l, n, N, query, field, start, end, nid, dt.name)

z <- conn$find(query = '{"news_id":"31,1,7239482"}', field = '{"_id":0, "news_id":1, "reply.replynum":1, "reply.hotness":1}') %>% setDT()
zdt <- as.data.table(z)

str(zdt)