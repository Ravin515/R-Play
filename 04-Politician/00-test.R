library(mongolite)

conn <- mongo(db = "test", collection = "CrawlerChinaVitae", url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0}')

#biog <- iter$batch(size = 5)

flat_careers <- function(ele) {
    if (!is.null(ele[["branch"]])) {
        branch <- unlist(ele[["branch"]]) %>% str_c(collapse = ", ")
        ele[["branch"]] <- branch
        ele
    }
}

flat_bio <- function(ele) {
    careers <- ele[["careers"]]
    careers <- lapply(careers, flat_careers) %>% rbindlist(use.names = T, fill = T)

    ele[["careers"]] <- NULL
    others <- ele
    others <- rbindlist(list(others), use.names = T, fill = T)
    others[, ':='(rid = 1L)]
    if (nrow(careers) > 0) {
        careers[, ':='(rid = 1L)]
        others[careers, on = .(rid), nomatch = NA]
    } else {
        others
    }
}



#bio <- lapply(biog, flat_bio) %>% rbindlist(use.names = T, fill = T)
bio <- data.table()

while (!is.null(res <- iter$batch(size = 1000))) {
    chunk <- lapply(res, flat_bio) %>% rbindlist(use.names = T, fill = T)
    bio <- rbindlist(list(bio, chunk), use.names = T, fill = T)
}