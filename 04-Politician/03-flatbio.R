library(mongolite)


conn <- mongo(collection = 'CrawlerChinaVitae', db = 'test', url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0}')

flat_careers <- function(ele) {
    #ele <- lapply(ele, set_empty_to_na, NA)
    if (!is.null(ele[['branch']])) {
        branch <- unlist(ele[['branch']]) %>% str_c(collapse = ', ')
        ele[['branch']] <- branch
        ele
    }
}

flat <- function(ele) {
    careers <- ele[['careers']]
    careers <- lapply(careers, flat_careers) %>% rbindlist(use.names = T, fill = T)

    ele[['careers']] <- NULL
    others <- ele
    others <- rbindlist(list(others), use.names = T, fill = T)
    others[, ':='(rid = 1L)]

    if (nrow(careers) > 0) {
        careers[, ':='(rid = 1L)]
        careers[others, on = .(rid), nomatch = 0]
    } else {
        others
    }
    
}

bio <- data.table()
while (!is.null(res <- iter$batch(size = 1e3))) {
    chunk <- lapply(res, flat) %>% rbindlist(use.names = T, fill = T)
    bio <- rbindlist(list(bio, chunk), use.names = T, fill = T)
}

fwrite(bio, file = "biography.csv")



#a <- data.table(x = 1, y = 2)
#a[2 * (1:.N) - 1]
#sample()

