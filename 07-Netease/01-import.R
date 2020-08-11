library(mongolite)
# collection artist_id 
conn.artid <- mongo(collection = 'artist_id', db = 'NetEaseMusic', url = "mongodb://localhost:27017")
iter.artid <- conn.artid$iterate(query = '{}', field = '{"_id":0}')
artist.id <- data.table()
while (!is.null(res <- iter.artid$batch(size = 1e3))) {
    chunkid <- lapply(res, as.data.table)  %>% rbindlist(use.names =T, fill = T)
    artist.id <- rbindlist(list(artist.id, chunkid), use.names = T, fill = T)
}
sv(artist.id, compress = T)

# collection artist_album
conn.artalbm <- mongo(collection = 'artist_album', db = 'NetEaseMusic', url = "mongodb://localhost:27017")
iter.artalbm <- conn.artalbm$iterate(query = '{}', field = '{"_id":0}')
res <- iter.artalbm$batch(size = 1e3)
flat_res <- function(x) {
    dt <- lapply(x, as.data.table) %>% rbindlist(use.name = T, fill = T)

    # replace all the blanks to NA
    indx <- which(sapply(dt, is.character))
    for (j in indx)
        set(dt, i = grep("^$|^ $", dt[[j]]), j = j, value = NA_character_)

    alias <- dt[, unlist(alias), by = .(album_id, artist_id)] %>% setnames(3, "alias") %>% unique()
    dt[, alias := NULL]
    d <- alias[dt, on = .(album_id, artist_id), allow.cartesian = TRUE] %>% unique()
    d
}
#a <- flat_res(res)
artist.album <- data.table()
while (!is.null(res <- iter.artalbm$batch(size = 1e3))) {
    chunkalbm <- flat_res(res)
    artist.album<- rbindlist(list(artist.album, chunkalbm), use.names = T, fill = T)
}

sv(artist.album, compress = T)
# replace all the blank to NA
indx <- which(sapply(artist.album, is.character))
for (j in indx)
    set(artist.album, i = grep("^$|^ $", artist.album[[j]]), j = j, value = NA_character_)