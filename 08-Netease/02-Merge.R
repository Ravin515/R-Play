ld(artist.id, force = T)
ld(artist.album, force = T)

artist.album[, artist_name := str_replace_all(artist_name, " ", "")
    ][, artist_name := str_replace_all(artist_name, "\t", "")]
artist.id[, artist_name := str_replace_all(artist_name, " ", "")
    ][, artist_name := str_replace_all(artist_name, "\t", "")]
album <- artist.id[artist.album[, artist_id := as.character(artist_id)], on = .(artist_id)]
a <- album[artist_name != i.artist_name]
id.china <- artist.id[artist_type == 1001 | artist_type == 1002 | artist_type == 1003]
album.china <- artist.album[, artist_id := as.character(artist_id)
    ][id.china, on = .(artist_id, artist_name)]