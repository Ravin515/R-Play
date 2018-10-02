r.replys <- fread("replys.csv", header = T, encoding = "UTF-8", na.strings = "")
r.posts <- fread("posts.csv", header = T, encoding = "UTF-8", na.strings = "", fill = T)

r.posts <- r.posts[postnums != 0]
r.posts[!is.na(post_id), .N]
r.posts[guba_name == "ÂİÎÆ¸Ö°É", .N > unique(postnums)]
earl.posts <- r.posts[order(create_time), .SD[1], keyby = .(guba_name)]

late.replys <- r.replys[order(post_id, reply_time), .SD[.N], keyby = .(post_id, reply_time)]
r.replys[, reply_content := str_replace(reply_content, "<.+>", "")]

int.data <- r.replys[r.posts, on = .(post_id), nomatch = 0]
int.data[, create_date := as.Date(create_time, "%Y-%m-%d")]

tim <- CJ(create_date = seq(int.data[, min(create_date)], int.data[, max(create_date)], by = 'day'), guba_name = unique(int.data$guba_name))
int.data.ts <- int.data[tim, on = .(guba_name, create_date), nomatch = NA]