library(data.table)
library(stringr)
# Q1
r.replys <- fread("replys.csv", header = T, encoding = "UTF-8", na.strings = "")
r.posts <- fread("posts.csv", header = T, encoding = "UTF-8", na.strings = "", fill = T)
# Q2
r.posts <- r.posts[postnums != 0]
r.posts[!is.na(post_id), .N]
r.posts[guba_name == "螺纹钢吧", .N > unique(postnums)]
earl.posts <- r.posts[order(create_time), .SD[1], keyby = .(guba_name)]
# Q3
late.replys <- r.replys[order(post_id, reply_time), .SD[.N]]
r.replys[, reply_content := str_replace_all(reply_content, "<.+>", "")]
# Q4
int.data <- r.replys[r.posts, on = .(post_id), nomatch = 0]
# Q5
int.data[, create_date := as.Date(create_time, "%Y-%m-%d")
    ][, create_time := str_replace_all(create_time, "[TZ]", " ")
    ][, create_time := as.POSIXct(create_time, format = "%Y-%m-%d %H:%M:%OS")]
tim <- CJ(create_date = seq(int.data[, min(create_date)], int.data[, max(create_date)], by = 'day'), guba_name = unique(int.data$guba_name))
int.data.ts <- int.data[tim, on = .(guba_name, create_date), nomatch = NA
    ][is.na(readnum) & is.na(replynum), ':='(readnum = 0, replynum = 0)]
# Q6.1
mx.post <- r.posts[order(-replynum), .SD[1:10]
    ][, .(post_id, guba_name, readnum, replynum)]
mx.ba <- r.posts[, .(replynum = sum(replynum), readnum = sum(readnum)), keyby = .(guba_name)
    ][order(-replynum), .SD[1:10]]
# Q6.2
library(pastecs)
stst.in <- r.posts[, .(replynum = replynum, readnum = readnum)]
stst.in <- stat.desc(stst.in)
# Q6.3
library(dplyr)
r.posts[, cor.test(replynum, readnum)]
b <- r.posts[, lm(replynum ~ readnum)] %>% summary() %>% list() %>% lapply(`[[`, "coefficients") %>% lapply(`[`, c(2,8)) %>% unlist()
post.reg <- r.posts[, .(coef = coef(lm(replynum ~ readnum))[2], p.value = lm(replynum ~ readnum) %>% summary() %>% list() %>% lapply(`[[`, "coefficients") %>% lapply(`[`, 8) %>% unlist()), keyby = .(guba_name)
    ][p.value < 0.05, .N]
# Q6.4
n <- 1000
reg.roll <- int.data.ts[, {
    l <- list()
    for (t in (n + 1) : .N) {
        l[[t]] <- as.list(c(coef(lm(replynum ~ readnum, data = .SD[(t - n):t]))[2], date = create_time[t]))
    }
    rbindlist(l)
},
    keyby = .(guba_name)]

reg.roll[, ":="(date = as.Date(date, origin = "1970-01-01"))]