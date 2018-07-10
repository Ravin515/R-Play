library(mongolite)
conn <- mongo(collection = 'guba', db = 'guba', url = "mongodb://localhost:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0, "post_id":1, "reply":1}')

# 用于填充缺失值的函数。对iter提取之后，可能有些元素是NULL，由于NULL不能作为rbindlist的输入，所以需要把他改成空list
# 注意，NULL，NA，list() 三个的length都是0，但他们是不同的东西！这个涉及到 R 关键的 object type，你一定要弄懂，否则下面的代码你看起来会云里雾里的
set_empty_to_na <- function(x, null.as)) {
    if (length(x) == 0) { 
        if (is.null(x)) {
            x <- null.as
        }
    } 
    x
}

# 这个是 下面 make_post_reply 函数的 batch 版，但是由于 batch 版不稳定，所以只能用 one()了
#make_post_reply <- function(batch) {
    #lapply(batch, `[[`, "reply") %>% lapply(set_empty_to_na, null.as = list()) %>% lapply(rbindlist, fill = T, use.names = T) %>% rbindlist(use.names = T, fill = T)
#}

make_post_reply <- function(one) {
    # “one" 代表一行记录
    # reply 是把one的reply部分提取，生成data.table。
    reply <- one["reply"] %>% lapply(set_empty_to_na, null.as = list()) %>% lapply(rbindlist, fill = T, use.names = T) %>% rbindlist(use.names = T, fill = T)
    # post 是把 one 的post 部分提取，生成一个向量。
    # 我这里 post 只提取了 post_id，你可以自己改，提取多个变量，只不过此时需要把post变成一个data.table
    post <- one$post_id %>% sapply(set_empty_to_na, null.as = NA, USE.NAMES = F)

    # 如果post_id 非空（post_id空说明抓取过程中出现错误，这个很少见），把 上面生成的post和reply 合并成成一个data.table
    if (!is.null(post)) {
        if (nrow(reply) == 0) { # 如果 reply 不存在，那么生成的dt中只有post
            data.table(post.id = post)
        } else { # 如果reply 存在，那么把reply也加入dt
            data.table(post.id = post, reply)
        }
    } else {# 如果 post_id 空，那么输出空dt。注意这是个关键，如果不输出 空 data.table而是输出 NULL 或 NA，就会报错。这涉及到函数的对象一致性，是个难点。
        data.table()
    }
}

# 逐行读取
posts <- data.table()
while (!is.null(res <- iter$one())) {
    chunk <- make_post_reply(res)
    posts <- rbindlist(list(chunk, posts), use.names = T, fill = T)
}
r.posts <- fread("posts.csv", fill = T, encoding = "UTF-8")
r.replys <- fread("replys.csv", fill = T, encoding = "UTF-8")

a <- r.posts[1, 2, 3]
b <- r.posts[1, 1]
c <- r.posts[1]
microbenchmark({
earl.posts <- r.posts[, head(.SD, 1), by = .(guba_name)]
}, times = 100)

microbenchmark({
earl.posts <- r.posts[, .SD[1], by = .(guba_name)]
}, times = 100)

