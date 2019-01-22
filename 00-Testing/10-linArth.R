data <- 1:16
data_random <- data.table(replicate(1e6, sample(data)))
random <- function(d) {
    x <- seq(from = 2, to = length(d), by = 2)
    b <- function(x) { 
        sort(d[(x - 1):x])
    }
    sapply(seq(from = 2, to = length(d), by = 2), b, simplify = FALSE) %>% unlist()
}

data.smlt <- data_random[, lapply(.SD, random)] %>% melt(measure.vars = colnames(data_random))
data.smlt[, ':='(id.match = rep(1:8, each = 2)), by = .(variable)
    ][, id.paste := str_c(value, collapse = ""), by = .(id.match, variable)
    ][, id.paste := as.numeric(id.paste)
    ][, id.group := prod(id.paste + 0.01), by = .(variable)]

# data.smlt[, uniqueN(id)]

setnames(data.smlt, 1:2, c("gnrt","prtcpt"))
fwrite(data.smlt, "data.smlt.csv")