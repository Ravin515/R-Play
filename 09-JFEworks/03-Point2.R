load("Point1.Rdata")
point1 <- point1[score == 1]
point2 <- point1[, port_ret := mean(ret), by = .(month, tag)
    ][, unique(.SD[, c(1, 2, 21)])
    ][, port_cum_ret := {
        n <- 12
        a <- vector()
        for (t in (n+1):.N) {
            a[t] <- sum(port_ret[(t-n):t])
        }
        a
    }, by = .(tag)]

stargazer(point2[tag == "bottom", .(port_ret)], median = T, type = "text")
stargazer(point2[tag == "top", .(port_ret)], median = T, type = "text")