library(quadprog)
library(nloptr)
m <- function(x) {
    return(1.925 + 2.775*x[1] +2.075*x[2] - 2.775*x[1]*x[2])
}

st <- function(x) {
    return(62*x -111*x[1]^2 -83*x[1]*x[2] + 111*x[1]^2*x[2]-7)
}

isres(x0 = c(0, 0), fn = m, heq = st, lower = c(0, 0), upper = c(1, 1))