# extracting diag of a matrix
ExtraDia <- function(x) {
    if (is.matrix(x)) {
        y <- vector(mode = "numeric", length = ncol(x))
        for (a in 1:ncol(x)) {
            y[a] <- x[a, a]
        }
    }
    y
}

# reverse operation of the function which()
unwhich <- function(x, n) {
    out <- rep_len(FALSE, n)
    out[x] <- TRUE
    out
}

total <- fread("total.csv", header = T)
setnames(total, "Labor cost", "labcst")
setnames(total, "Production costs", "Prodcst")
as.data.table(total)
mod <- lm(labcst ~ Prodcst, data = total)
mod["df.residual"]
s <- summary(mod)
s["r.squared"]

m <- as.data.frame(matrix(rnorm(150 * 300), 150, 300))
x <- 1:150
y <- 1:300
# disorder the column and the row
t <- as.data.frame(m[sample(x), sample(y)])

# select a random contiguous sample of m rows from a data frame
i <- sample(c(1:145), 1)
se <- seq(i, i + 4, by = 1)
t <- as.data.frame(m[se,])