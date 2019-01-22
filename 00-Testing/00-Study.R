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

a <- ls("package:base", all = T)

x <- 1
h <- function() {
    y <- 2
    i <- function() {
        z <- 3
        c(x, y, z)
    }
    i()
}
h()
rm(x, h)

j <- function(x) {
    y <- 2
    function() {
        c(x, y)
    }
}
k <- j(1)
k()
rm(j, k)

l <- function(x) x+1
f <- function() x + 1
codetools::findGlobals(f)

x <- sample(replace = T, 20, x = c(1:10, NA))
y <- runif(min = 0, max = 1, 20)
cor(m = "k", y = y, u = "p", x = x)

x <- 1:10
get("x")
"modify<-" <- function(x, position, value) { 
    x[position] <- value
    x
}

%function_name% #infix function
"function_name<-" #replacement function
`%paste%` <- function(x, y) str_c(x, y)
"a" %paste% "b"

`second<-` <- function(x, value) {
    x[2] <- value
    x
}
x <- 1:10
second(x) <- 5L
x

# a replacement function that modifies a random location in vector
"modify<-" <- function(x, position, value) {
    position <- runif(1, 1, length(x))
    x[position] <- value
    x
}

modify(x, 1) <- 9

y <- function() {
    y <- ls("package:base", all.names = T)
    y
}
y()
y("package:base")

j <- function(x) {
    y <- 2
    function() {
        c(x, y)
    }
}
k <- j(1)
k()

"(" <- function(e1) { 
    if (is.numeric(e1) && runif(1) < 0.1) {
        e1 + 1
    } else {
    e1
    }
}
replicate(100, (1 + 2))
rm("(")

f <- function(x) {
    f <- function(x) {
        f <- function(x) {
            x ^ 2
        }
        f(x) + 1
    }
    f(x) * 2
}
f(10)

f <- function() 1
g <- function() 2
class(g) <- "function"
class(f)
class(g)
length.function <- function(x) "function"
length(f)
length(g)
# when use length function
# otype() helps to distinguish between the behaviour of f and g

# Chapter 7 ENVIRONMENT
# Exercises
# Q1
is.baseenv <- function(x) identical(x, baseenv())
is.emptyenv <- function(x) identical(x, emptyenv())
is.bsanstr <- function(x, env = parent.frame()) {
    if (is.baseenv(env)) {
        return("True")
    } else {
        is.bsanstr(x, env = parent.env(env))
    }    
}
is.eptanstr <- function(x, env = parent.frame()) {
    if (is.emptyenv(env)) {
        return("True")
    } else {
        is.eptanstr(x, env = parent.env(env))
    }
}

is.bsanstr(globalenv)
is.eptanstr(globalenv)

srch.all <- function(env = parent.frame()) {
    pkges <- vector(mode = "character")
    while (!identical(env, emptyenv())) {
        pkges <- rbind(pkges, environmentName(env))
        srch.all(env = parent.env(env))
    } 
    pkges
}
srch.all()

# Q2
ray.get <- function(name, en, env = parent.frame()) {
    stopifnot(is.character(name), is.environment(en), length(name) == 1)
    if (identical(env, emptyenv())) {
        stop("Can't find ", name, call. = FALSE)
    }
    if (exists(name, en, inherits = FALSE)) {
        eval(parse(text = name))
    } else {
        ray.get(name, parent.env(en))
    }
}
ray.get("b", globalenv())

#Q3
fget <- function(name, en, env = parent.frame(), inherits = TRUE) {
    stopifnot(is.character(name), is.environment(en), length(name) == 1)
    if (identical(env, emptyenv())) {
        stop("Can't find ", name, call. = FALSE)
    }
    if (exists(name, en, inherits = inherits)) {
        if (is.function(eval(parse(text = name)))) {
            eval(parse(text = name))
        } 
        else {
            fget(name, parent.env(en))
        }
    } else {
        fget(name, parent.env(en))
    }
}
fget("get", baseenv())

#Q4
ray.exists <- function(name, env) {
    stopifnot(is.character(name), is.environment(env), length(name) == 1)
    if (identical(env, emptyenv())) {
        stop("Can't find", name, call. = FALSE)
    }
    if (name %in% ls(envir = env)) {
        TRUE
    } else {
        FALSE
    }
}
ray.exists("b", baseenv())

f <- function(x) x + y
environment(f) <- emptyenv()
environment(f)