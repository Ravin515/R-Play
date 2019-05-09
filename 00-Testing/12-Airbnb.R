mtcars <- as.data.table(mtcars)

a <- colnames(mtcars)
for (i in 1:ncol(mtcars)) {
     mtcars[, a[i] := shift(.SD, n = i, fill = NA, type = 'lead'), .SDcol = i]
}


b <- str_c(colnames(mtcars), 1:ncol(mtcars))
for (i in 1:ncol(mtcars)) {
    mtcars[, b[i] := shift(.SD, n = i, fill = NA, type = 'lead'), .SDcols = i]
}

