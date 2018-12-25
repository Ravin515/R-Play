#' A simplified save function.
#'
#' @export
#' @param x The object to be saved
#' @param compress Whether the object should be compressed
sv <- function(x, compress = T) {
    start <- Sys.time()
    svdir <- paste0(substitute(x), ".Rdata")
    if (file.exists(svdir)) {
        message("Dataset -", substitute(x), "- will be OVERWRITTEN!", "\n")
    }
    # Setting compression_level from default 6 to 3 results in an increase of size by 13% but a decrease of time by 64%!!!
    save(list = deparse(substitute(x)), file = svdir, compress = compress, compression_level = 3)
    cat("Dataset -", substitute(x), "- has been saved", "\n")
    end <- Sys.time()
    gap <- end - start
    cat("Use", round(gap, 2), units(gap), "\n\n")
}


#' A simplified load function.
#'
#' @export
#' @param x The object to be loaded.
#' @param force Whether the object should be reloaded if it's already in the current environment.
ld <- function(x, force = F) {
    start <- Sys.time()
    if (force == F) {
        if (!exists(as.character(substitute(x)))) {
            load(paste0(substitute(x), ".Rdata"), envir = .GlobalEnv)
            cat("Dataset -", substitute(x), "- has been loaded", "\n")
        } else {
            message(substitute(x), " -- already loaded! Will NOT load again.")
        }
    } else if (force == T) {
        if (exists(as.character(substitute(x)))) {
            message("Dataset -", substitute(x), "- will be OVERWRITTEN!", "\n")
        }
        load(paste0(substitute(x), ".Rdata"), envir = .GlobalEnv)
        cat("Dataset -", substitute(x), "- has been loaded", "\n")
    }
    end <- Sys.time()
    gap <- end - start
    cat("Use", round(gap, 2), units(gap), "\n\n")
}