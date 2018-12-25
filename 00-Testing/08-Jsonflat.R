library(data.table)
library(jsonlite)
library(stringr)
library(dplyr)

json <- fread("json.csv")

microbenchmark(flat.json <- json[, var := str_replace_all(var, '""', '\"')
    ][, {
        I <- list()
        for (i in 1:.N) {
            I[[i]] <- c(fromJSON(var[i]), cusnum = cusnum[i])
        }
        rbindlist(I, fill = T)        
    }] %>% fwrite("flat.json2.csv"))

library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(forcats)
library(tidyr)
library(readr)

microbenchmark(json %>%
    mutate(var = str_replace_all(var, '""', '\"')) %>%
    mutate(var = map(.x = var, .f = jsonlite::fromJSON)) %>%
    unnest() %>%
    as_tibble() %>%
    print %>%
    write_excel_csv('flat.json1.csv'))

#a <- a[1, 2]
#b <- fromJSON(a$var)