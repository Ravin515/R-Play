library(quanteda)
library(readtext)
library(data.table)
library(stringr)
library(spacyr)
setwd("C:/Users/Mr.Stylee/Desktop/regulatory-sandbox")
test.txt <- readtext("*.pdf") %>% as.data.table()
test.txt[, doc_id := str_replace_all(doc_id, "[0-9]", "")
    ][, text := str_replace_all(text, "\r\n", " ")
    ][, text := str_replace_all(text, "-", "")
    ][, text := str_replace_all(text, "[0-9]", "")
    ][, text := str_replace_all(text, "sandbox", "")
    ][, text := str_replace_all(text, "e.g|eu", "")
    ][, text := str_replace_all(text, "ii+", "")
    ][, text := str_replace_all(text, "financial", "")
    ][, text := str_replace_all(text, "firms", "")
    ][, text := str_replace_all(text, "regulatory", "")
    ][, text := str_replace_all(text, "rbi", "")
    ][, text := str_replace_all(text, "rs", "")
    ][, text := str_replace_all(text, "µÚ|Ò³|¹²|", "")
    ][, doc_id := str_replace_all(doc_id, "_Regulatory_Sandbox.pdf", "")
    ][, text := str_replace_all(text, "can|may", "")
    ][, text := str_replace_all(text, "services", "")
    ][, text := str_replace_all(text, "fca's", "")
    ][, text := str_replace_all(text, "set|fss|eu|fscs|ftu|readvaluelaunch|dnb|fis|rbi|fos", "")
    ][, text := gsub('\\b\\w{1,3}\\b', '', text)]

text.txt <- test.txt[, .(text = str_c(text, collapse = " ")), by = .(doc_id)]
my.corpus <- corpus(text.txt) 
wordcloud_txt <- dfm(my.corpus, remove = stopwords(language = "en"), remove_punct = TRUE)
#spacy_initialize()
#wordcloud_noun <- text.txt[, noun := spacy_extract_nounphrases(text)]
textplot_wordcloud(wordcloud_txt,
                   min_count = 6,
                   random_order = FALSE,
                   rotation = .25,
                   colors = RColorBrewer::brewer.pal(8, "Dark2"))

wordsimil_txt <- dfm(corpus_subset(my.corpus), remove = stopwords(language = "en"), stem = T, remove_punct = TRUE)
word_simil <- textstat_simil(wordsimil_txt, c("AU", "HongKong", "India", "Netherland", "Singapore"), margin = "documents", method = "cosine") %>% as.list()
a <- lapply(word_simil, as.data.frame)

#for (i in seq_along(a)) {
    #name <- str_c("name", i, ".csv")
    #fwrite(a[[i]], row.names = T, name)
#}

a <- lapply(a, setDT, keep.rownames = T)

nation <- text.txt[["doc_id"]][-6]

for (i in seq_along(a)) {
    setnames(a[[i]], 2, "similarity")
    a[[i]][, country := nation[i]]
}

txt <- rbindlist(a, fill = T)
txt <- dcast(txt, rn ~ country, value.var = "similarity")
fwrite(txt, "similarity.csv")

setwd("C:/Users/Mr.Stylee/Desktop/regulatory-sandbox/UN")
cloud.txt <- readtext("*.pdf") %>% as.data.table()
cloud.txt[, text := gsub('\\b\\w{1,3}\\b', '', text)]
my.corpus <- corpus(cloud.txt)
wordcloud_txt <- dfm(my.corpus, remove = stopwords(language = "en"), remove_punct = TRUE)
#spacy_initialize()
#wordcloud_noun <- text.txt[, noun := spacy_extract_nounphrases(text)]
textplot_wordcloud(wordcloud_txt,
                   min_count = 6,
                   random_order = FALSE,
                   rotation = .25,
                   colors = RColorBrewer::brewer.pal(8, "Dark2"))