library(tidyverse)
library(stopwords)
library(parallel)

# load data from udpipe, do the last processing, then pass it on to vowpal wabbit

data = read_csv("data/tidy_metadata.csv",
                col_types = "ccnnccTncncccn")

cat("[ ] parsing udpipe output\n")


read_parse_udpipe <- function(i) {
    library(tidyverse)
    lemma_list = str_c("data/from_udpipe/", i) %>%
        read_file() %>%
        # split udpipe output into documents
        str_split("# sent_id = \\d+\n", simplify = TRUE) %>%
        # skip header
        `[`(-1) %>%
        # parse the rest of each doc as tsv. keep lemma and type only
        map(read_tsv, skip = 1,
            col_names = c("lemma", "type"),
            col_types = "__cc______") %>%
        map(select, lemma, type) %>%
        # skip punctuation
        map(filter, type != "PUNCT") %>%
        # make sure there's only one word in each
        map(pluck, "lemma") %>%
        map(str_extract, "\\w+") %>%
        map(setdiff, custom_stopwords$word) %>%
        map(function(x) {x[!str_detect(x, "\\d+")]}) %>%
        map(function(x) {x[nchar(x) > 2]})

    return(lemma_list)
    

    ## ## cache this data to the filesystem
    ## save("lemma_list", file = str_c("data/from_udpipe_parsed/", i, ".Rdata"))
    ## rm(lemma_list)
    
}


##################
# prepare stopwords
ft_members = read_delim("data/folketing_members.csv", ";", col_names = FALSE,
                        col_types = cols())[[1]]
ft_titler = read_lines("folketing_titler.txt")
custom_stopwords = str_split(ft_members, " ") %>%
    c(ft_titler, str_c(ft_titler, "en")) %>%
    c(stopwords("da", source = "stopwords-iso")) %>%
    c("nr", "tak", "hr", "fm","ab", "hristian") %>%
    unlist() %>%
    str_to_lower() %>%
    tibble(word = .)



cluster = parallel::makePSOCKcluster(4)
clusterExport(cluster, "custom_stopwords")

lemma = clusterMap(cluster, read_parse_udpipe, unique(data$.groups)) %>%
    tibble(.groups = unique(data$.groups)) %>%
    unnest() %>%
    cbind(select(data, doc_id)) %>%
    unnest()

stopCluster(cluster)

names(lemma) <- c(".groups", "doc_id", "lemma")


# write out clean data for export for others to use

lemma %>%
    select(-.groups) %>%
    group_by(doc_id) %>%
    summarise(text = str_c(lemma, collapse = " ")) %>%
    right_join(data) %>%
    write_csv("data/folketinget_1953_2019_tokenized.csv")



##########
# select top n words
topn = count(lemma, lemma) %>%
    filter(n >= 50)

lemma3 = filter(lemma, lemma %in% topn$lemma)

######
# write out tidy data the the record
lemma3 %>%
    group_by(doc_id) %>%
    summarise(text = str_c(lemma, collapse = " ")) %>%
    write_csv("data/tidy_lemmas.csv")


rm(lemma)
rm(topn)
rm(data)
gc()


#####
# prep vw file
cat("[ ] Writing files ready for vw\n")
out = count(lemma3, doc_id, lemma) %>%
    ungroup() %>%
    mutate(hash = as.integer(as.factor(lemma))) %>%
    arrange(as.numeric(doc_id), desc(n))

# write hash table for later lookup
out %>%
    distinct(hash, lemma) %>%
    arrange(hash) %>%
    #tail(100) %>% View()
    write_csv("data/lemma_hash.csv")

out2 = unite(out, "freq", hash, n, sep = ":", remove = FALSE)

out3 = split(out2, out2$doc_id) %>%
    map_chr(~str_c(.$freq, collapse = " ")) %>%
    str_c("| ", .)

write_lines(out3, "models/hashed_lda_ip.vw")

out2$doc_id %>%
    unique() %>%
    write_lines("models/doc_id")
