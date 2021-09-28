
library(tidyverse)
library(entropy)
library(parallel)
library(multidplyr)
passes = 10

############

cat("[ ] Loading LDA document topic matrix\n")

doc_id = read_lines("models/doc_id")
doc = read_delim("models/doc_topic.model", delim = " ",
                 col_names = FALSE,
                 col_types = cols())
n_topics = ncol(doc)
n_docs = nrow(doc) / passes
doc = tail(doc, n_docs)

topic = read_delim("models/word_topic.model", delim = " ",
                   col_names = F, skip = 11,
                   col_types = cols())
topic = topic[,-c(1, ncol(topic))]


#############




# look at some results of the lda

## meta = read_csv("data/lemma_hash.csv", col_types = cols())

## topics = topic %>%
##     mutate(hash = 1:n()) %>%
##     gather("topic", "prob", -hash) %>%
##     mutate(topic = as.numeric(as.factor(topic))) %>%
##     left_join(meta, by = "hash") 






## #####################
## # Information theoretic measures inspired by
## # Barron, A. T., Huang, J., Spang, R. L., & DeDeo, S. (2018). Individuals, institutions, and innovation in the debates of the French Revolution. Proceedings of the National Academy of Sciences, 115(18), 4607-4612.

## # for each row in lda@gamma (ie for each document), calculate:
## # novelty (suprise from past)
## # resonance (surprise to future)
## # transience: novelty - resonance

vdiff <- function(x,n,fun) sapply(n, function(i) fun(x,i))
vlag  <- function(x,n) vdiff(x,0:n,dplyr::lag)
vlead <- function(x,n) vdiff(x,0:n,dplyr::lead)
## vboth <- function(x,n) {
## # for the "marked high resonance sample" plot
##     forward = vdiff(x, 1:n, dplyr::lead)
##     backward = vdiff(x, 1:n, dplyr::lag)
##     combined = x %>%
##         cbind(as.data.frame(backward[,ncol(backward):1])) %>%
##         cbind(as.data.frame(forward))
##     names(combined) = c(0, -n:-1, 1:n)
##     return(combined)
## }

## novelty <- function(mat, w) {
##     vlag(1:nrow(mat), w) %>%          # produce the lags (same shape as document)
##         apply(1, function(idx) {        # then for each row (document)
##             lapply(idx[2:length(idx)], function(i) { #for each lag
##                 if (is.na(i)) return(NA)             #check if it's na (we're at beginning / end of data)
##                 ## calculate surprise from past to present
##                 KL.plugin(mat[i,], mat[idx[1],], unit = "log2")
##             }) %>%
##                 unlist() %>%
##                 mean()
##         })}



## the xyz_2_ functions can be run in parallel without copying the data to each thread.
## To do this, we pass an R environment in with one variable: the document matrix "mat"

novelty2 <- function(w, cl, env) {
    # produce the lags (same shape as document)
    vlag(1:nrow(env$mat), w) %>%          
        parApply(cl = cl, X = ., MARGIN = 1,
                 function(idx, env) {
                     # then for each row (document)
                     #print(env)
                     mean(unlist(lapply(idx[2:length(idx)], function(i) {
                         #for each lag
                         #check if it's na (we're at beginning / end of data)
                         if (is.na(i)) return(NA)             
                         ## calculate surprise from past to present
                         entropy::KL.plugin(env$mat[i,], env$mat[idx[1],], unit = "log2")
                         
                     })))}, env = env)}


## transience <- function(mat, w) {
##     vlead(1:nrow(mat), w) %>%         # produce the leads (same shape as document)
##         apply(1, function(idx) {        # then for each row (document)
##             lapply(idx[2:length(idx)], function(i) { #for each lead
##                 if (is.na(i)) return(NA)             #check if it's na (we're at beginning / end of data)
##                 ## calculate surprise from present to future
##                 KL.plugin(mat[idx[1],], mat[i,], unit = "log2")
##             }) %>%
##                 unlist() %>%
##                 mean()
##         })}



transience2 <- function(w, cl, env) {
    # produce the lags (same shape as document)
    vlag(1:nrow(env$mat), w) %>%          
        parApply(cl = cl, X = ., MARGIN = 1,
                 function(idx, env) {
                     # then for each row (document)
                     #print(env)
                     mean(unlist(lapply(idx[2:length(idx)], function(i) {
                         #for each lag
                         #check if it's na (we're at beginning / end of data)
                         if (is.na(i)) return(NA)             
                         ## calculate surprise from present to future
                         entropy::KL.plugin(env$mat[idx[1],], env$mat[i,], unit = "log2")
                         
                     })))}, env = env)}



# for the "marked high resonance sample" plot
## kld_matrix <- function(mat, n) {
##     vboth(1:nrow(mat), n) %>%
##         apply(1, function(idx) {
##             # for each row
##             lapply(idx[2:length(idx)], function(i) {
##                 # for each lead'
##                 if (is.na(i)) return(NA)
##                 # surprise from present to X
##                 KL.plugin(
##                 })
##         })}


z <- function(d) (d - mean(d)) / sd(d)


dataenv = new.env()
dataenv$mat = doc

cluster <- parallel::makePSOCKcluster(4)


cat("[ ] Calculating novelty and transience.\n")

w = 7

lda_output = data_frame(doc_id) %>%
    mutate(
        novelty = novelty2(w, cluster, dataenv),
        transience = transience2(w, cluster, dataenv),
        resonance = novelty - transience) %>%
    filter(complete.cases(.)) %>%
    mutate(z_novelty = z(novelty),
           z_transience = z(transience),
           z_resonance = z(resonance))



res_nov_model = lm(z_resonance ~ z_novelty, data = lda_output)
lda_output$delta_R = lda_output$z_resonance - predict(res_nov_model)


lda_output %>%
    write_csv("data/nov_tra_res.csv")



