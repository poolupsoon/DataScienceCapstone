library(wordnet)
library(tokenizers)
library(tm)
set.seed(12345)

####

mycon <- file("~/DataScienceCapstone/en_US/en_US.blogs.txt", open = "rb")
vec_blogs <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("~/DataScienceCapstone/en_US/en_US.news.txt", open = "rb")
vec_news <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("~/DataScienceCapstone/en_US/en_US.twitter.txt", open = "rb")
vec_twitter <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)

vec_sample <- c(sample(vec_blogs, length(vec_blogs) * 0.2),
                sample(vec_news, length(vec_news) * 0.2),
                sample(vec_twitter, length(vec_twitter) * 0.2))

mycon <- file("~/DataScienceCapstone/en_US/en_US.sample_raw.txt", open = "w")
writeLines(vec_sample, mycon)
close(mycon)

####

mycon <- file("~/DataScienceCapstone/en_US/en_US.sample_raw.txt", open = "rb")
vec_sample <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("~/DataScienceCapstone/en_US/profanity.txt", open = "rb")
vec_profanity <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)

removeUnicode <- function(x) {
    gsub("<U+(.*?)>", " ", x)
}
removeNonAscii <- function(x) {
    iconv(x, to = "ASCII", sub = "")
}
removeApostrophe <- function(x) {
    gsub("'", "", x)
}
removePunct <- function(x) {
    gsub("[^[:alnum:][:space:]']", " ", x)
}
removeSingleChar <- function(x) {
    gsub("\\b[[:alpha:]]\\b", "", x)
}

mycorpus <- Corpus(VectorSource(vec_sample))
mycorpus <- tm_map(mycorpus, content_transformer(removeUnicode))
mycorpus <- tm_map(mycorpus, content_transformer(removeNonAscii))
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
mycorpus <- tm_map(mycorpus, removeWords, vec_profanity)
mycorpus <- tm_map(mycorpus, removeNumbers)
mycorpus <- tm_map(mycorpus, content_transformer(removeApostrophe))
mycorpus <- tm_map(mycorpus, content_transformer(removePunct))
mycorpus <- tm_map(mycorpus, content_transformer(removeSingleChar))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, PlainTextDocument)

mycon <- file("~/DataScienceCapstone/en_US/en_US.sample_cleaned.txt", open = "w")
writeLines(mycorpus[[1]][[1]], mycon)
close(mycon)

####

mycon <- file("~/DataScienceCapstone/en_US/en_US.sample_cleaned.txt", open = "rb")
mycorpus <- Corpus(VectorSource(readLines(mycon, encoding = "UTF-8", skipNul = TRUE)))
mycorpus <- tm_map(mycorpus, PlainTextDocument)
close(mycon)

tokenizer_1gram <- function(x) {
    unlist(tokenize_ngrams(as.character(x), n = 1))
}
freq_1gram <- termFreq(mycorpus[[1]], control = list(tokenize = tokenizer_1gram))
freq_1gram <-  sort(freq_1gram, decreasing = TRUE)
mycon <- file("~/DataScienceCapstone/en_US/en_US.dictionary_1gram.txt", open = "w")
writeLines(names(freq_1gram)[freq_1gram > 1], mycon)
close(mycon)

tokenizer_2gram <- function(x) {
    unlist(tokenize_ngrams(as.character(x), n = 2))
}
freq_2gram <- termFreq(mycorpus[[1]], control = list(tokenize = tokenizer_2gram))
freq_2gram <-  sort(freq_2gram, decreasing = TRUE)
mycon <- file("~/DataScienceCapstone/en_US/en_US.dictionary_2gram.txt", open = "w")
writeLines(names(freq_2gram)[freq_2gram > 1], mycon)
close(mycon)

tokenizer_3gram <- function(x) {
    unlist(tokenize_ngrams(as.character(x), n = 3))
}
freq_3gram <- termFreq(mycorpus[[1]], control = list(tokenize = tokenizer_3gram))
freq_3gram <-  sort(freq_3gram, decreasing = TRUE)
mycon <- file("~/DataScienceCapstone/en_US/en_US.dictionary_3gram.txt", open = "w")
writeLines(names(freq_3gram)[freq_3gram > 1], mycon)
close(mycon)

tokenizer_4gram <- function(x) {
    unlist(tokenize_ngrams(as.character(x), n = 4))
}
freq_4gram <- termFreq(mycorpus[[1]], control = list(tokenize = tokenizer_4gram))
freq_4gram <-  sort(freq_4gram, decreasing = TRUE)
mycon <- file("~/DataScienceCapstone/en_US/en_US.dictionary_4gram.txt", open = "w")
writeLines(names(freq_4gram)[freq_4gram > 1], mycon)
close(mycon)

####

setDict("C:/Users/poolupsoon/Documents/DataScienceCapstone/dict")

mycon <- file("~/DataScienceCapstone/data/en_US.dictionary_1gram.txt", open = "rb")
dict_1gram <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("~/DataScienceCapstone/data/en_US.dictionary_2gram.txt", open = "rb")
dict_2gram <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("~/DataScienceCapstone/data/en_US.dictionary_3gram.txt", open = "rb")
dict_3gram <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("~/DataScienceCapstone/data/en_US.dictionary_4gram.txt", open = "rb")
dict_4gram <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)

removeApostrophe <- function(x) {
    gsub("'", "", x)
}
removePunct <- function(x) {
    gsub("[^[:alnum:][:space:]']", " ", x)
}
removeSingleChar <- function(x) {
    gsub("\\b[[:alpha:]]\\b", "", x)
}

preclean <- function(x) {
    mycorpus <- Corpus(VectorSource(x))
    mycorpus <- tm_map(mycorpus, content_transformer(tolower))
    mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
    mycorpus <- tm_map(mycorpus, removeNumbers)
    mycorpus <- tm_map(mycorpus, content_transformer(removeApostrophe))
    mycorpus <- tm_map(mycorpus, content_transformer(removePunct))
    mycorpus <- tm_map(mycorpus, content_transformer(removeSingleChar))
    mycorpus <- tm_map(mycorpus, stripWhitespace)
    mycorpus <- tm_map(mycorpus, PlainTextDocument)
    return(unlist(strsplit(mycorpus[[1]][[1]], "[[:space:]]+")))
}

pred4gram <- function(x) {
    mydict <- grep(paste0("^", paste(tail(x, 3), collapse = " ")), dict_4gram, value = TRUE)
    if (length(mydict) > 0) {
        return(gsub("(.*) ", "", mydict))
    } else (
        return(NULL)
    )
}

pred3gram <- function(x) {
    mydict <- grep(paste0("^", paste(tail(x, 2), collapse = " ")), dict_3gram, value = TRUE)
    if (length(mydict) > 0) {
        return(gsub("(.*) ", "", mydict))
    } else (
        return(NULL)
    )
}

pred2gram <- function(x) {
    mydict <- grep(paste0("^", tail(x, 1)), dict_2gram, value = TRUE)
    if (length(mydict) > 0) {
        return(gsub("(.*) ", "", mydict))
    } else (
        return(NULL)
    )
}

antonyms <- function(x) {
    x <- tail(x, 3)
    vec_ant <- vector()
    pos <- c("ADJECTIVE", "ADVERB", "NOUN", "VERB")
    for (i in seq_len(length(x))) {
        myant <- NULL
        filter <- getTermFilter("ExactMatchFilter", x[i], TRUE)
        for (j in seq_len(length(pos))) {
            terms <- getIndexTerms(pos[j], 1, filter)
            if (!is.null(terms)) {
                synsets <- getSynsets(terms[[1]])
                for (k in seq_len(length(synsets))) {
                    related <- tryCatch(getRelatedSynsets(synsets[[k]], "!"), error = function(e) NULL)
                    if (!is.null(related)) {
                        myant <- head(getWord(related[[1]]), 1)
                        break
                    }
                }
            }
            if (!is.null(myant)) {
                break
            }
        }
        vec_ant <- c(vec_ant, myant)
    }
    return(vec_ant)
}

predtext <- function(x) {
    x <- preclean(x)
    pred_a <- antonyms(x)
    pred_4 <- pred4gram(x)
    pred_4 <- setdiff(pred_4, pred_a)
    if ((length(pred_a) + length(pred_4)) < 6) {
        pred_3 <- pred3gram(x)
        pred_3 <- setdiff(pred_3, c(pred_a, pred_4))
        if ((length(pred_a) + length(pred_4) + length(pred_3)) < 6) {
            pred_2 <- pred2gram(x)
            pred_2 <- setdiff(pred_2, c(pred_a, pred_4, pred_3))
            if ((length(pred_a) + length(pred_4) + length(pred_3) + length(pred_2)) < 6) {
                pred_1 <- dict_1gram
                pred_1 <- setdiff(pred_1, c(pred_a, pred_4, pred_3, pred_2))
                pred <- c(pred_4, pred_3, pred_2, head(pred_1, (6 - (length(pred_a) + length(pred_4) + length(pred_3) + length(pred_2)))), pred_a)
            } else {
                pred <- c(pred_4, pred_3, head(pred_2, (6 - (length(pred_a) + length(pred_4) + length(pred_3)))), pred_a)
            }
        } else {
            pred <- c(pred_4, head(pred_3, (6 - (length(pred_a) + length(pred_4)))), pred_a)
        }
    } else {
        pred <- c(head(pred_4, (6 - (length(pred_a)))), pred_a)
    }
    return(pred)
}
