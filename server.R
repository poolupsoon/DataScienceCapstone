library(shiny)
library(tm)
library(wordnet)
set.seed(12345)

setDict("dict/")

mycon <- file("data/en_US.dictionary_1gram.txt", open = "rb")
dict_1gram <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("data/en_US.dictionary_2gram.txt", open = "rb")
dict_2gram <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("data/en_US.dictionary_3gram.txt", open = "rb")
dict_3gram <- readLines(mycon, encoding = "UTF-8", skipNul = TRUE)
close(mycon)
mycon <- file("data/en_US.dictionary_4gram.txt", open = "rb")
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

shinyServer(function(input, output) {
    preclean <- reactive({
        mycorpus <- Corpus(VectorSource(input$textIn))
        mycorpus <- tm_map(mycorpus, content_transformer(tolower))
        mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
        mycorpus <- tm_map(mycorpus, removeNumbers)
        mycorpus <- tm_map(mycorpus, content_transformer(removeApostrophe))
        mycorpus <- tm_map(mycorpus, content_transformer(removePunct))
        mycorpus <- tm_map(mycorpus, content_transformer(removeSingleChar))
        mycorpus <- tm_map(mycorpus, stripWhitespace)
        mycorpus <- tm_map(mycorpus, PlainTextDocument)
        return(unlist(strsplit(mycorpus[[1]][[1]], "[[:space:]]+")))
    })
    
    pred4gram <- reactive({
        mydict <- grep(paste0("^", paste(tail(preclean(), 3), collapse = " ")), dict_4gram, value = TRUE)
        if (length(mydict) > 0) {
            return(gsub("(.*) ", "", mydict))
        } else (
            return(NULL)
        )
    })
    
    pred3gram <- reactive({
        mydict <- grep(paste0("^", paste(tail(preclean(), 2), collapse = " ")), dict_3gram, value = TRUE)
        if (length(mydict) > 0) {
            return(gsub("(.*) ", "", mydict))
        } else (
            return(NULL)
        )
    })
    
    pred2gram <- reactive({
        mydict <- grep(paste0("^", tail(preclean(), 1)), dict_2gram, value = TRUE)
        if (length(mydict) > 0) {
            return(gsub("(.*) ", "", mydict))
        } else (
            return(NULL)
        )
    })
    
    antonyms <- reactive({
        x <- tail(preclean(), 3)
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
    })
    
    predtext <- reactive({
        pred_a <- antonyms()
        pred_4 <- pred4gram()
        pred_4 <- setdiff(pred_4, pred_a)
        if ((length(pred_a) + length(pred_4)) < 6) {
            pred_3 <- pred3gram()
            pred_3 <- setdiff(pred_3, c(pred_a, pred_4))
            if ((length(pred_a) + length(pred_4) + length(pred_3)) < 6) {
                pred_2 <- pred2gram()
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
    })
    
    output$textOut1 <- renderText({
        if (input$textIn == "") {
            "-"
        } else {
            predtext()[1]
        }
    })
    
    output$textOut2 <- renderText({
        if (input$textIn == "") {
            NULL
        } else {
            predtext()[2]
        }
    })
    
    output$textOut3 <- renderText({
        if (input$textIn == "") {
            NULL
        } else {
            predtext()[3]
        }
    })
    
    output$textOut4 <- renderText({
        if (input$textIn == "") {
            NULL
        } else {
            predtext()[4]
        }
    })
    
    output$textOut5 <- renderText({
        if (input$textIn == "") {
            NULL
        } else {
            predtext()[5]
        }
    })
    
    output$textOut6 <- renderText({
        if (input$textIn == "") {
            NULL
        } else {
            predtext()[6]
        }
    })
})
