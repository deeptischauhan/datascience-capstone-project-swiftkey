library(tm)
library(plyr)
preprocess_corpus <- function(corpus) {
        
        #Remove any whitespace
        corpus_processed  <- tm_map(corpus, stripWhitespace)
        #Remove any punctuation
        corpus_processed  <- tm_map(corpus_processed, removePunctuation)
        #Remove any stop words
        corpus_processed  <- tm_map(corpus_processed, removeWords, stopwords("english"))
        #Convert to lower case
        corpus_processed  <- tm_map(corpus_processed, content_transformer(tolower))
        
        
        corpus_processed  <- tm_map(corpus_processed , stemDocument,language = ("english"))
        
        # Remove profanity words
        profanity_vector <- as.vector(c("shit", "fuck", "ass", "asshole", "hell", 
                                        "bitch", "damn", "crap", "piss", "dick", "darn",
                                        "pussy", "fag", "cock", "bastard", "slut", "douche"))
        corpus_processed <- tm_map(corpus_processed , removeWords, profanity_vector)
        
        #corpus <- gsub("[[:digit:]]+","", corpus)
        #corpus <- gsub("http\\w+","", corpus)
        corpus_processed <- tm_map(corpus_processed, content_transformer(function(corpus_processed) gsub(corpus_processed, pattern = "http\\w+", replacement = "")))
        corpus_processed <- tm_map(corpus_processed, content_transformer(function(corpus_processed) gsub(corpus_processed, pattern = "[[:digit:]]+", replacement = "")))
        #Remove any numbers or digits
        corpus_processed  <- tm_map(corpus_processed, removeNumbers)
        return(corpus_processed)
        
}

create_ngram <- function(corpus, n) {
        
        if (n == 1) {
                
                TermDocumentMatrix(corpus)
                
        } else {
                
                tdm2(corpus, ngmin = n, ngmax = n)
                
        }
        
}

katz_backoff_model <- function(phrase) {
        
        if (typeof(phrase) == "character") {
                
                trigram_model <- function(tokens) {
                        
                        key <- function(tokens) {
                                paste(
                                        tail(
                                                tokens,
                                                n = 2
                                        )[1],
                                        tail(
                                                tokens,
                                                n = 2
                                        )[2]
                                )
                        }
                        
                        # find matches and their count
                        matches_count <- function(phrase) {
                                sapply(
                                        names(
                                                which(
                                                        sapply(
                                                                Terms(trigram),
                                                                function(terms) {
                                                                        grepl(
                                                                                phrase,
                                                                                paste(
                                                                                        strsplit(
                                                                                                terms, split = " "
                                                                                        )[[1]][1],
                                                                                        strsplit(
                                                                                                terms, split = " "
                                                                                        )[[1]][2]
                                                                                ),
                                                                                ignore.case = TRUE
                                                                        )
                                                                }
                                                        )
                                                )
                                        ),
                                        function(match) sum(tm_term_score(trigram, match))
                                )
                        }
                        
                        # find the last word of the most frequent match
                        tail_of_most_frequent_match <- function(phrase) {
                                matches <- matches_count(phrase)
                                if (length(matches) > 0) {
                                        tail(
                                                strsplit(
                                                        names(
                                                                head(
                                                                        which(matches == max(matches)),
                                                                        n = 1
                                                                )
                                                        )
                                                        , split = " ")[[1]],
                                                n = 1
                                        )
                                } else bigram_model(tail(corpus_input, n = 1))
                        }
                        
                        return(
                                tail_of_most_frequent_match(key(tokens))
                        )
                        
                }
                
                bigram_model <- function(token) {
                        
                        # find matches and their count
                        matches_count <- function(phrase) {
                                sapply(
                                        names(
                                                which(
                                                        sapply(
                                                                Terms(bigram),
                                                                function(terms) {
                                                                        grepl(
                                                                                phrase,
                                                                                strsplit(
                                                                                        terms, split = " "
                                                                                )[[1]][1],
                                                                                ignore.case = TRUE
                                                                        )
                                                                }
                                                        )
                                                )
                                        ),
                                        function(match) sum(tm_term_score(bigram, match))
                                )
                        }
                        
                        # find the last word of the most frequent match
                        tail_of_most_frequent_match <- function(phrase) {
                                matches <- matches_count(phrase)
                                if (length(matches) > 0) {
                                        tail(
                                                strsplit(
                                                        names(
                                                                head(
                                                                        which(matches == max(matches)),
                                                                        n = 1
                                                                )
                                                        )
                                                        , split = " ")[[1]],
                                                n = 1
                                        )
                                } else unigram_model(tail(corpus_input, n = 1))
                        }
                        
                        return(
                                tail_of_most_frequent_match(token)
                        )
                        
                }
                
                unigram_model <- function(token) {
                        
                        associations <- llply(unigram$dimnames$Terms, function(i) findAssocs(unigram, token, 0.99), .progress = "text" )
                        if (length(associations) > 0) {
                                names(sample(which(associations == max(associations)), 1))
                        } else return("will")
                        
                }
                
                # preprocess phrase
                corpus_input <-
                        VCorpus(
                                VectorSource(phrase),
                                list(reader = PlainTextDocument)
                        )
                corpus_input <- preprocess_corpus(corpus_input)
                corpus_input <- scan_tokenizer(corpus_input[[1]][[1]][1])
                
                return(
                        if (length(corpus_input) >= 2) {
                                print("Trigram Model")
                                trigram_model(corpus_input)
                        } else if (length(corpus_input) == 1) {
                                print("Bigram Model")
                                bigram_model(corpus_input)
                        } else{
                                print("Unigram Model")
                                unigram_model(corpus_input)
                        }
                )
                
        } else {
                stop("non-character or null input")
        }
        
}