# Environment
library(quanteda)
library(readtext)
library(tidyverse)
library(data.table)


# process_tokens
# returns: a tokens object that has been processed
# args: 
#   - toks = tokens object from quanteda

process_tokens <- function(toks){
    
    # create tokens and do basic processing
    toks <- tokens(toks, 
                   remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                   remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE,
                   remove_url = TRUE)
    
    # convert to lower case
    toks <- tokens_tolower(toks)
    
    # word stem
    toks <- tokens_wordstem(toks)
    
    return(toks)
}


# predict_next_word 
# returns: a list containing the following
#   input_text = the raw text entered into the search.
#   input_toks = the tokens that were used to perform the prediction.
#   potential_matches a dataframe containing the top 10 matches and probabilities.
# args: 
#   input_text = takes any string as an input and predicts what word will come next.
#   train.dfm = a dataframe containing the training data

predict_next_word <- function(input_text, train.dfm){
    
    # train.dfm <- fread("data/train.dfm.csv")
    
    # process the input text
    input_toks <- tokens(input_text)
    input_toks <- process_tokens(input_toks)
    
    # determine the range of ngrams to search
    num_toks <- length(input_toks[[1]])
    ngram_range <- min(3, num_toks):1
    
    # loop through each range of ngrams to find matches
    potential_matches <- data.frame()
    for (num in ngram_range){
        
        # create the ngrams
        search_ngrams <- tokens_ngrams(input_toks, num)
        # create the search phrase
        search_term <- paste0("^", tail(search_ngrams[[1]], 1), "_")
        # identify the in scope search area
        search.dfm <- train.dfm %>% filter (ngrams == num + 1)
        # search the dataframe
        temp <- search.dfm[grepl(pattern = search_term, x = search.dfm$feature), ]
        if(nrow(temp) == 0) next
        temp$ngram.size <- num
        # calculate frequncy and score
        total_ngrams <- sum(temp$frequency)
        temp <- temp %>%
            mutate(frequency.percent = frequency / total_ngrams) %>%
            mutate(score = num + frequency.percent)
        # join the results together
        potential_matches <- union_all(temp, potential_matches)
        if(nrow(potential_matches) >= 3) break # break the loop if 3 predictions have been found to speed up algo
        
    }
    
    # check to see if there are matches
    if(nrow(potential_matches) == 0) {
        return(list("input_text" = input_text,
                    "input_toks" = input_toks,
                    "potential_matches" = NULL
        ))
    }
    
    # arrange the potential matches to find the most likely
    potential_matches <- potential_matches %>%
        arrange(desc(score), desc(frequency.percent)) %>% 
        head(10)
    
    # get the best predicted next word
    potential_matches$split_location <- stringi::stri_locate_last(str = potential_matches$feature, regex = "_")[,2]
    potential_matches <- potential_matches %>%
        mutate(prediction = substr(feature, split_location + 1, nchar(feature))) %>%
        select(feature, prediction, score, frequency.percent, ngram.size, ngrams, frequency)
    
    
    # return results
    return(list("input_text" = input_text,
                "input_toks" = input_toks,
                "potential_matches" = potential_matches
    ))
}


# get_word
# returns: the top prediction as a string
# args:
#   x = takes any string as input

get_word <- function(x, train.dfm){
    y <- predict_next_word(x, train.dfm)
    if (is.null(y$potential_matches)){
        return("New word, no prediction")
    }
    top <- y$potential_matches[1,2] 
    return(top)
}


# get_word3
# returns: the top 3 predictions as a character vector
# args:
#   x = takes any string as input


get_word3 <- function(x, train.dfm){
    y <- predict_next_word(x, train.dfm)
    if (is.null(y$potential_matches)){
        return("New word, no prediction")
    }
    potential_matches <- y$potential_matches
    top <- unique(y$potential_matches$prediction)
    top3 <- top[1:3]
    return(top3)
}