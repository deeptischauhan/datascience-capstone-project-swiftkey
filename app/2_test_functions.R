# get functions ----
source("prediction_functions.R")

# load model ----
if(!exists("train.dfm")){
    load("data/train.dfm.rda")
}

# Test the functions ----
test_text <- "I want to go"
z <- predict_next_word(test_text); z
get_word(test_text)
get_word3(test_text)