## Background

The text prediction model is an ngram language model that analyses text to predict what the next word should be. The algorithm will predict what word it thinks you wish to type next.

## About the model

The model was built as part of the JHU Coursera Data science Capstone project. At a high level, the model works as follows:

**1. Develop ngram model** 

- Using [training data](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) that contains text from twitter, blogs, and news sites, a corpus was developed. To keep the model lightweight, only a small percentage of randomly selected data was used for training.
- The training text was pre-processed to remove any characters that were non-alpha (e.g. a, b, c, etc.).
- The training text was tokenized and further processed in an attempt to normalize the data to get the most our of our smaller training set.
- Ngrams ranging from a size of 1 to 4 were created, and then transformed into a data feature matrix (DFM). The DFM counted the frequency that each ngram occurred with.

**2. Predicting the next word**

The prediction algorithm considers the last 3 words that have been typed in order to generate a prediction. To predict the next word, an approach similar to the so called *Stupid Back-off* was used (this approach was selected as it is easier to understand, however it may not produce the most accurate or useful results):

- the app turns the text entered by the user into a 3 word ngram (the tokens used to generate the ngram are processed as well to "normalize" them)
- the app searches the training data to identify if the 3 ngrams have ever occurred before
- if the ngrams have occurred, the app then identifies the word that proceeds the 3 ngrams
- in the cases where there is more than one word, the model selects the word that occurs most frequently
- if no matches are found for 3 word ngrams, the model then performs the same search using a 2 word ngram
- the model suggests words based on the following criteria
    - words preceded by larger word ngrams are favored over those with less
    - words that occur more frequently are favored over those that occur less frequently

For example, the model would work something like this:

- *"What do you think of president"* may be typed into the search box
- The model will look at the 3 most recent words, in this case: *"think of president"*
- It will then search the training data for any time these three words have occurred in a row
- It will then suggest what ever the top 3 most common preceding words are
- If the model is not able to find those three words ever occurring, it will back of and search for *"of president"*, if no matches are found it will further back-off and search for *"president"*

## Resources

Project:

- [GitHub Repo](https://github.com/SamEdwardes/predictive-text-model-swift-key)
- [Data](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)
[JHU Coursera Capstone](https://www.coursera.org/learn/data-science-project)
- [Training data](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

Ngram models:

- [Predicting Next Word Using Katz Back-Off](https://rpubs.com/mszczepaniak/predictkbo3model)
- [Large Language Models in Machine Translation (Stupid Backoff)](http://www.aclweb.org/anthology/D07-1090.pdf) *note this link will download a pdf document*
- [Katz’s Backoff Model Implementation in R](https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/)
- [Beginers guide to quanteda](https://data.library.virginia.edu/a-beginners-guide-to-text-analysis-with-quanteda/)
- [Text mining infrastucture in R](http://www.jstatsoft.org/v25/i05/)
- [CRAN Task View: Natural Language Processing](http://cran.r-project.org/web/views/NaturalLanguageProcessing.html)
- [Videos](https://www.youtube.com/user/OpenCourseOnline/search?query=NLP) and [Slides](https://web.stanford.edu/~jurafsky/NLPCourseraSlides.html) from Stanford Natural Language Processing course
