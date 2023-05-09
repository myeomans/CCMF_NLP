################################################
#
#       SICSS-London 2022 NLP Workshop
#
#     Some basic text manipulation tools
#
#
################################################

# paste() joins text - default setting is to put a space in the middle

paste("x","y")

# paste0 changes the default to put nothing in the middle

paste0("x","y")

# paste handles vectors - if they are the same length, one item from the first 
# argument will be pasted to one from the second, and so on down the line

paste0(c("x","y","z"),
       c("a","b"))

# If the second argument is only one item long, 
# it is pasted to every item in the first argument

paste0(c("x","y"),
       c("a"))

# If they are unequal lengths you can get strange behavior - not recommended!
paste0(c("x","y","z"),
       c("a","b"))

##############################

toolong="this string is good but it is too long"

# This counts characters
toolong %>%
  str_length()

# This counts words - the funny thing in the bracket is a "regular expression"
# REs are a bit advanced for this class.. For now just memorize this one
# It basically means "the number of blocks of consecutive alphabetical characters"
toolong %>%
  str_count("[[:alpha:]]+")


# str_sub() helps us chop up a strings
toolong %>%
  str_sub(0,19)

# negatives count from the end
toolong %>%
  str_sub(20,-1)

toolong %>%
  str_sub(-9,-1)

##############################

# str_replace substitutes one character string for another


c("abcx","defxx","ghiy") %>%
  str_replace("x","y")


# If you just want the literal characters, you need to "escape" them with \\
c("abc*","defx*","ghiy") %>%
  str_replace("\\*","y")

# We can delete character strings by replacing them with an empty string

c("abc","def","rabbit") %>%
  str_replace("b","")

# Notice it only replaces the first instance... to replace all, we use a different function

c("abc","def","rabbit") %>%
  str_replace_all("b","")


c("before","abetting","in my bed") %>%
  str_replace_all("be","")


##############################

# str_detect() tells us whether a string is contained within another

c("teases","taxes","xylophone") %>%
  str_detect("x")

# It is case sensitive
c("Before","abetting","in my bed") %>%
  str_detect("be")

# We can convert to lowercase first
c("Before","abetting","in my bed") %>%
  str_to_lower() %>%
  str_detect("be")


# We can also get counts using str_count()

c("Before","abetting","in my bed") %>%
  str_count("be")

c("Before","abetting","in my bed") %>%
  str_count("e")

c("Before","abetting","in my bed") %>%
  str_count("t")

###############################################################
###############################################################

######### Simple bag of words

testDocs<-c("This is a test sentence.", 
            "I am providing another sentence to test this.",
            "This isn't a sentence",
            "This is a test document. It has 2 sentences")

# First we need to split up the sentences into "tokens" - (usually words)

testDocs %>%
  tokens()

# We then count how often each token occurs in each document 
# This produces a "document feature matrix" (or document term matrix)
# One row for each doc, one column for each feature
testDocs %>%
  tokens() %>%
  dfm()

# We can also combine adjoining words into "bigrams"

testDocs %>%
  tokens() %>%
  tokens_ngrams(2) %>%
  dfm()

# often people combine multiple token lengths together, as ngrams
testDocs %>%
  tokens() %>%
  tokens_ngrams(1:2) %>%
  dfm()

# Many different ways to tokenize - see the help file for options

?tokens

# We can stem words

testDocs %>%
  tokens(remove_punct=TRUE) %>%
  tokens_wordstem()

# we can remove punctuation
testDocs %>%
  tokens(remove_punct=TRUE) %>%
  tokens_ngrams(1:2)

# we can remove numbers
testDocs %>%
  tokens(remove_numbers=TRUE) %>%
  tokens_ngrams(1:2)

# contractions are done with a function from textclean
testDocs %>%
  replace_contraction() %>%
  tokens()


# dfm converts everything to lower case by default, but we can turn this off
testDocs %>%
  tokens() %>%
  dfm()

testDocs %>%
  tokens() %>%
  dfm(tolower=FALSE)

# we can also remove "stop words"
testDocs %>%
  tokens() %>%
  tokens_select(pattern = stopwords("en"), 
                selection = "remove") %>%
  tokens_ngrams(1:2)

# This is the built-in quanteda stopword list
stopwords("en")

# we can create our own custom list if we like
testDocs %>%
  tokens() %>%
  tokens_select(pattern = c("a","is","the"), 
                selection = "remove") %>%
  tokens_ngrams(1:2)


# Instead of removing common words, we can downweight them, using tfidf

dox<-c("This is a sentence.",
       "this is also a sentence.",
       "here is a rare word",
       "here is another word.",
       "and other sentences")

# Without tfidf, all words are given the same weight
dox %>%
  tokens(remove_punct= TRUE) %>%
  dfm() %>%
  convert(to="data.frame") %>%
  select(-doc_id) %>%
  round(2)

# Here, rare words are given more weight
dox %>%
  tokens(remove_punct= TRUE) %>%
  dfm() %>%
  dfm_tfidf() %>%
  convert(to="data.frame") %>%
  select(-doc_id) %>%
  round(2)

# We can also remove words that are too rare to learn anything about

dox %>%
  tokens(remove_punct= TRUE) %>%
  dfm() %>%
  dfm_trim(min_docfreq = 2) %>%
  convert(to="data.frame") %>%
  select(-doc_id) %>%
  round(2)

# Usually we do this by proportion of words

dox %>%
  tokens(remove_punct= TRUE) %>%
  dfm() %>%
  dfm_trim(min_docfreq = .25,docfreq_type="prop") %>%
  convert(to="data.frame") %>%
  select(-doc_id) %>%
  round(2)

# Typically the cut-off gets set around 1% of documents

# Here  I am creating a function that saves all of our defaults in one place
# this is the same function that is in your "create_dfm.R" script
create_dfm<-function(text,
                    ngrams=1:2,
                    stop.words=TRUE,
                    min.prop=.01){
  if(!is.character(text)){                # First, we check our input is correct
    stop("Must input character vector")
  }
  drop_list=""
  if(stop.words) drop_list=stopwords("en") #uses stop.words arugment to adjust what is dropped
  
  text_data<-text %>%
    replace_contraction() %>%
    tokens(remove_numbers=TRUE,
           remove_punct = TRUE) %>%
    tokens_wordstem() %>%
    tokens_select(pattern = drop_list, 
                  selection = "remove") %>%
    tokens_ngrams(ngrams) %>%
    dfm() %>%
    dfm_trim(min_docfreq = min.prop,docfreq_type="prop")
  return(text_data)
}

create_dfm(dox)

# we can easily modify the defaults of our custom arguments
create_dfm(dox, ngrams=2)

create_dfm(dox, stop.words = FALSE)

create_dfm(dox, min.prop=.25)

# Note... this is a bit rudimentary
# If you prefer, you can use a more robust function I wrote
# it is stored in the doc2concrete package
# install.packages("doc2concrete")
# library(doc2concrete)
# 
# ngramTokens(dox)
