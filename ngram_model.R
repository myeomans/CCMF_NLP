################################################
#
#       SICSS-London 2022 NLP Workshop
#
#  Predicting, evaluating, and interpreting
#
#
################################################

# Restaurant reviews
# review_dat<-readRDS("review_dat.RDS")

# First thing - check variables

names(review_dat)


# Calculate a 1-gram feature count matrix for the review data, with no dropped words
dfm1<-SICSSL_dfm(review_dat$text,
                ngrams=1,
                min.prop=0,
                stop.words = FALSE)

dim(dfm1) # >10k ngrams! Too many

# most common words - obvious
sort(colMeans(dfm1),decreasing=TRUE)[1:20]

# least common words
sort(colMeans(dfm1))[1:20]

# Let's put this on a plot
ngram_counts<-data.frame(word=colnames(dfm1),
                         frequency=colMeans(dfm1),
                         f_rank=rank(-colMeans(dfm1),
                                     ties.method="first"))

ngram_counts %>%
  ggplot(aes(x=f_rank,y=frequency)) +
  geom_point()


# Let's write the words on the plot
ngram_counts %>%
  ggplot(aes(x=f_rank,y=frequency,label=word)) +
  geom_text()

# That's a mess... let's create a new column and pick only a few words to plot

ngram_counts <- ngram_counts %>%
  mutate(word_label=ifelse(f_rank%in%c(1:10, # top 10 words
                                       sample(11:200,10), # ten random words from 11-100
                                       sample(201:1000,20), # twenty random words from 101-1000
                                       sample(1000:n(),20)), # twenty random words above 1000
                           word,""))

# better, but still messy.
ngram_counts %>%
  ggplot(aes(x=f_rank,y=frequency,label=word_label)) +
  geom_text()

# Let's get rid of the rarest words and plot again

ngram_counts %>%
  filter(f_rank<1000) %>%
  ggplot(aes(x=f_rank,y=frequency,label=word_label)) +
  geom_text()

# still some overlap.. we'll use the ggrepel package to space out the words
# We'll also add the points for all the words, just to see
ngram_counts %>%
  filter(f_rank<1000) %>%
  ggplot(aes(x=f_rank,y=frequency,label=word_label)) +
  geom_point(color="red") +
  geom_label_repel(max.overlaps=100,size=7,
                   nudge_x = 1,nudge_y = 1,
                   force=3) + 
  labs(x="Frequency Rank",y="Frequency per Document") +
  theme_bw() +
  theme(axis.title = element_text(size=24),
        axis.text = element_text(size=24))

# let's save the plot for my slides
ggsave("word_freq.png")

######## Ok, let's build a model to predict price!

# First, let's look at our price data

table(review_dat$price)

# Let's only use 1-grams for now
dfm3<-SICSSL_dfm(review_dat$text,ngrams=1) %>%
  convert(to="data.frame") %>%
  select(-doc_id)

# Lots of words
dim(dfm3)

#  Most common words in 1- and 2-price reviews... lots of the same words!
sort(colMeans(dfm3[review_dat$price==2,]),decreasing=T)[1:20]

sort(colMeans(dfm3[review_dat$price==1,]),decreasing=T)[1:20]

# What we really care about is - does the presence of a word predict price?

# A simple start - correlate each word with star rating

correlations<-dfm3 %>%
  summarise_all(~round(cor(.,review_dat$price),3)) %>%
  unlist()

# Ten lowest associations
sort(correlations)[1:10]

# Ten highest associations
rev(sort(correlations))[1:10]

# note - same as:
sort(correlations,decreasing=TRUE)[1:10]

# Let's put this on a plot

cor_set<-data.frame(correlation=correlations,
                    frequency=colMeans(dfm3),
                    word=colnames(dfm3)) %>%
  # # let's group the points so we can add color
  mutate(colour=case_when(
    correlation>.02  ~ "high", # positive correlations (> +.02)
    correlation<(-.02) ~ "low", # negative correlations (< -.02)
    T ~ "none"))               # black otherwise (i.e. if it is close to zero)

head(cor_set)

# Let's make a quick plot
cor_set %>%
  ggplot(aes(x=correlation,y=frequency,label=word,color=colour)) +
  geom_point() +
  geom_label_repel()+  
  theme_bw()

####### A few problems with this plot:
#
# The repel function is removing too many words
# We probably don't want to label the neutral words
# The points are all compressed at the bottom
# The colours are chosen automatically
# Axis labels are too small and don't make much sense
# We don't really need a legend here
# It would be nice to have a dark line indicating zero 
# The other grid lines are not that useful

# Here's a fixed version

cor_set %>%
  mutate(word=ifelse(abs(correlation)>.02,word,NA)) %>% # get rid of labels for all neutral words
  ggplot(aes(x=correlation,y=frequency,label=word,color=colour)) +
  scale_color_manual(breaks=c("low","none","high"),
                     values=c("navyblue","gray","forestgreen"))+  # manually assign colors to each category
  geom_vline(xintercept=0)+ # Add dark line at zero
  geom_point() +
  geom_label_repel(max.overlaps=16,force=3)+ # tolerate more overlapping words and force them apart
  scale_y_continuous(trans="log2",  # log-transform the Y axis so it's not compressed at the bottom
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+ # manually set axis ticks
  theme_bw() +
  labs(x="Correlation with Price",y="Uses per Review")+ # Write in proper axis titles
  theme(legend.position = "none",         # get rid of legend
        panel.grid = element_blank(),     # get rid of grid lines
        axis.title=element_text(size=20), # increase size of axis titles
        axis.text=element_text(size=16))  # increase size of axis text

##################################################

# As we will discuss, we are not interested in effects of individual words
# Instead, we care more about how all the words perform as a class

# To do this, we will use the cv.glmnet() function to build a model

# First, we need to split the data into training and testing samples
train_split=sample(1:nrow(review_dat),round(nrow(review_dat)/2))

length(train_split)

# create our prediction variables
dfm3<-SICSSL_dfm(review_dat$text,ngrams=1) %>%
  convert(to="data.frame") %>%
  select(-doc_id)


trainX<-dfm3 %>%
  slice(train_split) %>%
  as.matrix() # we convert to matrix format for glmnet

trainY<-review_dat %>%
  slice(train_split) %>%
  pull(price)

testX<-dfm3 %>% 
  slice(-train_split) %>%
  as.matrix() # we convert to matrix format for glmnet

testY<-review_dat %>%
  slice(-train_split) %>%
  pull(price)

# Put training data insto LASSO model (note - glmnet requires a matrix)

lasso_mod<-cv.glmnet(x=trainX,y=trainY)

# generate predictions for test data
test_predict<-predict(lasso_mod,newx = testX)[,1]

# Note that while the true answers are binary, the predictions are continuous
# Always check these distributions!!
hist(testY)
hist(test_predict)

# For now, let's just split the predictions in two, using the median

test_predict_binary=ifelse(test_predict>median(test_predict),
                           2,1)

# This should have the same values as testY
hist(test_predict_binary)

# and we can calculate accuracy from that

round(100*mean(test_predict_binary==testY),3)

#######################################################
# Let's get a little better at interpreting the model

# instead of splitting the dfm we can split the data itself

review_dat_train<-review_dat[train_split,]
review_dat_test<-review_dat[-train_split,]

review_dat_dfm_train<-SICSSL_dfm(review_dat_train$text,ngrams=1)

review_dat_dfm_test<-SICSSL_dfm(review_dat_test$text,ngrams=1) %>%
  dfm_match(colnames(review_dat_dfm_train))

# let's build a model to predict star rating this time
rev_model<-glmnet::cv.glmnet(x=review_dat_dfm_train %>%
                               as.matrix(),
                             y=review_dat_train$stars)

plot(rev_model)

#### Interpret with a coefficient plot
# This plots coefficients from the model, rather than raw correlations
# Many fewer features, and more focused on what the model is actually doing
plotDat<-rev_model %>%
  coef() %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(review_dat_dfm_train),
                       freq=colMeans(review_dat_dfm_train)))

plotDat %>%
  mutate_at(vars(score,freq),~round(.,3))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=ngram,color=score)) +
  scale_color_gradient(low="blue",high="red")+
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 30,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Review")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))


#### Evaluate Accuracy

# we are going to use a function that calculates non-parametric correlations
# this function reports percentage accuracy, where 50% is random guessing
# it is calculated across all possible pairs of observations
# this is the same function that is in your "kendall_acc.R" script
kendall_acc<-function(x,y,percentage=TRUE){
  kt=cor(x,y,method="kendall")
  kt.acc=.5+kt/2
  kt.se=sqrt((kt.acc*(1-kt.acc))/length(x))
  report=data.frame(acc=kt.acc,
                    lower=kt.acc-1.96*kt.se,
                    upper=kt.acc+1.96*kt.se)
  report = round(report,4)
  if(percentage) report = report*100
  return(report)
}


test_ngram_predict<-predict(rev_model,
                            newx = review_dat_dfm_test %>%
                              as.matrix())[,1]

acc_ngram<-kendall_acc(review_dat_test$stars,test_ngram_predict)

acc_ngram

############ Find examples

# store predictions in data, calculate accuracy
review_dat_test<-review_dat_test %>%
  mutate(prediction=test_ngram_predict,
         error=abs(stars-prediction),
         bias=stars-prediction)

close_high<-review_dat_test %>%
  filter(stars==5 & error<.5) %>%
  select(text,stars,prediction)

close_low<-review_dat_test %>%
  filter(stars==1 & error<.5) %>%
  select(text,stars,prediction)

close_high
close_high %>%
  slice(1:2) %>%
  pull(text)

close_low
close_low %>%
  slice(1:2) %>%
  pull(text)

# Error analysis - find biggest misses

hist(review_dat_test$prediction)

hist(review_dat_test$stars)

miss_high<-review_dat_test %>%
  arrange(bias) %>%
  slice(1:10) %>%
  select(text,stars,prediction)

miss_low<-review_dat_test %>%
  arrange(-bias) %>%
  slice(1:10) %>%
  select(text,stars,prediction)

miss_low
miss_low%>%
  slice(1:2) %>%
  pull(text)

miss_high%>%
  slice(1:2) %>%
  pull(text)

# Error analysis - compare quantiles in confusion matrix
table(review_dat_test$stars %>%
        ntile(5),
      test_ngram_predict %>%
        ntile(5))



#### Evaluate Accuracy
test_ngram_predict<-predict(rev_model,
                            newx = review_dat_dfm_test %>%
                              as.matrix())[,1]

acc_ngram<-kendall_acc(review_dat_test$stars,test_ngram_predict)

acc_ngram


############### Benchmarks

acc_random<-kendall_acc(review_dat_test$stars,sample(test_ngram_predict))

acc_random

# Create benchmarks

review_dat_test <- review_dat_test %>%
  mutate(text_wdct=str_count(text,"[[:alpha:]]+"),
         text_sentiment=syuzhet::get_sentiment(text))

acc_wdct<-kendall_acc(review_dat_test$stars,review_dat_test$text_wdct)

#lower than 50%! Worse than chance! this just means it is negatively correlated
acc_wdct

# Note - you can see that the scale is reversed in the confusion matrix!
table(review_dat_test$stars %>%
        ntile(5),
      review_dat_test$text_wdct %>%
        ntile(5))

# here, we flip the predictor variable
acc_wdct<-kendall_acc(review_dat_test$stars,-review_dat_test$text_wdct)

acc_wdct


# We'll talk about sentiment more later... But this is from the documentation
?syuzhet::get_sentiment()
#  The default method, "syuzhet" is a custom sentiment dictionary developed in the Nebraska Literary Lab. 
# Here is the dictionary it uses
syuzhet::get_sentiment_dictionary(dictionary="syuzhet") %>%
  head(30)

acc_sentiment<-kendall_acc(review_dat_test$stars,review_dat_test$text_sentiment)

acc_sentiment

################### Subtract features

# in this case, we will remove the words from the sentiment dictionary
# and see how well the model does with all of the other words

sentiment_words<-syuzhet::get_sentiment_dictionary(dictionary="syuzhet") %>%
  pull(word) %>%
  SnowballC::wordStem()

# remove from training data
review_dat_dfm_train_minus<-review_dat_dfm_train %>%
  convert(to="data.frame") %>%
  select(-doc_id) %>%
  select(-contains(sentiment_words))

dim(review_dat_dfm_train)
dim(review_dat_dfm_train_minus)

# remove from test data
review_dat_dfm_test_minus<-review_dat_dfm_test %>%  
  convert(to="data.frame") %>%
  select(-doc_id) %>%
  select(-contains(sentiment_words))


# retrain
rev_model_minus<-glmnet::cv.glmnet(x=review_dat_dfm_train_minus %>%
                                     as.matrix(),
                                   y=review_dat_train$stars)


test_ngram_predict_minus<-predict(rev_model_minus,
                                  newx = review_dat_dfm_test_minus %>%
                                    as.matrix())[,1]


acc_ngram_minus<-kendall_acc(review_dat_test$stars,test_ngram_predict_minus)

# performance drops.. the words were removed were important!
acc_ngram_minus


#### Interpret with a coefficient plot
plotDat<-rev_model_minus %>%
  coef() %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(review_dat_dfm_train_minus),
                       freq=colMeans(review_dat_dfm_train_minus)))

plotDat %>%
  ggplot(aes(x=score,y=freq,label=ngram,color=score)) +
  scale_color_gradient(low="blue",high="red")+
  geom_vline(xintercept=0)+
  geom_point() +
  geom_label_repel(max.overlaps = 20,force = 6)+  
  scale_y_continuous(trans="log2",
                     breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  theme_bw() +
  labs(x="Coefficient in Minus Model",y="Uses per Review")+
  theme(legend.position = "none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=16))
