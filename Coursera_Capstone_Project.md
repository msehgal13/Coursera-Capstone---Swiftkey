Coursera Capstone Project: Word Predictor
========================================================
author: Manav Sehgal
date: 4th april, 2018
autosize: true

Overview
========================================================

This project is created for Coursera Data Scientist Specialization. As part of this project, I have created a word predictor, similar to Swiftkey.

The algorithm uses NLP to predict the next word using the previous two words by calculating the probability of all probable words in the existing corpus.

This app is designed to provide 5 unique words as predictions that the user can choose from.

Data Collection
========================================================

The first step in building the app is data collection. For this exercise we used the data set provided by Coursera - [Link] (https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

This data set contains a sample of news items, blogs and tweets for four languages - English, Finnish, Russian and Dutch. We have used the English language dataset to build our app.

The entire dataset contains ~4M english sentences. To make the model run faster, we have taken random 10% of this sample (~400K sentences). This is assuming the distribution of words in this random sample will be the same as the entire corpus

Data Cleaning
========================================================

The next step is to clean the data and convert into relevant ngrams (contiguous sequence of n words) to be used in our algorithm. The following steps were taken to clean the dataset

1. Transformed all the letters to lower case.
2. Removed punctuation - Our model is created only for the text words.
3. Removed number - Same as above, our model is created only for the text words.
4. Removed extra whitespaces - They are generally errors and are removed
5. Removed profanity words - We don't want our model to predict profanity words

Algorithm
========================================================
To predict the next word, we need to calculate the probabilities of all the probable words. These could be the ones present in the corpus. However, we also need to take into account words that are not abserved in the ngram in our corpus. To do that, I have used [Katz's back off model](https://en.wikipedia.org/wiki/Katz%27s_back-off_model).

Katz's backoff model says that for a given ngram, we need to save some part of probability for the words in the lower ngrams that are not observed in the higher ngrams. 

1. A discounting factor (d) is applied on the probability of higher ngrams*. The factor is calculating from [Good Turing estimation] (https://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation).
2. Left over probability is what remains of the probability after applying the discounting factor
3. This left over probability is distributed among the words observed in the lower ngram (and not in higher ngram) in the ratio of the probabilities of words in lower ngrams.
4. This is done for top 5 words in each ngram (as the app predicts the top 5 words) and then take the top 5 words as the calcualted probabilities.

**The discounting factor is applied only on the words which occur less frequently (frequency of less than 5) as they are assumed to be one off cases and have a higher chance of having an unobserved word. For higher , the probability is absolute.*

Application
========================================================

The application is deployed on the shiny server - [link](https://msehgal.shinyapps.io/Coursera_Capstone_Swiftkey/).

**Instructions**
1. Click on the above link to open the application in a web browser.
2. Enter a phrase in the left side bar panel titles - "Enter Input here"
3. A set of 5 predicted words will show on the right side under "Predicted words"

![alt text](word_predictor.png)

*Note - It takes a few seconds for the app to predict the words for the first time as the application is loading the data. After that the results are instantaneous*

References
========================================================

1. Katz's back off model wiki - [link](https://en.wikipedia.org/wiki/Katz%27s_back-off_model) 
2. Katz's back off model implentation in R (1) - [link](https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/)
3. Katz's back off model implentation in R (2) - [link](https://rpubs.com/mszczepaniak/predictkbo3model)
4. Github repository of the project - [link]()
