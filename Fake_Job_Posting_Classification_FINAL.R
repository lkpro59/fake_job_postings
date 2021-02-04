#Modeling Pipeline
#1. Analyze the data, understand the variables and decide which variables to use
#2. Perform data cleaning and transformations on the variables of interest
#3. Feature selection
#4. Fitting models, cross validation and evaluating performaces

rm(list = ls())
library(tidyverse)

#read data
fjp <- read.csv("./datasets/fake_job_postings/fake_job_postings.csv")
fjp_tb <- as_tibble(fjp)
str(fjp_tb)

#STEP 1 - DATA PRE-PROCESSING
#drop the following:
#1.job_id(identifier) 2.department(65% null) 3. salary_range(84% null) 4.employment_type(65% full time and 19% null) 
fjp_tb <- select(fjp_tb, -c(1,4,5,13))
str(fjp_tb)

#no missing values  for these variables and no transformations needed
table(fjp_tb$telecommuting, useNA = "ifany")
table(fjp_tb$has_company_logo,useNA = "ifany")
table(fjp_tb$has_questions, useNA = "ifany")

#transformation for Variables with missing values****
#required_experience,required_education,industry & function.
#these variables contain empty values, we first need to convert those to actual NA's

#function to replace empty values with actual NAs
replace_empty_with_na <- function(t){
  ifelse(t == "", NA, t)
}
fjp_tb$required_experience <- unlist(lapply(as.character(fjp_tb$required_experience), replace_empty_with_na),use.names=FALSE)
fjp_tb$required_education <- unlist(lapply(as.character(fjp_tb$required_education), replace_empty_with_na),use.names=FALSE)
fjp_tb$industry <- unlist(lapply(as.character(fjp_tb$industry), replace_empty_with_na),use.names=FALSE)
fjp_tb$function. <- unlist(lapply(as.character(fjp_tb$function.), replace_empty_with_na),use.names=FALSE)
fjp_tb$location <- unlist(lapply(as.character(fjp_tb$location), replace_empty_with_na),use.names=FALSE)

#check if replacing the empty values with NA worked
table(fjp_tb$required_experience, useNA = "ifany")
table(fjp_tb$required_education, useNA = "ifany")
table(fjp_tb$industry, useNA = "ifany")
table(fjp_tb$function., useNA = "ifany")
table(is.na(fjp_tb$location)) #we have long list for location, view only the na's table value

#Replace the NA's with the most frequent value for each variable that contains NA's
#This is a prefered approach for handling missing values for a categorical variables
#install.packages(modeest)
library(modeest)

cols_with_na <- c("required_experience", "required_education", "industry", "function.","fraudulent", "location")
most_frequent_value_real <- apply(filter(fjp_tb[,colnames(fjp_tb) %in% cols_with_na], fraudulent=="0"),
                                  2,
                                  mfv,
                                  na_rm =  TRUE)
most_frequent_value_real

most_frequent_value_fake <- apply(filter(fjp_tb[,colnames(fjp_tb) %in% cols_with_na], fraudulent=="1"),
                                  2,
                                  mfv,
                                  na_rm =  TRUE)
most_frequent_value_fake

#replace the null value for "required_experience" with most frequent value per class
table(fjp_tb$required_experience, useNA = "ifany")
most_frequent_value_real[2]
most_frequent_value_fake[2]

fjp_tb$required_experience[fjp_tb$fraudulent=="1"]<- unlist(lapply(fjp_tb$required_experience[fjp_tb$fraudulent=="1"], function(e){
  ifelse(is.na(e), most_frequent_value_fake[2], e)
}), use.names = FALSE)
table(fjp_tb$required_experience, useNA = "ifany")

fjp_tb$required_experience[fjp_tb$fraudulent=="0"]<- unlist(lapply(fjp_tb$required_experience[fjp_tb$fraudulent=="0"], function(e){
  ifelse(is.na(e), most_frequent_value_real[2], e)
}), use.names = FALSE)
table(fjp_tb$required_experience, useNA = "ifany")

#Replace the null value for "required_education" with most frequent value per class
table(fjp_tb$required_education, useNA = "ifany")
most_frequent_value_real[3]
most_frequent_value_fake[3]

fjp_tb$required_education[fjp_tb$fraudulent=="1"]<- unlist(lapply(fjp_tb$required_education[fjp_tb$fraudulent=="1"], function(e){
  ifelse(is.na(e), most_frequent_value_fake[3], e)
}), use.names = FALSE)
table(fjp_tb$required_education, useNA = "ifany")

fjp_tb$required_education[fjp_tb$fraudulent=="0"]<- unlist(lapply(fjp_tb$required_education[fjp_tb$fraudulent=="0"], function(e){
  ifelse(is.na(e), most_frequent_value_real[3], e)
}), use.names = FALSE)
table(fjp_tb$required_education, useNA = "ifany")

#Replace the null value for "industry" with most frequent value per class
table(fjp_tb$industry, useNA = "ifany")
most_frequent_value_real[4]
most_frequent_value_fake[4]

fjp_tb$industry[fjp_tb$fraudulent=="1"]<- unlist(lapply(fjp_tb$industry[fjp_tb$fraudulent=="1"], function(e){
  ifelse(is.na(e), most_frequent_value_fake[4], e)
}), use.names = FALSE)
table(fjp_tb$industry, useNA = "ifany")

fjp_tb$industry[fjp_tb$fraudulent=="0"]<- unlist(lapply(fjp_tb$industry[fjp_tb$fraudulent=="0"], function(e){
  ifelse(is.na(e), most_frequent_value_real[4], e)
}), use.names = FALSE)
table(fjp_tb$industry, useNA = "ifany")

#replace the null value for "function." with most frequent value per class
table(fjp_tb$function., useNA = "ifany")
most_frequent_value_real[5]
most_frequent_value_fake[5]

fjp_tb$function.[fjp_tb$fraudulent=="1"]<- unlist(lapply(fjp_tb$function.[fjp_tb$fraudulent=="1"], function(e){
  ifelse(is.na(e), most_frequent_value_fake[5], e)
}), use.names = FALSE)
table(fjp_tb$function., useNA = "ifany")

fjp_tb$function.[fjp_tb$fraudulent=="0"]<- unlist(lapply(fjp_tb$function.[fjp_tb$fraudulent=="0"], function(e){
  ifelse(is.na(e), most_frequent_value_real[5], e)
}), use.names = FALSE)
table(fjp_tb$function., useNA = "ifany")

#replace the missing value for "location" with most frequent value per class
table(is.na(fjp_tb$location))
most_frequent_value_real[1]
most_frequent_value_fake[1]

fjp_tb$location[fjp_tb$fraudulent=="1"]<- unlist(lapply(fjp_tb$location[fjp_tb$fraudulent=="1"], function(e){
  ifelse(is.na(e), most_frequent_value_fake[1], e)
}), use.names = FALSE)
table(is.na(fjp_tb$location))

fjp_tb$location[fjp_tb$fraudulent=="0"]<- unlist(lapply(fjp_tb$location[fjp_tb$fraudulent=="0"], function(e){
  ifelse(is.na(e), most_frequent_value_real[1], e)
}), use.names = FALSE)
table(is.na(fjp_tb$location))

#additional transformation for location variable
#split to state and country variables

#function to trim leading and trailing spaces from a given value
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#function to split location value and extract the country
#the dataset has location values like "US, NY, New York" - this function gets us "US"
get_country <- function(l){
  locs <- strsplit(l, ",")[[1]] #split by ',',
  return(trim(locs[1]))
}

#function to splot location value and extract the state
get_state <- function(l){
  locs <- strsplit(l, ",")[[1]] #split by ',',
  if(!is.na(locs[2]) && trim(locs[2]) != "")
    return(trim(locs[2]))
  else
    return(trim(locs[3]))
}

#add country and state variables to our dataset using the functions
fjp_tb <- mutate(fjp_tb, country = unlist(lapply(as.character(fjp_tb$location), get_country), use.names=FALSE),
                         state = unlist(lapply(as.character(fjp_tb$location), get_state), use.names=FALSE)
                 )     
#check the result for the first five entries
fjp_tb$location[1:5]
fjp_tb$country[1:5]
fjp_tb$state[1:5]

#drop the location attribute, since we have split it in to state and country
fjp_tb$location <- NULL

#convert to factors
fjp_tb$telecommuting = as.factor(fjp_tb$telecommuting)
fjp_tb$has_company_logo = as.factor(fjp_tb$has_company_logo)
fjp_tb$has_questions = as.factor(fjb$has_questions)
fjp_tb$state = as.factor(fjp_tb$state)
fjp_tb$country = as.factor(fjp_tb$country)
fjp_tb$fraudulent = as.factor(fjp_tb$fraudulent)

#check the tibble structure at this point
str(fjp_tb)

#Data Transformation for Text Variables, we have four variables that requires text processing
#company profile, description, benefits and requirements
#we will create a document term matrix for each variable using the most frequent terms we find in the fake observations
#using the frequent terms in the fake observations resulted in better performance as opposed to using frequent terms found in all
#also after fitting the model, we found out that description and company profile variables are helping in prediction
#hence the code is cleaned to only include processing for these two variables

#create fake postings and real postings data sets
fraudulent_posting <- filter(fjp_tb, fraudulent == '1') #will get all postings that are fake
real_posting <- filter(fjp_tb, fraudulent == '0') #will get all postings that are real

#a function to create a word cloud from a given document(corpus) and minimum word count required to be included
generate_word_cloud <- function(theCorpus, min_freq){
  theCorpus <- theCorpus %>%
               tm_map(removeNumbers) %>%
               tm_map(removePunctuation) %>%
               tm_map(stripWhitespace)
 
  theCorpus <- tm_map(theCorpus, content_transformer(tolower))
  theCorpus <- tm_map(theCorpus, removeWords, stopwords("english"))
  
  dtm <- TermDocumentMatrix(theCorpus) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) #sum up the term frequency listed as rows in the marix and flip it to get words and thier frequency data frame
  df <- data.frame(word = names(words),freq=words)
  
  set.seed(1234) # for reproducibility #scale=c(3.5,0.25)
  wordcloud(words = df$word, freq = df$freq, min.freq = min_freq, max.words=300, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
}

library(textclean)
library(tm)
library(SnowballC)
library(wordcloud)

#function to convert the document matrix term counts to a "Yes"/"No" value
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

#function to generate a document matrix for all the elements
#this is called for description and company profile variables
#the other two benefits and requirements are not needed - did not help in prediction
#corpus_all is the corpus created from the attribute(example from description) for all observations
#corpus_fraudulent is created only using the fake observations
get_dm <- function(corpus_all, corpus_fraudulent, word_cloud_freq, all_posting_freq, fake_posting_freq, fake_freq_dtm = TRUE){
  #generate word cloud for fake postings
  generate_word_cloud(corpus_fraudulent, word_cloud_freq)
  #create the document term matrix for all posting
  dtm_all <- DocumentTermMatrix(corpus_all, control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = function(x) { removeWords(x, stopwords(kind="en")) },
    removePunctuation = TRUE,
    stemming = TRUE
  ))
  #inspect(dtm_all)
  #create the document term matrix for fake posting
  dtm_fake <- DocumentTermMatrix(corpus_fraudulent, control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = function(x) { removeWords(x, stopwords(kind="en")) },
    removePunctuation = TRUE,
    stemming = TRUE
  ))
  inspect(dtm_fake)
  if(fake_freq_dtm)
    freq_words <- findFreqTerms(dtm_fake, fake_posting_freq)
  else
    freq_words <- findFreqTerms(dtm_all, all_posting_freq)
  return (dtm_all[, freq_words])
}

#get the document matrix for company profile
cp_corpus <- VCorpus(VectorSource(fjp_tb$company_profile))
cp_corpus_fake <- VCorpus(VectorSource(fraudulent_posting$company_profile))
lapply(cp_corpus_fake[1:3], as.character)

#100 = word cloud freq, 1000 term freq for all data
#500 term freq for fake data, FALSE - return dtm filtered for frequent all terms TRUE - return dtm filtered for frequent fake terms
cp_df_numer <- get_dm(cp_corpus, cp_corpus_fake,50,1000,100,TRUE) #get dtm using fake terms freq, **try by increasing 100 the thrid param
cp_df_categ <- apply(cp_df_numer, MARGIN = 2, convert_counts)

#get the document matrix for description
desc_corpus <- VCorpus(VectorSource(fjp_tb$description))
desc_corpus_fake <- VCorpus(VectorSource(fraudulent_posting$description))
lapply(desc_corpus_fake[1:3], as.character)

desc_df_numer <- get_dm(desc_corpus, desc_corpus_fake,100,3000,100,TRUE) #**try by increasing 100 the thrid param
desc_df_categ <- apply(desc_df_numer, MARGIN = 2, convert_counts)

#drop the text fields
fjp_tb$company_profile <- NULL
fjp_tb$description <- NULL

#not needed and not transformed to matrices
fjp_tb$requirements <- NULL
fjp_tb$benefits <- NULL

#check the structure of the tibble at this point
str(fjp_tb)

#this is our final dataset for modeling, all variables are factors
#here we may run in to some names that are common in both matrices, because some terms can be common in description
#and company profile like "busi"
colnames(desc_df_categ)
unlist(lapply(colnames(desc_df_categ), function(n){paste("desc_", n, sep = "")}))
#add prefix to the description matrix variable names to avoid issues
colnames(desc_df_categ) <- unlist(lapply(colnames(desc_df_categ), function(n){paste("desc_", n, sep = "")}))
colnames(desc_df_categ)

fjp_cleaned_categ <- cbind(fjp_tb, desc_df_categ, cp_df_categ)
#str(fjp_cleaned_categ)

#STEP 2 - MODELING, OPTIMIZATIONS & PERFORMANCE MEASURES
#install.packages("caret")
#install.packages("irr")
library(caret)
library(irr)

#Create ten folds and fit a model for each fold and report various performance measure for each fold
set.seed(123)
folds <- createFolds(fjp_tb$fraudulent, k = 10)
#Naive Bayes Cross Validation using the 10 folds
#install.packages(e1071)
library(e1071)
str(fjp_cleaned_categ)
cv_results_nb <- lapply(folds, function(x) {
  train_data <- fjp_cleaned_categ[-x, -9] #9 is the index for the label = fraudulent
  train_labels <- fjp_cleaned_categ[-x, 9]
  
  test_data <- fjp_cleaned_categ[x, -9]
  test_labels <- fjp_cleaned_categ[x, 9]
  
  fjp_nb_model <- naiveBayes(train_data, train_labels)
  test_pred <- predict(fjp_nb_model, newdata =  test_data, laplace = 1)
  
  kappa <- kappa2(data.frame(test_labels,test_pred))$value
  sen <- sensitivity(test_pred, test_labels, positive = "1")
  spec <- specificity(test_pred, test_labels, positive = "1")
  prec <- posPredValue(test_pred, test_labels, positive = "1")
  f1 <-F_meas(test_pred, test_labels, positive = "1")
  
  #Confusion Matrix for each fold
  #confusionMatrix(test_pred, test_labels, positive = "1")
  
  return(data.frame(kappa,sen,spec, prec, f1,"Naive Bayes"))
  
})
per_df <- rbind(cv_results_nb$Fold01, cv_results_nb$Fold02, cv_results_nb$Fold03, cv_results_nb$Fold04
                ,cv_results_nb$Fold05,cv_results_nb$Fold06, cv_results_nb$Fold07, cv_results_nb$Fold08,
                cv_results_nb$Fold09, cv_results_nb$Fold10)

names(per_df) <- c("Kappa", "Sensitivity","Specificity", "Precision", "F1_Score", "Algorithm")
per_df

#Group by algorithms used and report the mean for all folds
per_df %>% group_by(Algorithm) %>%
  summarise(kappa = mean(Kappa), sen = mean(Sensitivity), spec = mean(Specificity), pre = mean(Precision), f1_score = mean(F1_Score))

#We are using F1 score as a final performance indicator...

#####################################################################################################################
