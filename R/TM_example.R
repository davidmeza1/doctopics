# packages needed for analysis
library(topicmodels)
library(tm)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(doBy)
library(Rmpfr)
library(slam)
library("stringr")


## The category colum contained multiple entries. For this demonstration I extracted the first
## category and used it for correlation. I use all categories for the graph DB
## Load the data file to start the analysis
## You need to load the magrittr package. I can not use magrittr::

load("data/llisDisplay.RData")
head(llis.display)
tmp <- data.frame()
CategorybyLesson <- data.frame()
for (i in 1:length(rownames(llis.display))) {
  tmp <- stringr::str_split(llis.display$Categories[i], ",") %>%
    unlist(tmp)
    tmp <- cbind(tmp, as.integer(llis.display$LessonId[i]))
    tmp <- as.data.frame(tmp)
    CategorybyLesson <- plyr::rbind.fill(CategorybyLesson, tmp)
    tmp <- data.frame()
}
colnames(CategorybyLesson) <- c("Category", "LessonId")
CategorybyLesson$LessonId <- as.character(CategorybyLesson$LessonId)
CategorybyLesson$LessonId <- as.numeric(CategorybyLesson$LessonId)
CategorybyLesson<- dplyr::filter(CategorybyLesson, Category != "")
CategorybyLesson<- dplyr::filter(CategorybyLesson, Category != " ")

# Create data frame of first category for simplicity and use in corrleation
tmp <- data.frame()
FirstCategorybyLesson <- data.frame()
for (i in 1:length(rownames(llis.display))) {
  tmp <- stringr::str_split(llis.display$Categories[i], ",") %>%
    unlist(tmp)
  tmp <- cbind(tmp[1], as.integer(llis.display$LessonId[i]))
  tmp <- as.data.frame(tmp)
  FirstCategorybyLesson <- plyr::rbind.fill(FirstCategorybyLesson, tmp)
  tmp <- data.frame()
}
colnames(FirstCategorybyLesson) <- c("Category", "LessonId")
FirstCategorybyLesson$LessonId <- as.character(FirstCategorybyLesson$LessonId)
FirstCategorybyLesson$LessonId <- as.numeric(FirstCategorybyLesson$LessonId)
FirstCategorybyLesson$Category <- as.character(FirstCategorybyLesson$Category)
# FirstCategorybyLesson<- dplyr::filter(FirstCategorybyLesson, Category != "")
for (i in 1:1637){
     if (FirstCategorybyLesson[i,1] == "") { FirstCategorybyLesson[i,1] <-
          c("Unknown")
     }
}
FirstCategorybyLesson$Category <- as.factor(FirstCategorybyLesson$Category)
######


## Since I am importing a spreadsheet I need to Create a mapping for the corpus. The first item is the
## content used for the modeling, the rest are added as meta data
m <- list(content = "Lesson", id = "LessonId", heading = "Title", authors = "Submitter1", topic = "Categories",
          abstract = "Abstract", org = "Organization", data = "LessonDate", dir = "MissionDirectorate",
          safetyissue = "SafetyIssue", docnum = "DocNum", topic = "Topic")
## Create the Corpus
llis.corpus <- tm::Corpus(tm::DataframeSource(llis.display), readerControl = list(reader = tm::readTabular(mapping = m)))
## Create the document term matrix. The final dtm will be used in the LDA model.
llistopic.dtm <- tm::DocumentTermMatrix(llis.corpus, control = list(stemming = TRUE, stopwords = TRUE,
                                    minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))
# I decided to use all the terms and let the tf-idf reduce it
## Reduces dtm to words occurring in at least five docs
#   llisreduced.dtm <-llistopic.dtm[ , which(table(llistopic.dtm$j) >= 5)]

## The mean term frequency-inverse document frequency (tf-idf) over documents containing
## this term is used to select the vocabulary. This measure allows to omit terms which have low
## frequency as well as those occurring in many documents. We only include terms which
## have a tf-idf value of at least 0.2 which is a bit more than the median and ensures that the very
## frequent terms are omitted.
term_tfidf <- tapply(llistopic.dtm$v/slam::row_sums(llistopic.dtm)[llistopic.dtm$i], llistopic.dtm$j, mean) *
  log2(tm::nDocs(llistopic.dtm)/slam::col_sums(llistopic.dtm > 0))
summary(term_tfidf)
## Keeping the rows with tfidf >= to the 0.155
llisreduced.dtm <- llistopic.dtm[,term_tfidf >= 0.155]
## Keeps rows with sum greater than 0
#llisreduced.dtm <- llisreduced.dtm[slam::row_sums(llisreduced.dtm) > 0,]
summary(slam::col_sums(llisreduced.dtm))

## Regarding the general question of optimal topic numbers, I now follow the example of Martin Ponweiser on
## Model Selection by Harmonic Mean (4.3.3 in his thesis, which is here: http://epub.wu.ac.at/3558/1/main.pdf).
##
## The harmonic mean function
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
## As an example of finding the harmonic mean for one value of k, using a burnin of 1000 and iterting
## 1000 times andhere keep indicates that every keep iteration the log-likelihood is evaluated and stored.
## The log-likelihood values are then determined by first fitting the model. This returns all
## log-likelihood values including burnin, i.e., these need to be omitted before calculating the
## harmonic mean:
k <- 25
burnin <- 1000
iter <- 1000
keep <- 50
fitted <- topicmodels::LDA(llisreduced.dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
## assuming that burnin is a multiple of keep
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

## This returns the harmomnic mean for k = 25 topics.
harmonicMean(logLiks)

## To find the best value for k, we do this over a sequence of topic models with different vales for k
## To generate numerous topic models with different numbers of topics, create a vector to hold the k values.
## In this case, a sequence of numbers from 2 to 100, by ones.
## Using the lapply function, run the LDA function using all the values of k.
## To see how much time is needed to run the process on your system, sue the system.time function.
## On my system, an iMAC, running OS X 10.10.3, i5 2.9 GHz and 32GB ram it took
## user   system  elapsed
## 2034.002    2.469 2047.612
seqk <- seq(2, 100, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(llisreduced.dtm, k = k,
                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                         iter = iter, keep = keep) )))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# Using ggplot 2 we construct a plot to inspect the harmonic means
ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
       theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
     annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", atop(italic("How many distinct topics in the abstracts?"), ""))))
ldaplot

# compute optimum number of topics
seqk[which.max(hm_many)]
## 35

## Run the LDA model using the optimal number of topics, 35. The Gibbs sampling method is used for estimation.
## The model will run through 2000 iterations, using the seed for reproducability. The system time was:
##    user  system elapsed
##  14.005   0.013  14.033
system.time(llis.model <- topicmodels::LDA(llisreduced.dtm, 27, method = "Gibbs", control = list(iter=2000, seed = 0622)))
save(llis.model, file = "data/llismodel.RData")

## You use the topics function from the topicmodels package to extract the most likely topic
## for each documents.
llis.topics <- topicmodels::topics(llis.model, 1)
# Creates a dataframe to store the Lesson Number and the most likely topic
doctopics.df <- as.data.frame(llis.topics)
doctopics.df <- dplyr::transmute(doctopics.df, LessonId = rownames(doctopics.df), Topic = llis.topics)
doctopics.df$LessonId <- as.integer(doctopics.df$LessonId)

## You use the terms function to extract the terms per topic, supplying the model and number of terms
## to return. In this case I am returning the top 30 terms.
llis.terms <- as.data.frame(topicmodels::terms(llis.model, 30), stringsAsFactors = FALSE)
llis.terms[1:5]
## Adds topic number to original dataframe of lessons
llis.display <- dplyr::inner_join(llis.display, doctopics.df, by = "LessonId")
## create Label for each topic
topicTerms <- tidyr::gather(llis.terms, Topic)
topicTerms <- cbind(topicTerms, Rank = rep(1:30))
topTerms <- dplyr::filter(topicTerms, Rank < 4)
topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
topTerms$Topic <- as.numeric(topTerms$Topic)
topicLabel <- data.frame()
for (i in 1:27){
     z <- dplyr::filter(topTerms, Topic == i)
     l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2], sep = " " ), stringsAsFactors = FALSE)
     topicLabel <- rbind(topicLabel, l)

}
colnames(topicLabel) <- c("Label")

## Dataframe of theta, the per-document probabilities from topics from the fitted model's posteriors
theta <- as.data.frame(topicmodels::posterior(llis.model)$topics)

## Working with theta. What can we do woth theta
## Theta mean for topic probabilities based on the documents grouped by the Category
## Adding Category column to theta2, keeping original theta for future use
x <- as.data.frame(row.names(theta), stringsAsFactors = FALSE)
colnames(x) <- c("LessonId")
x$LessonId <- as.numeric(x$LessonId)
theta2 <- cbind(x, theta)
theta2 <- dplyr::left_join(theta2, FirstCategorybyLesson, by = "LessonId")
## Returns column means grouped by catergory
theta.mean.by <- by(theta2[, 2:28], theta2$Category, colMeans)
theta.mean <- do.call("rbind", theta.mean.by)

## The corrplot package provides different schemes for plotting correlation.
## We have run correlation on the theta mean by category to see how the Topics are corrleated
library(corrplot)
c <- cor(theta.mean)
corrplot(c, method = "circle")

## With all categories' mean theta available, it is now possible to select the most diagnostic
## (i.e., representative) topics of each of these categories

theta.mean.ratios <- theta.mean
for (ii in 1:nrow(theta.mean)) {
  for (jj in 1:ncol(theta.mean)) {
    theta.mean.ratios[ii,jj] <-
      theta.mean[ii,jj] / sum(theta.mean[ii,-jj])
  }
}
topics.by.ratio <- apply(theta.mean.ratios, 1, function(x) sort(x, decreasing = TRUE, index.return = TRUE)$ix)

# The most diagnostic topics per category are found in the theta 1st row of the index matrix:
topics.most.diagnostic <- topics.by.ratio[1,]

# We can now reduce the mean theta matrix to just the most diagnostic topics per category:
theta.diagnostic <-theta.mean.ratios[,topics.most.diagnostic]
colnames(theta.diagnostic) <- topics.most.diagnostic

## The LDAvis package provides an excellant way to visualize the topics and terms associated with them.
## See https://github.com/cpsievert/LDAvis for more information.
## However converting your values to work with LDAvis can be daunting. I found a function at this site,
## http://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/,
#' To make the conversion from topicmodels output to LDAvis JSON input easier. The linking function,
#' called topicmodels_json_ldavis, is below. To use it follow these steps:
#'
#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param corpus Corpus object used to create the document term
#' matrix for the \code{LDA} model. This should have been create with
#' the tm package's \code{Corpus} function.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

## Load the function.
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
     # Required packages
     library(topicmodels)
     library(dplyr)
     library(stringi)
     library(tm)
     library(LDAvis)

     # Find required quantities
     phi <- posterior(fitted)$terms %>% as.matrix
     theta <- posterior(fitted)$topics %>% as.matrix
     vocab <- colnames(phi)
     doc_length <- vector()
     for (i in 1:length(corpus)) {
          temp <- paste(corpus[[i]]$content, collapse = ' ')
          doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
     }
     temp_frequency <- inspect(doc_term)
     freq_matrix <- data.frame(ST = colnames(temp_frequency),
                               Freq = colSums(temp_frequency))
     rm(temp_frequency)

     # Convert to json
     json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length,
                                    term.frequency = freq_matrix$Freq)

     return(json_lda)
}
## Create a json file to pass to LDAvis, supplying the fitted model, corpus and document term matrix used.
## I have an issue with the number of items in the corpus not equal to the number of items in the dtm or
## the model. I had to for the corpus to use the same number of elements in order to do the calculation.
## I am still working to find out where the isse occured.
llis.json <- topicmodels_json_ldavis(llis.model, llis.corpus, llisreduced.dtm)
serVis(llis.json)

