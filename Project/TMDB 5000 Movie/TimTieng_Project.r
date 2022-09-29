# Code Author - Tim Tieng
# Group Number - Group E (Tim Tieng, Vanessa Caldera, Matthew Hill)
# Subject - IST 657 Intro to Data Science
# Assignment - Group Project

# Load Packages
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(readr)
library(lattice)
library(caret)
library(measures)
library(rpart)
library(rpart.plot)
library(kernlab)
library(quanteda)
library(quanteda.textplots)

# Create a dataframe from the movies.csv file
setwd("/Users/timtieng/Library/CloudStorage/OneDrive-Personal/Desktop/Masters in Applied Data Science/IST-687 Intro to Data Science/Project/TMDB 5000 Movie/Datasets")
movieFile <- "tmdb_5000_movies.csv"
moviesDF <- data.frame(read_csv(movieFile))

# Inspect to see if moviesDF was successfully created
str(moviesDF) # 4803 Rows, 20 Columns

# Columns: budget, genres, homepage, id, keywords, orginal_language, original_title, overview, popularity, production_companies, production_countries, release_date, revenue, runtime, 
# spoken_languages, status, tagline, title, vote_average, vote_count,


# Task 1 - Analyse how budget correlates with movie popularity
# Dependent Variable - popularity
# Independent Variable - budget

# Determine if there are any NA values in the two columns -- Expecting 0's
numNABudgetColumn <- sum(is.na(moviesDF$budget))
numNABudgetColumn
numNAPopularityColumn <- sum(is.na(moviesDF$popularity))
numNAPopularityColumn

# Visualize using Scatterplot
budgetVSPopularityPlot <- ggplot(moviesDF) +
                            geom_point(aes(x=budget, y=popularity)) + 
                            ggtitle("Budget Versus Popularity Rating")
budgetVSPopularityPlot

# Linear Regression Model
budgetLM <- lm(formula = popularity ~ budget, data=moviesDF)
summary(budgetLM)


# Visualize Populartiy vs additional ind. variables
genresVSPopularityPlot <- ggplot(moviesDF) +
                            geom_point(aes(x=genres, y=popularity)) + 
                            ggtitle("Genres Versus Popularity Rating")
genresVSPopularityPlot

voteAvgVSPopularityPlot <- ggplot(moviesDF) +
                            geom_point(aes(x=vote_average, y=popularity)) + 
                            ggtitle("Vote Average Versus Popularity Rating")
voteAvgVSPopularityPlot

voteCountVSPopularityPlot <- ggplot(moviesDF) +
                            geom_point(aes(x=vote_count, y=popularity)) + 
                            ggtitle("Vote Count Versus Popularity Rating")
voteCountVSPopularityPlot

releaseDateVSPopularityPlot <- ggplot(moviesDF) +
                            geom_point(aes(x=release_date, y=popularity)) + 
                            ggtitle("Release DateVersus Popularity Rating")
releaseDateVSPopularityPlot

revenueVSPopularityPlot <- ggplot(moviesDF) +
                            geom_point(aes(x=revenue, y=popularity)) + 
                            ggtitle("Revenue Versus Popularity Rating")
revenueVSPopularityPlot

# Linear Regrssion Model  - Multi Variable analysis (budget + genres + runtime)
budgetMultiVariable <- lm(formula = popularity ~ budget + genres + runtime + vote_average + vote_count + release_date + revenue, data = moviesDF)
summary(budgetMultiVariable)


# Task 2 - Use another technique to dtermine which attributes have the most influence on the dependent variable

# Create a predictable sample
set.seed(111)

# Create the indices to use to create two additional data sets
trainList <- createDataPartition(y = moviesDF$popularity, p = .80, list = FALSE)

#Create test and train data sets
trainSet <- moviesDF[trainList, ]
testSet <- moviesDF[-trainList, ]

# Inspect
str(trainSet)
str(testSet)
dim(trainSet)
dim(testSet)
head(trainSet)

# Build treebag model
fit1 <- train(popularity ~ budget, runtime, vote_average, vote_count, data = trainSet, method = "treebag",preProc = c("center", "scale"))

cartTree <- rpart(popularity ~ budget , runtime, vote_average, data = trainSet, method = "class")

prp(cartTree, faclen = 0, cex = 0.8, extra = 1)


# Analyze tagline column via wordcloud to understand initially what words are used - potential to shed light on type of genrea
movieTaglingCorpus <- corpus(moviesDF$tagline)
#summary(movieCorpus) # 43803 documents

# Format text to remove punctuation and numbers from corpous variable
movieTaglineCorpus <- tokens(movieTaglingCorpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
head(movieCorpus, 10)

# Ensure all words are the same case
movieTaglineCorpus <- tokens_tolower(movieTaglineCorpus)

# remove stop words
movieTaglineCorpus <- tokens_select(movieTaglineCorpus, selection = "remove", pattern = stopwords("en"))
head(movieCorpus)

# Create Document Matrix
taglineMatrix <- dfm(movieTaglineCorpus)
taglineMatrix

taglineWordCloud <- textplot_wordcloud(taglineMatrix, min_count =25)

# Analyze genres column
genreCorpus <- corpus(moviesDF$genres)
summary(genreCorpus) # 43803 documents
head(genreCorpus)
# Format text to remove punctuation and numbers from corpous variable
genreCorpus <- tokens(genreCorpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
head(genreCorpus, 10)

# Ensure all words are the same case
genreCorpus <- tokens_tolower(genreCorpus)

# remove stop words
genreCorpus <- tokens_select(genreCorpus, selection = "remove", pattern = stopwords("en"))
head(movieCorpus)

# Create Document Matrix
genreMatrix <- dfm(genreCorpus)
genreMatrix

genreWordCloud <- textplot_wordcloud(genreMatrix, min_count = 20)
