#Setup the working directory
#This is not necessary if the data is stored in the same folder where this script is saved and you open by
# double clicking the saved script.

#setwd("/Users/mrahman/Documents/Teaching/Purdue/Web Data Analytics/Lectures/Working Versions")

library(ggplot2)
library(dplyr)

#########################
#   Load DATA           #
#   Explore             #
#########################

#Load data

setwd('/Users/sreekailash/Documents/MSBAIM/Fall Module 2/MGMT 590 Web data/Final Project/')

df <- read.csv("input_df.csv", sep=",", header=T); # data is loaded to a frame

summary(df)
df = df[ , !(names(df) %in% c('X'))]

str(df)

df = transform(df, pages = as.numeric(pages))
df = transform(df, numRatings = as.numeric(numRatings))
df = transform(df, price = as.numeric(price))

#########################
#  MYSTERY VS SCIENCE FICTION  #
#########################

t.test(df[df$Mystery == 1 & df$Science.Fiction ==0,]['to.read'], df[df$Science.Fiction == 1 & df$Mystery == 0,,]['to.read'])

df = df[df$to.read >=0,]

df$ltoread = log(df$to.read)
df$lpages = log(1+df$pages)

r0 <- lm(ltoread ~ rating+lpages+numRatings+price+description_num_words+
           description_sentiment_polarity + Fiction+Romance+Fantasy+Contemporary+
           Nonfiction+Historical.Fiction+Mystery+Adult+Paranormal+Science.Fiction,data = df) #simple model
summary(r0)

r1 <- lm(ltoread~pages, data = df)



