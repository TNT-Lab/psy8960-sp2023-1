# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Import and Cleaning
raw_df <- read.csv("../data/week3.csv")
raw_df$timeStart <- as.POSIXct(raw_df$timeStart)
raw_df$timeEnd <- as.POSIXct(raw_df$timeEnd)
clean_df <- raw_df[raw_df$timeStart > "2017-07-01 00:00:00",]
clean_df <- clean_df[clean_df$q6 == 1,]

# Analysis
clean_df$timeSpent <- difftime(clean_df$timeEnd, clean_df$timeStart, units="secs")


# Alternatives
clean_df <- raw_df[!grepl("2017-06", raw_df$timeStart),]
clean_df$timeSpent <- as.numeric(clean_df$timeEnd - clean_df$timeStart) * 60
