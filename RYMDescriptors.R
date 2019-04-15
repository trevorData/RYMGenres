setwd("~/Projects/RYMGenres")

library(dplyr)
library(tm)

raw.data <- read.csv("data.csv", encoding = 'UTF-8')
data <- raw.data %>% subset(select=-c(release_date, score, num_ratings))
names(data)[1] <- "artist"

data[, 9:ncol(data)] <- gsub(" ", "_", as.matrix(data[, 9:ncol(data)]))
data$full <- data[, 9:ncol(data)] %>% apply(1, paste, collapse=" ")

corpus <- VCorpus(VectorSource(data$full))
dtm <- DocumentTermMatrix(corpus)

data.encoded <- data[, 1:8] %>% cbind(as.data.frame(as.matrix(dtm)))

# create grouped df with count of descriptors for genre1
genre1.group <- data.encoded[-(1:8)] %>% aggregate(by=list(data.encoded$genre1), FUN=sum)
genre1.count <- data.encoded[-(1:8)] %>% aggregate(by=list(data.encoded$genre1), FUN=NROW)

# create grouped df with count of descriptors for genre2
genre2.group <- data.encoded[data.encoded$genre2!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$genre2 != "",]$genre2), FUN=sum)
genre2.count <- data.encoded[data.encoded$genre2!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$genre2!="",]$genre2), FUN=NROW)

# create grouped df with count of descriptors for secgenre1
secgenre1.group <- data.encoded[data.encoded$sec_genre1!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre1 != "",]$sec_genre1), FUN=sum)
secgenre1.group[-1] <- secgenre1.group[-1] * .5
secgenre1.count <- data.encoded[data.encoded$sec_genre1!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre1 != "",]$sec_genre1), FUN=NROW)

# create grouped df with count of descriptors for secgenre2
secgenre2.group <- data.encoded[data.encoded$sec_genre2!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre2 != "",]$sec_genre2), FUN=sum)
secgenre2.group[-1] <- secgenre2.group[-1] * .5
secgenre2.count <- data.encoded[data.encoded$sec_genre2!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre2 != "",]$sec_genre2), FUN=NROW)

# create grouped df with count of descriptors for secgenre3
secgenre3.group <- data.encoded[data.encoded$sec_genre3!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre3 != "",]$sec_genre3), FUN=sum)
secgenre3.group[-1] <- secgenre3.group[-1] * .5
secgenre3.count <- data.encoded[data.encoded$sec_genre3!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre3 != "",]$sec_genre3), FUN=NROW)

# create grouped df with count of descriptors for secgenre4
secgenre4.group <- data.encoded[data.encoded$sec_genre4!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre4 != "",]$sec_genre4), FUN=sum)
secgenre4.group[-1] <- secgenre4.group[-1] * .5
secgenre4.count <- data.encoded[data.encoded$sec_genre4!="",][-(1:8)] %>% aggregate(by=list(data.encoded[data.encoded$sec_genre4 != "",]$sec_genre4), FUN=NROW)

# append all the grouped dfs together into one big grouped df, take only .5 of the count from the subgenre dfs
all.genres <- do.call("rbind", list(genre1.group, genre2.group, secgenre1.group, secgenre2.group, secgenre3.group, secgenre4.group))
all.count <- do.call('rbind', list(genre1.count, genre2.count, secgenre1.count, secgenre2.count, secgenre3.count, secgenre4.count))

final.genres <- all.genres[-1] %>% aggregate(by=list(all.genres$Group.1), FUN=sum)
final.count <- all.count[-1] %>% aggregate(by=list(all.count$Group.1), FUN=sum)

# divide final by final.count to get likelyhood of each descriptor appearing for an album in each genre
final <- final.genres
final[-1] <- final[-1]/final.count[-1]

# create clusters

# visualize dendrogram












