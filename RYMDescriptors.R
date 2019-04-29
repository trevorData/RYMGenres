setwd("~/Projects/RYMGenres")

library(dplyr)
library(tm)
library(dendextend)

raw.data <- read.csv("fulldata.csv", encoding = 'UTF-8')
data <- raw.data %>% subset(select=-c(release_date, score, num_ratings))
names(data)[1] <- "artist"

data[, 10:ncol(data)] <- gsub(" ", "_", as.matrix(data[, 10:ncol(data)]))
data$full <- data[, 9:ncol(data)] %>% apply(1, paste, collapse=" ")

corpus <- VCorpus(VectorSource(data$full))
dtm <- DocumentTermMatrix(corpus)

data.encoded <- data[, 1:9] %>% cbind(as.data.frame(as.matrix(dtm)))

# create grouped df with count of descriptors for genre1
genre1.group <- data.encoded[-(1:9)] %>% aggregate(by=list(data.encoded$genre1), FUN=sum)
genre1.count <- data.encoded[-(1:9)] %>% aggregate(by=list(data.encoded$genre1), FUN=NROW)

# create grouped df with count of descriptors for genre2
genre2.group <- data.encoded[data.encoded$genre2!="",][-(1:9)] %>% aggregate(by=list(data.encoded[data.encoded$genre2 != "",]$genre2), FUN=sum)
genre2.count <- data.encoded[data.encoded$genre2!="",][-(1:9)] %>% aggregate(by=list(data.encoded[data.encoded$genre2!="",]$genre2), FUN=NROW)

# create grouped df with count of descriptors for genre3
genre3.group <- data.encoded[data.encoded$genre3!="",][-(1:9)] %>% aggregate(by=list(data.encoded[data.encoded$genre3 != "",]$genre3), FUN=sum)
genre3.count <- data.encoded[data.encoded$genre3!="",][-(1:9)] %>% aggregate(by=list(data.encoded[data.encoded$genre3!="",]$genre3), FUN=NROW)

# append all the grouped dfs together into one big grouped df
all.genres <- do.call("rbind", list(genre1.group, genre2.group, genre3.group))
all.count <- do.call('rbind', list(genre1.count, genre2.count, genre3.count))

final.genres <- all.genres[-1] %>% aggregate(by=list(all.genres$Group.1), FUN=sum)
final.count <- all.count[-1] %>% aggregate(by=list(all.count$Group.1), FUN=sum)

# divide final by final.count to get likelyhood of each descriptor appearing for an album in each genre
final <- final.genres
final[-1] <- final[-1]/final.count[-1]

# set rownames
rownames(final) <- final$Group.1
final <- final[-1]

genres <- c(
  'Ambient',
  'Acoustic Rock',
  'Alternative R&B',
  'Art Pop',
  'Art Rock',
  'Atmospheric Black Metal',
  'Avant-Garde Metal',
  'Baroque Pop',
  'Black Metal',
  'Blues Rock',
  'Breakbeat',
  'Britpop',
  'Cloud Rap',
  'Crust Punk',
  'Death Metal',
  'Deep House',
  'Doom Metal',
  'Dream Pop',
  'Drone Metal',
  'Drone',
  'Drum and Bass',
  'Dubstep',
  'East Coast Hip Hop',
  'Electro',
  'Emo',
  'Experimental Hip Hop',
  'Folk Metal',
  'Folk Punk',
  'Funk',
  'G-Funk',
  'Garage Rock',
  'Glam Metal',
  'Glam Rock',
  'Grime',
  'Grindcore',
  'Grunge',
  'Hardcore [EDM]',
  'Heavy Metal',
  'Heavy Psych',
  'House',
  'IDM',
  'Indie Folk',
  'Indie Pop',
  'Indie Rock',
  'Industrial',
  'Instrumental Hip Hop',
  'Jam Band',
  'Jazz Rap',
  'K-Pop',
  'Krautrock',
  'Math Rock',
  'Melodic Death Metal',
  'Metalcore',
  'Midwest Emo',
  'Neofolk',
  'Neo-Soul',
  'New Wave',
  'No Wave',
  'Noise',
  'Nu Metal',
  'Pop Punk',
  'Pop Rap',
  'Post-Grunge',
  'Post-Hardcore',
  'Post-Punk',
  'Post-Rock',
  'Power Metal',
  'Progressive Rock',
  'Psychedelic Rock',
  'Punk Rock',
  'Rap Metal',
  'Screamo',
  'Shoegaze',
  'Singer/Songwriter',
  'Ska Punk',
  'Slowcore',
  'Soul',
  'Southern Hip Hop',
  'Southern Rock',
  'Surf Rock',
  'Synthpop',
  'Techno',
  'Thrash Metal',
  'Trance',
  'Trap [EDM]',
  'Trap Rap',
  'Trip Hop',
  'UK Garage',
  'UK Hip Hop',
  'Vaporwave',
  'Witch House')

# Pare down to only important genres
final <- final[genres,]
  
# create clusters
dend <- as.dendrogram(hclust(dist(final, method = 'euclidean'), method = 'ward.D'))

# visualize dendrogram
dend %>% set('branches_k_color', k=6) %>% plot(horiz=F, xlab='Distance')

(dend %>% cut(h=6))$lower[[1]] %>% set('branches_k_color', value=c('red', 'red4', 'lightpink'), k=3) %>% plot(horiz=F)

(dend %>% cut(h=6))$lower[[2]] %>% set('branches_k_color', value=c('yellow3', 'yellow4', 'khaki'), k=3) %>% plot(horiz=F)

(dend %>% cut(h=6))$lower[[3]] %>% set('branches_k_color', value=c('chartreuse3', 'forestgreen', 'darkolivegreen3'), k=3) %>% plot(horiz=F)

(dend %>% cut(h=6))$lower[[4]] %>% set('branches_k_color', value=c('seagreen2', 'seagreen4', 'darkseagreen2'), k=3) %>% plot(horiz=F)

(dend %>% cut(h=6))$lower[[5]] %>% set('branches_k_color', value=c('royalblue1', 'royalblue4', 'paleturquoise3'), k=3) %>% plot(horiz=F)

(dend %>% cut(h=6))$lower[[6]] %>% set('branches_k_color', value=c('orchid4', 'slateblue4', 'thistle3'), k=3) %>% plot(horiz=F)
