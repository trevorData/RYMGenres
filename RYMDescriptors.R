setwd("~/Projects/RYMGenres")

library(dplyr)
library(tm)
library(dendextend)
library(cluster)

# Load and format raw data
raw.data <- read.csv("fulldata.csv", encoding = 'UTF-8')
data <- raw.data %>% subset(select=-c(release_date, score, num_ratings))
names(data)[1] <- "artist"

# Binary encode the descriptors for each album
data[, 10:ncol(data)] <- gsub(" ", "_", as.matrix(data[, 10:ncol(data)]))
data$full <- data[, 9:ncol(data)] %>% apply(1, paste, collapse=" ")

corpus <- VCorpus(VectorSource(data$full))
dtm <- DocumentTermMatrix(corpus)

data.encoded <- data[, 1:9] %>% cbind(as.data.frame(as.matrix(dtm)))

# Count the occurence of each descriptor in each genre, and count the total entries for each genre
genre.group <- data.encoded[-(1:9)] %>% aggregate(by=list(data.encoded$genre1), FUN=sum)
genre.count <- data.encoded[-(1:9)] %>% aggregate(by=list(data.encoded$genre1), FUN=NROW)

# Calculate likelyhood of each descriptor appearing for a given album in each genre
final <- genre.group 
final[-1] <-  genre.group[-1]/genre.count[-1]

# Fix rownames
rownames(final) <- final$Group.1
final <- final[-1]

# Subset data to only the important genres
genres <- c(
  'Ambient',
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

final <- final[genres,]
  
# Create clusters
dend <- final %>% dist(method = 'euclidean') %>% hclust(method = 'ward.D') %>% as.dendrogram

# visualize dendrogram
par(mar=c(1, 4, 1, 9), cex = .5)
dend %>% set('branches_k_color', k=5)  %>% plot(horiz=T, xlab='', axes = F)

# Zoom in on dendrogram branches
par(mar=c(1, 4, 1, 10), cex = .8)
(dend %>% cut(h=7.5))$lower[[1]] %>% set('branches_k_color', value=c('red', 'lightpink'), k=2) %>% set('branches_lwd', 2) %>% plot(horiz=T, axes = F)

(dend %>% cut(h=7.5))$lower[[2]] %>% set('branches_k_color', value=c('yellow4', 'khaki'), k=2) %>% set('branches_lwd', 2) %>% plot(horiz=T, axes = F)

(dend %>% cut(h=7.5))$lower[[3]] %>% set('branches_k_color', value=c('seagreen2', 'seagreen4', 'darkseagreen2'), k=3) %>% set('branches_lwd', 2) %>% plot(horiz=T, axes = F)

(dend %>% cut(h=7.5))$lower[[4]] %>% set('branches_k_color', value=c('royalblue1', 'royalblue4', 'paleturquoise3'), k=3)  %>% set('branches_lwd', 2) %>% plot(horiz=T, axes = F)

(dend %>% cut(h=7.5))$lower[[5]] %>% set('branches_k_color', value=c('slateblue4', 'thistle3', 'darkorchid4'), k=3) %>% set('branches_lwd', 2) %>% plot(horiz=T, axes = F)

# Create a new dendrogram using only descriptors for lyrical content/tone
lyrical_desc <- c('angry',
                  'aggressive',
                  'anxious',
                  'bittersweet',
                  'calm',
                  'meditative',
                  'disturbing',
                  'energetic',
                  'manic',
                  'happy',
                  'playful',
                  'lethargic',
                  'longing',
                  'mellow',
                  'soothing',
                  'passionate',
                  'quirky',
                  'romantic',
                  'sad',
                  'depressive',
                  'lonely',
                  'melancholic',
                  'sombre',
                  'sensual',
                  'sentimental',
                  'uplifting',
                  'triumphant',
                  'abstract',
                  'alienation',
                  'conscious',
                  'crime',
                  'death',
                  'suicide',
                  'drugs',
                  'alcohol',
                  'fantasy',
                  'folklore',
                  'hedonistic',
                  'history',
                  'christmas',
                  'halloween',
                  'anti-religious',
                  'pagan',
                  'political',
                  'anarchism',
                  'nationalism',
                  'protest',
                  'religious',
                  'christian',
                  'satanic',
                  'introspective',
                  'lgbt',
                  'love',
                  'breakup',
                  'misanthropic',
                  'mythology',
                  'nature',
                  'occult',
                  'paranormal',
                  'patriotic',
                  'philosophical',
                  'existential',
                  'nihilistic',
                  'science_fiction',
                  'self-hatred',
                  'sexual',
                  'sports',
                  'violence',
                  'war',
                  'apathetic',
                  'boastful',
                  'cryptic',
                  'deadpan',
                  'hateful',
                  'humorous',
                  'optimistic',
                  'pessimistic',
                  'poetic',
                  'rebellious',
                  'sarcastic',
                  'satirical',
                  'serious',
                  'vulgar',
                  'apocalyptic',
                  'cold',
                  'dark',
                  'funereal',
                  'infernal',
                  'ominous',
                  'scary',
                  'epic',
                  'ethereal',
                  'futuristic',
                  'hypnotic',
                  'martial',
                  'mechanical',
                  'medieval',
                  'mysterious',
                  'natural',
                  'aquatic',
                  'desert',
                  'forest',
                  'rain',
                  'tropical',
                  'nocturnal',
                  'party',
                  'pastoral',
                  'peaceful',
                  'psychedelic',
                  'ritualistic',
                  'seasonal',
                  'autumn',
                  'spring',
                  'summer',
                  'winter',
                  'space',
                  'spiritual',
                  'surreal',
                  'suspenseful',
                  'tribal',
                  'urban',
                  'warm'
)

# Subset only columns that describe lyrics/tone
final_l <- final[lyrical_desc]

# create clusters
dend_l <- final_l %>% dist(method = 'euclidean') %>% hclust(method = 'ward.D') %>% as.dendrogram

# visualize dendrogram
par(mar=c(1, 4, 1, 9), cex = .5)
dend_l %>% set('branches_k_color', k=5)  %>% plot(horiz=T, xlab='', axes = F)
