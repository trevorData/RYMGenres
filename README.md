Music fans are generally aware of the idea of a [music genre genealogy tree](https://en.wikipedia.org/wiki/Genealogy_of_musical_genres), with parent genres sprouting branches made up of the genres that they influenced. For example, a branch might go Blues -> Rock & Roll -> Hard Rock -> Metal and so on.

But while the general shape of such tree might be agreed upon by listeners, the specifics could be up for debate. This is because music genres don't literally "evolve" the way animals do; artists may be influenced by one another, but ultimately they write their songs independently. Because of this, any proposed "genealogy" will be somewhat arbitrary.

**With this project, I hoped to construct a music genealogy tree that ignored the idea of influence entirely, focusing only on the sound of the music itself**.

---

# Methodology:

[Rate Your Music](https://rateyourmusic.com) is a popular music database and social media platform, where users rate and discuss music. For each album on the site, users are able to vote on which genres best suit the album, as well as a set of adjectives that best describe it.

Using this data, I calculated the likelihood of each descriptor appearing for a given genre. A Heavy Metal album for example would most likely be described by the users as "Anthemic", "Melodic", "Energetic" and "Heavy" (duh). If a genre was *always* described as "Melodic" it would get assigned a 1 for "Melodic" and a 0 if it was *never* described that way.

There were 251 different descriptors in total, and I used the likelihoods I found to assign coordinates to each genre that placed each of them at a point in 251-dimensional space. 

Genres that were often described the same way would end up near each other in the coordinate space, and using a [Hierarchical Clustering](https://en.wikipedia.org/wiki/Hierarchical_clustering) algorithm, I created a tree plot to show exactly how close they were.

---

# Findings:

# [Main Plot](https://imgur.com/7VikfHq.jpg)
#### [Red Branch](https://imgur.com/NE7DiMf.jpg), [Yellow Branch](https://imgur.com/ehq1Tbb.jpg), [Green Branch](https://imgur.com/8jXn7IV.jpg), [Blue Branch](https://imgur.com/I86liCs.jpg), [Purple Branch](https://imgur.com/4dJJkGr.jpg)

The similarity of two genres is indicated on the tree by where their branches split apart. Indie-Rock and Art-Rock were very similar, so they dont split off until the end of the tree. Techno and Indie-Rock were very dissimillar, so their respective branches split off right at the beginning of the tree.

Looking at the tree, we can see that the results are roughly similar to how music geneology is traditionally conceived, with some key differences:

All of the descriptors were weighted equally, so genres that differ in an area that is usually considered important will be placed close together if they are similar everywhere else. Pop Rap and Synthpop for example, are close together despite the differing vocal deliveries. This would be analogous to an evolutionary tree putting dolphins and sharks close together despite key differences in breathing and reproduction.

Similarily, because the algorithm uses no concept of "influence", genres that may have sprung from the same movement but sound very different will be placed far apart on the graph. Grunge and Post-Grunge are farther apart than one would expect. Going back to the biology example, this would be like placing whales and hippos far apart despite the evolutionary link.

---

##### Bonus: What if we ran the same analysis using only the descriptors that describe lyrical content and themes?
 
##### [Plot](https://imgur.com/kVUShaN.jpg)

---

# Sources:

Data was compiled manually from rateyourmusic.com

Analysis and visualization done in R using the packages:  
dplyr  
tm  
dendextend

View my code [here](https://github.com/trevorData/RYMGenres)
