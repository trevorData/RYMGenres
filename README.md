# [Main Plot](https://imgur.com/7VikfHq.jpg)

#### [Red Branch](https://imgur.com/NE7DiMf.jpg), [Yellow Branch](https://imgur.com/ehq1Tbb.jpg), [Green Branch](https://imgur.com/8jXn7IV.jpg), [Blue Branch](https://imgur.com/I86liCs.jpg), [Purple Branch](https://imgur.com/4dJJkGr.jpg)

---

### Using Machine Learning to Create a Music Genre Family Tree

Music listeners are usually familiar with the idea of a [music genre genealogy tree](https://en.wikipedia.org/wiki/Genealogy_of_musical_genres), with parent genres sprouting branches made up of the genres that they influenced. For example, a branch might go Blues -> Rock & Roll -> Hard Rock -> Metal and so on.

But while the general shape of the tree might be agreed upon by listeners, the specifics are up for debate. 

This is because music is subjective; genres don't literally "evolve" the way animals do. Artists might be influenced by one another, but ultimately they write their songs independently. Because of this, any proposed "genealogy" will be somewhat arbitrary.

**In this project, I hoped to construct a music family tree that ignored the idea of "influence" entirely, focusing only on descriptions of the music itself**.

---

# Methodology:

[Rate Your Music](https://rateyourmusic.com) is a popular music database and social media platform, where users rate and discuss music. For each album on the site, users are able to vote on which genres best suit the album, as well as a set of adjectives that best describe it.

Using this data, I calculated the likelihood of each descriptor appearing for a given genre. A Heavy Metal album for example would most likely be described by the users as "Anthemic", "Melodic", "Energetic" and "Heavy" (duh). There were 251 available descriptors, and I used the probabilities I found to assign 251 features to each genre. 

Genres that were often described similarly would end up near each other in the 251-D coordinate space, and using a [Hierarchical Clustering](https://en.wikipedia.org/wiki/Hierarchical_clustering) algorithm, I created a dendrogram to show exactly how close they were.

---

# Findings:

The similarity of two genres is indicated on the tree by where their branches split apart. Indie-Rock and Art-Rock were very similar, so they dont split off until the end of the tree. Techno and Indie-Rock were dissimillar, so their respective branches split off right at the beginning of the tree.

Looking at the tree, we can see that the results are roughly similar to how listeners usually think of music geneology , with some key differences:

All of the descriptors were weighted equally, so genres that differ in an area that is usually considered important will be placed close together if they are similar everywhere else. For example: Pop Rap and Synthpop are close together despite the differing styles of vocals. This would be analogous to an evolutionary tree putting dolphins and sharks close together despite key differences in breathing and reproduction.

Similarily, because the algorithm has no concept of "influence", genres that may have sprung from the same movement but sound very different will be placed far apart on the graph. Grunge and Post-Grunge are farther apart than one would expect. Going back to a biology analogy, this would be like placing whales and hippos far apart despite the evolutionary link.

---

##### Bonus: What if we ran the same analysis using only the descriptors that describe lyrical content?
 
##### [Plot](https://imgur.com/kVUShaN.jpg)

---

# Sources:

Data was compiled manually from rateyourmusic.com

Analysis and visualization done in R using the packages:  
dplyr  
tm  
dendextend
