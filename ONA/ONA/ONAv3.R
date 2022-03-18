install.packages("igraph")  #installing new packages
install.packages("igraphdata")
library(igraph) #load the packages into R
library(igraphdata) 
data(package = "igraphdata") #looks for the datasets in this package

data(UKfaculty) #loads the specific dataset "UK faculty"
UK <-UKfaculty #renaming the dataset

as_data_frame(UK, what="edges") #checking out the edges
as_data_frame(UK, what="vertices") #checking out the nodes
as_data_frame(UK, what="both") #smush together

as_edgelist(UK, names=T) #lists the edges
as_adjacency_matrix(UK, attr="weight") #creates an adjacency matrix

#let's examine the data!
UK
summary(UK)
#D = graph is directed
#- = graph is not named
#W = graph is weighted
#- = graph is uniparite

V(UK) #nodes
gorder(UK) #count # of vertices/nodes

E(UK) #links
gsize(UK) #count # of edges/links

V(UK)$Group #school
E(UK)$weight #strength of the friendship

plot(UK, edge.arrow.size = .2, vertex.label = NA)

colrs <- c("1", "2", "3") #creates and names a vector
V(UK)$color <- colrs[V(UK)$Group] #assigns color to each item in the vector based on group
UKplot <- plot(UK, edge.arrow.size = .5, #arrow size
     vertex.size = 9, #size of node
     edge.width = E(UK)$weight,  #width of links based on the weight of the relationship
     vertex.label = NA) #no label - its too messy

legend(x = 1.5, y = -1.5, c("1","2","3"), pch = 10, col = "#777777", 
       pt.bg = colrs, pt.cex = 2, cex = .8, bty = "n", ncol = 1) #made a legend!

#network measures
degree(UK) #total connections
degree(UK, mode = 'in') #total incoming connections
degree(UK, mode = 'out') #total outgoing connections

diameter(UK) #the length of the longest linked path

edge_density(UK, loops = FALSE) #ratio of number of edges by number of possible edges

reciprocity(UK) #the proportion of mutual connections

closeness(UK, mode = 'all') #how many steps you need to access every other node from a given node

betweenness(UK) #number of shortest paths going through a node or edge

