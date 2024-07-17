library(data.table)

# Install packages to analyze the graph
install.packages("R.utils")
install.packages("igraph")

# load the libraries
library(R.utils)
library(igraph)

# Read the Facebook network data.
# Each row is an edge. The first two columns are the two nodes.
FC = fread("698.edges")

# make FB as character matrix to fit the needs of igraph package 
FC$V1 = as.character(FC$V1)
FC$V2 = as.character(FC$V2)
FC=as.matrix(FC)

# create the graph from an edge list
gg = graph_from_edgelist(FC, directed=FALSE)

graph.name = "Facebook friend circle #698"
par(mar=c(2,2,2,2))
set.seed(12345)
plot(gg, 
     layout=layout_with_fr, # Fruchterman-Reingold layout
     vertex.size=4,         # size of vertices; the default is too large
     vertex.label=NA,       # to avoid clutter, do not print the labels
     main=graph.name)

# number of nodes tell you the number of users
length(V(gg))

# number of edges tell you total number of relationships
length(E(gg))

# Simplify the graph by removing redundant edges
g=simplify(gg)

# number of nodes
length(V(g))

# number of edges
length(E(g))

# plot the friend circle
graph.name = "Facebook friend circle #698"
par(mar=c(2,2,2,2))
set.seed(12345)
plot(g, 
     layout=layout_with_fr, # Fruchterman-Reingold layout
     vertex.size=4,         # size of vertices; the default is too large
     vertex.label=NA,       # to avoid clutter, do not print the labels
     main=graph.name)

# How many components?
components(g)

# Only keep the nodes in group 1.
sub = delete_vertices(g, V(g)[components(g)$membership!=2])

# number of nodes
length(V(sub))

# number of edges
length(E(sub))

set.seed(12345)
plot(sub, 
     layout=layout_with_fr,
     vertex.size=4, 
     vertex.label=NA, 
     main=graph.name)

#Calculate the degree for each node
user=degree(sub,mode='all')
user=as.data.table(user, keep.rownames=T)
names(user) = c("ID", "alldegree")

#Draw the distribution of the degree measure
par(mar=c(4,4,4,4))
hist(user$alldegree, 
     main='Distribution of degree centrality',
     nclass=30)



# Calculate the radius of the graph the most efficient require how many jumps
radius(sub)

# Calculate the diameter of the graph the least efficient require how many jumps 
diameter(sub)

# Calculate the closeness centrality of each node
user$clo.centrality = centr_clo(sub)$res

hist(user$clo.centrality, 
     main='Distribution of closeness centrality',   
     nclass=30)


# Calculate the betweenness centrality of each node
user$betw.centrality = centr_betw(sub)$res

hist(user$betw.centrality, 
     main='Distribution of betweenness centrality',   
     nclass=30)

# Identify the person with the highest degree centrality
max(user$alldegree)

# Identify the person with the highest betweeness centrality
max(user$betw.centrality)

# Identify the person with the highest closeness centrality
max(user$clo.centrality)

# What's the index for this node in the node sequence?（where is the person）

id1=which(user$alldegree==max(user$alldegree))
id1

# What's the index for this node in the node sequence?（where is the person）

id2=which(user$betw.centrality==max(user$betw.centrality))
id2

# What's the index for this node in the node sequence?（where is the person）

id3=which(user$clo.centrality==max(user$clo.centrality))
id3

# Identify the location of the highest degree influencer in the graph
tmp.label = user$ID
tmp.label[which(user$alldegree < max(user$alldegree))] = ""
set.seed(12345)
par(mar=c(2,2,2,2))
plot(sub, 
     layout=layout_with_fr,
     vertex.size=user$alldegree, #people with larger circle are more important
     vertex.label=tmp.label
)

# Identify the location of the highest betweeness centrality in the graph
tmp.label = user$ID
tmp.label[which(user$betw.centrality < max(user$betw.centrality))] = ""
set.seed(12345)
par(mar=c(2,2,2,2))
plot(sub, 
     layout=layout_with_fr,
     vertex.size=user$betw.centrality, #people with larger circle are more important
     vertex.label=tmp.label
)

# Identify the location of the highest closeness centrality in the graph
tmp.label = user$ID
tmp.label[which(user$clo.centrality < max(user$clo.centrality))] = ""
set.seed(12345)
par(mar=c(2,2,2,2))
plot(sub, 
     layout=layout_with_fr,
     vertex.size=user$clo.centrality, #people with larger circle are more important
     vertex.label=tmp.label
)
# Find the immediate neighbors for Vertex with highest degree centrality 
s1=make_ego_graph(sub, order=1, nodes=V(sub)[id3], mode='all')[[1]]

# plot the neighbors for Vertex with highest closeness centrality
# with distance=1 
set.seed(12345)
plot(sub, 
     layout=layout_with_fr,
     vertex.size=user$clo.centrality, 
     vertex.label=tmp.label,       
     mark.groups=list(labels(V(s1)))
)

