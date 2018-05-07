library(igraph)

# 1
NAm2 = read.table("NAm2.txt", header=TRUE)

subset = subset(NAm2, Pop == "Chipewyan" | Pop == "Pima" | Pop == "Huilliche")

# 2

mat = data.matrix(subset[, -c(1:8)])
adj = mat%*%t(mat) # scalar product
diag(adj) = 0

# 3
q = quantile(adj, prob = 0.7)
adj[adj <=  q] = 0
select <- rowSums(adj > 0) >= 2
adj = adj[select, select]

# 4

# 5
g = graph.adjacency(adj, mode="undirected", weighted = TRUE)
l = layout.fruchterman.reingold(g)

# 6 
plot(g, vertex.label=NA, layout = l, vertex.size = 5)

# 7
gorder(g)
gsize(g)
diameter(g, weights = NA)
mean_distance(g)

# 8

sim = similarity(g)

# 9
res_hclust = hclust(as.dist(1-sim))

# 10
mod = c()
for (i in 1:10){
  lab = cutree(res_hclust, i)
  mod[i] = modularity(g, lab)
}
plot(mod,type="l")

comMax = which(mod == max(mod))[1]

# 11
labels = cutree(res_hclust, comMax)
V(g)$color = labels
plot(g, vertex.label=NA, layout = l, vertex.size = 5)

# 12

com = igraph::cluster_edge_betweenness(g)
dendPlot(com)

# 13
mods =  sapply(0:ecount(g), function(i){
  g2 = delete.edges(g, com$removed.edges[seq(length=i)])
  cl = clusters(g2)$membership
  modularity(g,cl)
})
g2<-delete.edges(g, com$removed.edges[seq(length=which.max(mods)-1)])
V(g)$color=clusters(g2)$membership
plot(g, vertex.label=NA, layout = l, vertex.size = 5)