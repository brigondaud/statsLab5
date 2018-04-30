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
adj <- adj - q # Translation to normalize
adj[adj <=  0] = 0
#adj[adj > 0] = 1
select <- rowSums(adj == 0) < nrow(adj) - 2
#adj[rowSums(adj == 1) < length(adj) - 2]
#select = rowSums(adj) >= 2
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
res_hclust = hclust(as.dist(sim))

# 10
mod = c()
for (i in 1:10){
  lab = cutree(res_hclust, i)
  mod[i] = modularity(g, lab)
}
plot(mod,type="l")

# 11


