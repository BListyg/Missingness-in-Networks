library(igraph)

#1/Degree = Prob of missing
#1 - (1/D) = Prob of missing

a <- erdos.renyi.game(
  n = 30,
  p.or.m = 1/5, 
  directed = F, 
  loops = F
)

#####

#####

V(a)$btw <- betweenness(a)

V(a)$cls <- closeness(a)

V(a)$deg <- degree(a)

fine <- 500 # this will adjust the resolving power.
pal <- colorRampPalette(c('MediumPurple', 'LightSalmon'))

#this gives you the colors you want for every point
graphCol_cls = pal(fine)[as.numeric(cut(closeness(a),breaks = fine))]

graphCol_btw <- pal(fine)[as.numeric(cut(betweenness(a),breaks = fine))]

graphCol_deg <- pal(fine)[as.numeric(cut(degree(a),breaks = fine))]

# now you just need to plot it with those colors
par(mfrow = c(2,2))
plot(a)
plot(a, vertex.color=graphCol_btw, layout = layout_on_grid)
plot(a, vertex.color=graphCol_cls, layout = layout_on_grid)
plot(a, vertex.color=graphCol_deg, layout = layout_on_grid)
