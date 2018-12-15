library(igraph)
library(mice)
library(simstudy)
library(scales)

nPeople <- 30
pm <- (1/3)

g <- erdos.renyi.game(n = nPeople, 
                      p.or.m = 1/20, 
                      directed = F, 
                      loops = F)

V(g)$names <- apply(X = expand.grid(LETTERS, letters, stringsAsFactors = F), 1, paste, sep='', collapse='')[1:length(V(g))]

g1 <- g
g2 <- g
g3 <- g

#Adding Covariates
g_dat <- V(g) %>% 
  matrix %>% 
  data.frame

names(g_dat)[1] <- 'node'

g_dat <- cbind(
  node = g_dat,
  m = rnorm(n = length(V(g)), 
            mean = 0, 
            sd = 1) %>% 
    scales::rescale(c(18,80)) %>% 
    round,
  cent = centralization.betweenness(g)$res,
  u = rbeta(n = length(V(g)),
            shape1 = 1,
            shape2 = 2)
)

###MCAR###

#Vertex Coloring
V(g)$color <- 'white'

V(g1)$color <- ifelse( (pnorm(g_dat$m, mean = mean(g_dat$m), sd = sd(g_dat$m)) < pm) == T, 'red', 'white')

g1_del <- delete.vertices(graph = g1, v = V(g1)[which(V(g1)$color == 'red')])

V(g2)$color <- 'white'

V(g3)$color <- V(g1)$color

#Edge Coloring
E(g)$color <- 'black'

E(g1)$color <- 'black'

E(g2)$color <- ifelse( (pnorm(g_dat$m, mean = mean(g_dat$m), sd = sd(g_dat$m)) > pm) == T, 'red', 'white')

g2_del <- delete.edges(graph = g2, edges = E(g2)[which(E(g2)$color == 'red')])

E(g3)$color <- E(g2)$color

g3_del <- g3

g3_del <- delete.vertices(graph = g3_del, v = V(g3_del)[which(V(g3_del)$color == 'red')])

g3_del <- delete.edges(graph = g3_del, edges = E(g3_del)[which(E(g3_del)$color == 'red')])

#Plot
par(mfrow= c(2,4))

plot(g,
     layout=layout_in_circle,
     main = 'Original Graph', 
     vertex.label=V(g)$names)

plot(g1,
     layout=layout_in_circle,
     main = 'MCAR Nodes Colored', 
     vertex.label=V(g1)$names)

plot(g1_del,
     layout=layout_in_circle,
     main = 'MCAR Nodes Removed', 
     vertex.label=V(g1_del)$names)

plot(g2,
     layout=layout_in_circle,
     main = 'MCAR Edges Colored',
     vertex.label=V(g2)$names)

plot(g2_del,
     layout=layout_in_circle,
     main = 'MCAR Edges Removed',
     vertex.label=V(g2_del)$names)

plot(g3,
     layout=layout_in_circle,
     main = 'MCAR Edges & Nodes Colored',
     vertex.label=V(g3)$names)

plot(g3_del,
     layout=layout_in_circle,
     main = 'MCAR Edges and Nodes Removed',
     vertex.label=V(g3_del)$names)

#####

library(reshape2)
library(ggplot2)
library(cowplot)


#

A <- matrix(as_adjacency_matrix(g), ncol=length(V(g)))

longData<-melt(A)
longData<-longData[longData$value!=0,]

a <- ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="", y="", title="") + 
  ylim(c(0,length(V(g)))) + 
  xlim(c(0,length(V(g)))) + 
  theme_bw() + 
  theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
        axis.text.y=element_text(size=9),
        plot.title=element_text(size=11))+ theme(legend.position="none")

#

A <- matrix(as_adjacency_matrix(g1_del), ncol=length(V(g1_del)))

longData<-melt(A)
longData<-longData[longData$value!=0,]

b <- ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="", y="", title="") + ylim(c(0,length(V(g)))) + xlim(c(0,length(V(g)))) + theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                                                                                                      axis.text.y=element_text(size=9),
                                                                                                      plot.title=element_text(size=11))+ theme(legend.position="none")

#

A <- matrix(as_adjacency_matrix(g2_del), ncol=length(V(g2_del)))

longData<-melt(A)
longData<-longData[longData$value!=0,]

c <- ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="", y="", title="") + ylim(c(0,length(V(g)))) + xlim(c(0,length(V(g)))) + theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                                                                                                      axis.text.y=element_text(size=9),
                                                                                                      plot.title=element_text(size=11))+ theme(legend.position="none")

#

A <- matrix(as_adjacency_matrix(g3_del), ncol=length(V(g3_del)))

longData<-melt(A)
longData<-longData[longData$value!=0,]

d <- ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="", y="", title="") + ylim(c(0,length(V(g)))) + xlim(c(0,length(V(g)))) + theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                                                                                                      axis.text.y=element_text(size=9),
                                                                                                      plot.title=element_text(size=11)) + theme(legend.position="none")

plot_grid(a,b)#,c,d, nrow = 2, ncol = 2)
