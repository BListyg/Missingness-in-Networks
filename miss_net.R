library(igraph)
library(mice)
library(simstudy)
library(scales)

nPeople <- 30
pm <- 0.3

g <- erdos.renyi.game(n = nPeople, 
                 p.or.m = 1/20, 
                 directed = F, 
                 loops = F)
                  
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

V(g1)$color <- ifelse( as.logical(rbinom(n = V(g), size = 1, prob = pm)) == T, 'red', 'white') 

V(g2)$color <- 'white'

V(g3)$color <- V(g1)$color

#Edge Coloring
E(g)$color <- 'black'

E(g1)$color <- 'black'

E(g2)$color <- ifelse( as.logical(rbinom(n = E(g2), size = 1, prob = pm)) == T, 'red', 'black') 

E(g3)$color <- E(g2)$color

#Plot
par(mfrow= c(1,4))

plot(g,
     layout=layout_in_circle,
     main = 'Original Graph')

plot(g1,
     layout=layout_in_circle,
     main = 'MCAR Nodes')

plot(g2,
     layout=layout_in_circle,
     main = 'MCAR Edges')

plot(g3,
     layout=layout_in_circle,
     main = 'MCAR Edges & Nodes')
