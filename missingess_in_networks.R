library(igraph)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(cowplot)
library(scales)

nPeople <- 100
pEdge <- 1/40

#Generate a random graph with n vertices set at nPeople and the probabiltiy of an edge forming between them set to pEdge 
g <- erdos.renyi.game(n = nPeople,
                      p.or.m = pEdge,
                      directed = T,
                      loops = F)

na_sample <- function(vertex_id,p_nomiss,p_miss,...){
  sample(x = c(vertex_id, NA), size = 1, prob = c(p_nomiss,p_miss))
}

cbind_list <- function(list){
  max_length <- c()
  
  for(i in 1:length(list)){
    max_length[i] <- length(list[[i]])
  }
  
  for(i in 1:length(list)){
    length(list[[i]]) <- max(max_length)
  }
  
  completed_list <- do.call(cbind,list)
  
  return(completed_list)
}

#Giving the fake people in my network fakes letter IDs.
#Note the max value for this is 676 (26 letters * 26 letters)
names <- expand.grid(letters, LETTERS) %>% 
  apply(X = ., 1, 
        paste, sep='',
        collapse='')

g_sim <- function(g){

#Randomly sampling nPeople names and setting the names of the vertices to the sample
V(g)$names <- sample(x = names, 
                     size = nPeople, 
                     replace = F)

g_dat <- data.frame(
  
  vertex_id = V(g)$names,
  
  connections = degree(g),
  
  p_nomiss = 1/degree(g),
  
  stringsAsFactors = F
  
)

g_dat$p_nomiss[which(g_dat$p_nomiss > 0 & is.infinite(g_dat$p_nomiss))] <- 1
g_dat$p_nomiss[which(g_dat$p_nomiss < 0 & is.infinite(g_dat$p_nomiss))] <- 0

g_dat$p_miss <- 1 - g_dat$p_nomiss

na_vertices <- mapply(FUN = na_sample, g_dat$vertex_id, g_dat$p_nomiss, g_dat$p_miss)

g_miss <- g

g_miss <- delete.vertices(graph = g_miss, v = V(g_miss)[which(is.na(na_vertices))])

return(list(
  g_miss = degree.distribution(g_miss),
  g = degree.distribution(g)))
}

full_sim <- function(){
  
  g <- erdos.renyi.game(n = nPeople, 
                        p.or.m = pEdge, 
                        directed = T, 
                        loops = F)
  
  g_sim(g)
  
}

simulation_data <- replicate(n = 1000, full_sim())

par(mfrow=c(1,2))

cbind_list(simulation_data[1,]) %>% 
  matplot(type='l',lwd=0.5,col='black',main = 
            "Degree distribtion of 1000 random graphs, N = 100\nProbability of edge formation = .2\nP(V_i missing) = 1 - (1/degree(V_i))")

cbind_list(simulation_data[2,]) %>% 
  matplot(type='l',lwd=0.5,col='red',main = 
            "Degree distribtion of 1000 random graphs, N = 100\nProbability of edge formation = .2\nNo Missingness Mechanism")

#####

par(mfrow=c(1,4))

V(g)$color <- 'lightblue'

plot(g, layout = layout_on_sphere, edge.width = 1, vertex.size = 10, main = 'Original graph',vertex.label = V(g)$names)

###

g1 <- g

V(g1)[which(is.na(na_vertices))]$color <- "indianred"

V(g1)[which(!is.na(na_vertices))]$color <- "lightblue"

plot(g1, layout = layout_on_sphere, edge.width = 1, vertex.size = 10, main = 'Degree Missingness\nNodes Colored',vertex.label = V(g1)$names)

###

g2 <- g

V(g2)[which(is.na(na_vertices))]$color <- "indianred"
V(g2)[which(is.na(na_vertices))]$names <- NA

V(g2)[which(!is.na(na_vertices))]$color <- "lightblue"

plot(g2, layout = layout_on_sphere, edge.width = 1, vertex.size = 10,main= 'Degree Missingness\nLabel Erased',vertex.label = V(g2)$names)

###

g3 <- g

g3 <- delete.vertices(graph = g3, v = V(g3)[which(is.na(na_vertices))])

V(g3)$color <- "lightblue"

plot(g3, layout = layout_on_sphere, edge.width = 1, vertex.size = 10,main= 'Degree Missingness\nNodes Erased',vertex.label = V(g3)$names)

