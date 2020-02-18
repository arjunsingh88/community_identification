# ===================================================================================================
#       Libraries to be installed if not already installed
# ===================================================================================================
# install.packages("dplyr")
# install.packages("tibble")
# install.packages("sna")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("class")
# install.packages("ggvis")
# install.packages("GGally")
# install.packages("randomForest")
# install.packages("OneR")
# install.packages("randomForestExplainer")
# install.packages("e1071")
# ===================================================================================================
#       Libraries used
# ===================================================================================================
library(igraph)
library(dplyr)
library(tibble)
library(sna)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(class)
library(ggvis)
library(GGally)

# ===================================================================================================
#             Mod R
# ===================================================================================================

mod_R<- function(G,C,B,S)
{
  bin = length(E(G)[B %--% B])
  bout = length(E(G)[B %--% S])
  return(bin/(bin+bout))
} 


# ===================================================================================================
#             Mod L
# ===================================================================================================
neighbors_in <- function(n,G,E)
{ 
  #returns the number of neighbors of node n in set E in graph g
  return(length(intersect(neighbors(G,n),E)))
}


mod_L <-function(G,C,B,S)
{
  D <- union(C,B)
  lin <- sum(sapply(D,neighbors_in,G,D))/length(D)
  lout <- sum(sapply(B,neighbors_in,G,S))/length(B)
  return(lin/lout)
}


# ===================================================================================================
#             Mod M
# ===================================================================================================
mod_M<- function(G,C,B,S)
{ 
  D<- union(C,B)
  din = length(E(G)[D %--% D])
  dout = length(E(G)[B %--% S])
  return(din/dout)
}


# ===================================================================================================
#             Mod S - conductance
# ===================================================================================================

conductance<- function(G,C,B,S)
{
  D<- union(C,B)
  cs<- length(E(G)[B%--%S])
  ms<- length(E(G)[D%--%D])
  return(cs/(cs+2*ms))
}

# ===================================================================================================
#             Local Community detection algorithm
# ===================================================================================================

# Update function to update the sets i.e shell, boundary, core of community as we add the new nodes
update<- function(n,g,C,B,S)
{
  # move n in S to D
  S<-S[S!=n]
  D <- union (C,B)
  if(all(neighbors(g,n) %in% D))
  { 
    # add n to C
    C <- union(C,n)
  }
  else
  { 
    # add n to B
    B <- union(B,n)
    new_s = setdiff(neighbors(g,n),union(D,S))
    if(length(new_s)>0)
    { 
      S <- union(S,new_s)
    }
    for(b in B) 
    { 
      if(all(neighbors(g,b) %in% D)) 
      { 
        B <- B[B!=b]
        C<-union(C,b)
      }
    }
  }
  return(list(C=C,B=B,S=S))
}


# ALGORITHM
# We continue with local community detection for each node based on different modularity measure

compute_quality<-function(n,g,C,B,S,mod)
{
  # computes the quality of a community if node n joins
  # n is a node in S
  res<-update(n,g,C,B,S)
  C<-res$C
  B<-res$B
  S<-res$S
  return(mod(g,C,B,S))
}


local_com<- function(target,g,mod)
{
  if(is.igraph(g) && target %in% V(g))
  {
    C<-c()
    B<-c(target)
    S<-c(V(g)[neighbors(g,target)]$id)
    Q<-0
    new_Q<-0
    while((length(S)>0) && (new_Q>=Q))
    {
      QS<- sapply(S,compute_quality,g,C,B,S,mod)
      new_Q<-max(QS)
      if(new_Q>=Q)
      {
        s_node <- S[which.max(QS)]
        res <- update(s_node,g,C,B,S)
        C<-res$C
        B<-res$B
        S<-res$S
        Q<-new_Q
      }
      
    }
    #print(Q)
    return(union(C,B))
  }
  else
  {
    stop("invalid arguments")
  }
  #print(Q)
}

# =========================================================
local_commquality<- function(target,g,mod)
{
  if(is.igraph(g) && target %in% V(g))
  {
    C<-c()
    B<-c(target)
    S<-c(V(g)[neighbors(g,target)]$id)
    Q<-0
    new_Q<-0
    while((length(S)>0) && (new_Q>=Q))
    {
      QS<- sapply(S,compute_quality,g,C,B,S,mod)
      new_Q<-max(QS)
      if(new_Q>=Q)
      {
        s_node <- S[which.max(QS)]
        res <- update(s_node,g,C,B,S)
        C<-res$C
        B<-res$B
        S<-res$S
        Q<-new_Q
      }
      
    }
    
    return(c(Q,c(union(C,B))))
  }
  else
  {
    stop("invalid arguments")
  }
  
}
#local_commquality(2,g,mod_R)[1]

# =========================================================



# g <- read.graph("karate.gml", format= "gml")
# plot(g)
# V(g)
# E(g)
#local_com(3,g,mod_R)

# ===================================================================================================
# compute partitions
# ===================================================================================================
compute_ego_partition<- function(target,g,mod)
{  
  res <- vector(mode="list", length=2)
  names(res) <- c("com", "notcom")
  res$com <- local_com(target,g,mod)
  res$notcom <- V(g)[!(id %in% res$com)]$id
  res
  return(res)
}




# ===================================================================================================
# Quality of community
# ===================================================================================================

groundtruth_localcom_quality <- function(g,bipartition,method="nmi")
{
  #print(V(g)[id%in%bipartition$com]$egocom)
  V(g)[id%in%bipartition$com]$egocom <-0
  #print(V(g)[id%in%bipartition$notcom]$egocom)
  V(g)[id%in%bipartition$notcom]$egocom <-1
  return(compare(V(g)$value,V(g)$egocom,method))
}



localcom_quality <- function(target,g,mod,method)
{
  bipartition <- compute_ego_partition(target,g,mod)
  #print(bipartition)
  return(groundtruth_localcom_quality(g,bipartition,method))
  
}

