#' Uses igraph to draw graphs how I like them
#' 
#' Works well for 3-5 nodes
#'
#' @param graph The adjacency matrix of the graph you want drawn
#' @param labels The names for the nodes in the graph
#' @keywords graph
#' @export
#' @examples
#' draw_graph(matrix(c(0,1,1,0,0,0,0,0,0),3,3, byrow=T), c('A','B','C'))

draw_graph<-function(graph, labels)
{
  require(igraph)
  
  #Draw a graph with the edges in the adjacency matrix given (provided it has at least one edge)
  edges<-which(graph !=0, arr.ind = T)
  tmp<-matrix(labels[edges], nrow(edges),2)
  tmp2<-cbind(tmp[,1], '--+',tmp[,2])
  
  tmp3<-labels[c(1:nrow(graph))[!(1:nrow(graph)%in%edges)]]
  
  if (length(tmp3>0))
  {
    tmp4<-paste(', ', paste(tmp3, collapse=', '), collapse="")
  } else {
    
    tmp4<-tmp3
  }
  
  tmp5<-paste(rbind(paste(apply(tmp2, 1, paste, collapse=""),collapse=", "), tmp4), collapse="")
  
  expression<-paste(c("my.graph.formula(", tmp5, ")"), collapse="")
  g1<-eval(parse(text=expression))
  
  
  
  
  V(g1)$name
  V(g1)$label<-V(g1)$name
  V(g1)$size <- 60
  V(g1)$color<-'white'
  V(g1)$label.cex <- 2#1.5
  #V(g1)$label.dist <- 2
  V(g1)$label.color <- 'black'
  V(g1)$width<-3
  
  
  E(g1)$color <- 'black' #colour_vec
  E(g1)$width <- 3#thickness_vec
  #E(g1)$color[2] <- E(g1)$color[4] <- "red"
  #E(g1)$lty <- c(1,2,1,2,1,1)
  #E(g1)$label <- c(NA,"X",NA,"X",NA,NA)
  #E(g1)$label.color <- "red" 
  #E(g1)$label.cex <- 2
  #E(g1)$curved=0.2
  
  if (length(labels)==3)
  {
    #unpermeate locations
    locations<-matrix(0,nrow(graph), 2)
    locations[V(g1)$name==labels[1],]<- c(100,186.6)/100
    locations[V(g1)$name==labels[2],]<- c(186.6,50)/100
    locations[V(g1)$name==labels[3],]<- c(13.4,50)/100
    
    plot(g1, layout=locations, edge.arrow.size=2)
  }
  else if(length(labels)==4)
  {
    #unpermeate locations
    locations<-matrix(0,nrow(graph), 2)
    locations[V(g1)$name==labels[1],]<- c(0,200)
    locations[V(g1)$name==labels[2],]<- c(200,200)
    locations[V(g1)$name==labels[3],]<- c(200,0)
    locations[V(g1)$name==labels[4],]<- c(0,0)
    
    plot(g1, layout=locations, edge.arrow.size=2)
  } else if (length(labels)==5)
  {
    locations<-matrix(0,nrow(graph), 2)
    
    locations[V(g1)$name==labels[1],]<- c(-100,100)
    locations[V(g1)$name==labels[2],]<- c(90,-38)
    locations[V(g1)$name==labels[3],]<- c(18,-262)
    locations[V(g1)$name==labels[4],]<- c(-218,-262)
    locations[V(g1)$name==labels[5],]<- c(-290,-38)   
    
    #fill in coordinates later
    plot(g1, layout=locations, edge.arrow.size=2)
  }
  else
  {
    plot(g1, edge.arrow.size=2)
  }
}

