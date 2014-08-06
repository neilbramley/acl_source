#' Generates the outcome of an intervention or observation of a directed acyclic graph of binary variables
#'
#' This function computes the shannon entropy of a probability distribution.  It requires a second function called propagation.
#' @param int is the intervention expressed as a vector with 1 as on, -1 as off and zero as free.
#' graph is directed acyclic graph expressed as a directed adjacency matrix
#' br is the background activation rate, defaults to .1
#' pow is the causal power of the links, defaults to .9
#' @keywords action
#' @export
#' @examples
#' action(c(1,0,0), matrix(c(0,1,0,0,0,0,0,0,0),3,3, byrow=T))

action <-
function(int,graph,br=.1,pow=.9)
{
  
  #int is the intervention
  #graph is the current graph
  #br=base rate of random activations, 'noisyness' of environment
  #pow=probability of effect given cause
  
  #current graph as matrix
  graph
  
  #chance activations at noise level on right
  ca<-runif(length(int))<=br
  ca=ca+0 #logical to numeric
  
  #first step: fixed nodes + base activations
  a1<-int
  
  for (i in 1 : length(int))
  {
    if (int[i]==1 || (ca[i]==TRUE && int[i]!=-1))
    {
      a1[i]<- 1
    }
  }
  
  a2<-a1
  for (i in 1:length(int))
  {
    a2<-(a2 + propagation(graph,a1,i,pow))
    #print(a2)
  }
  
  a2<-(a2>0)+0
  #end of ACTION
  a2
}
