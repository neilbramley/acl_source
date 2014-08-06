#' Unfold a cyclic causal network
#'
#' This function unfolds a cyclic causal graph probabilitically for a set number of time steps
#' @param graph The graph to be unfolded
#' @param starting_state The state at t=0
#' @param unfold_for How many steps to unfold the graph for? Defaults to 20
#' @param pow How likely are the causal connections to cause their effects, defaults to .75
#' @param sar How likely are the components to turn on on their own, defaults to 0.
#' @keywords cyclic networks
#' @export
#' @examples
#' generate_unfolding(matrix(c(1,1,0,0), 2, 2, byrow=T), c(1,0))

generate_unfolding<-function(graph, starting_state, unfold_for=20, pow=.75, sar=0)
{
  output=matrix(0, nrow=nrow(graph), ncol=unfold_for)
  
  output[,1]<-starting_state
  for (i in 2:ncol(output))
  {
    
    for (j in 1:nrow(output))
    {
      if(output[j,i-1]==1)
      {
        output[graph[j,]==1,i]<-runif(sum(graph[j,]==1))<pow
      }
      
      output[runif(nrow(output))<sar,i]<-1
    } 
  }
  output
}