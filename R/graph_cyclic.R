#' Is this graph cyclic
#'
#' Does the graph contain a loop.  True if it does.
#' @param graph An adjacency matrix
#' @keywords DAG
#' @export
#' @examples
#' graph_cyclic(matrix(c(0,1,1,0,0,0,0,0,0),3,3, byrow=T))
#' graph_cyclic(matrix(c(0,1,0,1,0,0,0,0,0),3,3, byrow=T))



graph_cyclic <-
function(graph)
{
  st<-F
  
  while (st==F)
  {
    #Get nodes with no children
    tmp<-which(  rowSums(graph)==0 & colSums(graph)>0)
    
    if (length(tmp)==0)
    {
      st<-T
      
      if (sum(graph)>0)
      {
        out<-T
      } else
      {
        out<-F
      }
      
    } else{
      graph[,tmp[1]]<-0
    }
  }
  
  out
}
