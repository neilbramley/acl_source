#' Virility property of graphs
#'
#' How many forks and how many colliders does the graph have.
#' Returns number of forks - number of colliders
#' @param graph the graph to check
#' @keywords graph
#' @export
#' @examples
#' virility(matrix(c(0,1,1,0,0,0,0,0,0),3,3, byrow=T))

virility <-
function(graph)
{
  
  parents<-children<-rep(0, nrow(graph))
  
  for (i in 1:nrow(graph))
  {
      parents[i]<-sum(graph[,i])
      children[i]<-sum(graph[i,])
  }
  
  sum(children>1) - sum(parents>1)
}
