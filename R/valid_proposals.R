#' Valid proposal space for a causal judgement
#'
#' This function creates the space of all reportable graphs i.e. all DAGs and all 3+ cycles for my experiments
#' Requires rowwise matrix g listing causal hypotheses g to be in the environment
#' @keywords causal judgment decision
#' @export
#' @examples
#' valid_proposals()

valid_proposals <-
function()
{
  #Create all possible proposals by the learning (including cycles length 2+)
  
  
  #Create graphs with all permutations of edge:present/absent
  tmp<-as.matrix(expand.grid(rep(list(0:1), dim(g)[2]))) #WARNING, gets v large for large g
  p<-g
  
  #Select graphs not already in g unless they have both an edge A>B and an edge B<A.
  #The discounting of those already in g, and appending to g is done so as to have the same ordering as in g
  for (i in 1:dim(tmp)[1])
  {  
    if ((sum(apply(matrix(tmp[i,],sqrt(dim(g)[2]))*t(matrix(tmp[i,],sqrt(dim(g)[2]))),1,sum)) == 0)  && (sum(apply(kronecker(matrix(1,dim(g)[1],1),t(as.matrix(tmp[i,])))==g,1,sum)==dim(g)[2])==0))
      
    {
      
      p<-rbind(p,tmp[i,])
    }
    
  }
  
  
  
  p
}
