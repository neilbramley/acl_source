#' Create a value matrix for utility calculations
#'
#' This function creates a value matrix for calculating the utility of endorsing a particular graph given your prior
#' Requires hypothesis graphs g to be in data.frame form in the environment
#' Requires valid proposal space vps to be in data.frame form in the environment too
#' @param r Value link correctly endorsed, defaults to 20
#' @param w Value link incorrectly endorsed, defaults to -10
#' @param m Value link incorrectly not endorsed, defaults to -10
#' @param f Value link correctly not endorsed, defaults to 0
#' @param b Bonus completely correct, defaults to 0
#' @keywords utility
#' @export
#' @examples
#' vps<-g
#' initialise_payoffs()


initialise_payoffs <-
function(r=20, w=-10, m=-10, f=0, b=0)
{ 
  
  #r=value link correctly endorsed
  #w=value link incorrectly endorsed
  #m=value link incorrectly not endorsed
  #f=value link correctly not endorsed
  #b=bonus completely correct
  
  
  #Matrix of choice values C(i,j) is value of choosing proposal graph i when graph j is correct
  C<-matrix(0,dim(vps)[1],dim(g)[1])
  
  #Bonus for being completely right
  diag(C)<-b
  
  for (i in 1:dim(C)[1])
  {
    for (j in 1:dim(C)[2])
    {
      
      
      #Add b if proposal is completely correct
      #       if (sum(vps[i,]==g[j,])==dim(g)[2])
      #       {
      #         C[i,j]<-C[i,j]+b
      #       }
      
      
      #Add r for every correct link
      C[i,j]<-C[i,j] + r*as.matrix(sum((vps[i,]==1) * (g[j,]==1)))
      
      #Subtract w for every link present in i but not in j
      C[i,j]<-C[i,j] + w*as.matrix(sum((vps[i,]==1) * (g[j,]!=1)))
      
      #Subtract m for every link missing in i but present in j
      C[i,j]<-C[i,j] + m*as.matrix(sum((vps[i,]!=1) * (g[j,]==1)))
      
      #Add f for every link not in i or in j
      C[i,j]<-C[i,j] + f*(sum(matrix((vps[i,]==0) + (g[j,]==0),sqrt(dim(g)[2]))+t(matrix((vps[i,]==0) + (g[j,]==0),sqrt(dim(g)[2])))==4)-sqrt(dim(g)[2]))/2
    }
  }
  C
}
