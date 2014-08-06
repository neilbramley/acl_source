#' Create likelihood data.frame
#'
#'This creates a data-frame of likelihoods of outcomes under interventions on a DAG with a parameterisation
#'given noisy or function of active endogenous causes and constant exogenous causes
#' @param g matrix of hypothesis graphs where each line is the graph adjacency marix written out by row
#' @param br power of background causes, defaults to .1
#' @param power of active endogenous causes, defaults to .8
#' @keywords likelihood
#' @export
#' @examples
#' li<-likelihood(g)

likelihood <-
function(g,br=.1,pow=.9)
{
  
  
  #ints replicated g times
  ints.expanded<-matrix(t(matrix(ints,dim(ints)[1],dim(ints)[2])),dim(ints)[1]*dim(g)[1],dim(ints)[2],byrow=T)
  
  #Factor label for Interventions
  Interventions<-(seq(1:dim(ints)[1])); Interventions<-factor(rep(Interventions,times=dim(g)[1]))
  
  #g replicated elementwise ints times
  g.expanded<-g[rep(1:nrow(g),each=(dim(ints)[1])),]
  
  #Factor label for graphs
  Graphs<-(seq(1,dim(g)[1])); Graphs<-factor(rep(Graphs,each=dim(ints)[1]))
  
  #g=list of all possible graphs
  #br=base rate
  #pow=probability of effect given cause
  
  #create the likelihood matrix acording to size of intervention, graph and outcome space
  
  li<-matrix(1,dim(ints)[1]*dim(g)[1],dim(o)[1])
  
  #likelihoods initialised to ones so multiply by probability of each node to get likelihood
  for (i in 1:sqrt(dim(g)[2]))
  {
    j=1:dim(g)[2]-1
    j=j[seq(1,length(j),sqrt(dim(g)[2]))]+i
    #this creates an indexing for getting active causes
    #i.e. the columns of the transpose of the line treated as a sqaure matrix; i.e. j=[1+i 4+i 7+i] for 3 nodes
    
    #o is a transformed outcome matrix (for current node only) made to same dimensions of li
    o.expanded<-t(matrix(rep(o[,i],dim(li)[1]),dim(o)[1]))==1
    
    ints.expanded.i<-kronecker(matrix(1,1,dim(o)[1]),ints.expanded[,i])
    
    nA<-(g[,j])%*%t(o)#number of active causes of curent node
    nA2<-g.expanded[,j]%*%t(o)
    nA.expanded<-nA[rep(1:nrow(nA),each=(dim(ints)[1])),]
    
    
    #p=zero if node is fixed off but is on
    li<-li * (1-(ints.expanded.i==-1) * (o.expanded==1))
    #p=zero if node is fixed on but is off
    
    li<-li * (1 -  (ints.expanded.i==1) * (o.expanded==0))
    
    #add comments!            
    li<-li * (1 -  ((ints.expanded.i==0) * (o.expanded==0))*(1-(1-br) *((1-pow)^nA.expanded))  )
    
    
    li<-li * (1 - ((ints.expanded.i==0) * (o.expanded==1))* ((1-br) *((1-pow)^nA.expanded))    )
    
    
  }
  li <-data.frame(Graphs,Interventions,li)
}
