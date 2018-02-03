#' Get intervention values
#'
#' Computes the most useful intervention according to any desired function.
#' Does not require likelihoods to be preloaded as it uses indi_li to compute these on the fly
#' @param DAGs an array where every slice is the next graph matrix in the hypothesis space
#' @param int_opts the available interventions in a maatrix where each row is an intervention 1 is on, -1 is off, 0 is free
#' @param prior the prior distribution over graphs
#' @param sar spontaneous activation rate
#' @param pow power of causal links
#' @param FUN the function to use to evaluate the intervention options relative to the hypothesis space and prior
#' @keywords expected value
#' @export
#' @examples
#' int_opts<-as.matrix(expand.grid(rep(list(0:2), 3)))
#' int_opts[int_opts==2]<--1
#' out<-choose_int.nl(DAGs_3, int_opts, prior('random',25), .1, .8, shannon_entropy)


choose_int.nl <- 
  function(DAGs, int_opts, prior, sar, pow, FUN, ...)
  {
    
    #possible outcomes
    o<-as.matrix(expand.grid(rep(list(0:1 ), dim(DAGs)[1])))
    
    V <- mar <- matrix(0,nrow(int_opts), dim(o)[1])
    
    #for each intervention
    for (i in 1:nrow(int_opts))
    {
      
      likelihoods <- posteriors <- matrix(0,dim(o)[1], dim(DAGs)[3])
      
      #For each outcome
      for (j in 1:nrow(o))
      {
        
        #How likely is this outcome given each graph
        for (k in 1:dim(DAGs)[3])
        {
          likelihoods[j,k]<-indi_like(DAGs[,,k], int_opts[i,], o[j,], sar, pow)
        }
        
        #What would the posterior be?
        post.un<-prior*likelihoods[j,]
        posteriors[j,]<-post.un/sum(post.un)
        
        V[i,j]<- -apply(as.matrix(posteriors[j,] * log(posteriors[j,])), 2, sum, na.rm=T)
      }
      
      #And the value of this posterior
      V[i,]<-FUN(as.matrix(prior), ...) - FUN(t(posteriors))
      #V[i,]<-shannon_entropy(as.matrix(prior)) - shannon_entropy(t(posteriors))
      
      #What is the marginal likelihood of each of outcome?
      #p(outcome| graph) * prior p (graph)
      mar[i,]<-rowSums(sweep(likelihoods, 2, prior, FUN="*"))
      
    }
    
    #So what is the expected value of each intervention
    out<-rowSums(mar*V)
    
    out
  }