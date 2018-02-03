#' Create likelihoods for a particular observation/intervention and graph on the fly
#'
#' An alternative to precomputing likelihood of all possible data patterns for when the problem is too large
#' @param graph the true graph
#' @param int the intervention performed
#' @param dat the outcome observed
#' @param sa spontaneous activation rate
#' @param po power of causal links
#' @param combination_function the way in which multiple active causes interact, defaults to noisy_or, currently no other option
#' @keywords likelihoods
#' @export
#' @examples
#' indi_like(DAGs_3[,,15], c(1,0,0), c(1,1,1), .1, .8)


indi_like <-
  function(graph, int, dat, sa, po, combination_function='noisy_or')
  {
    
    p<-rep(NA, nrow(graph))
    
    for (i in 1:nrow(graph))
    {
      #How many active parents
      n_act_pa<-sum(graph[,i]==1 & dat==1)
      
      if (int[i]==0)
      {
        #Probability of state
        p[i]<-(1-(1-sa)*(1-po)^n_act_pa)*dat[i] + ((1-sa)*(1-po)^n_act_pa)*(1-dat[i])
      }
      else if (int[i]==1)
      {
        #OR overriden by any interventions
        p[i]<-1*(dat[i]==1)
      }
      else if (int[i]==-1)
      {
        p[i]<-1*(dat[i]==0)
      }
    }
    
    prod(p)  
  }