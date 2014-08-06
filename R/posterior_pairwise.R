#' Posterior distribution over two graphs given prior and an unfolding
#'
#' Posterior distribution over two graphs given prior and an unfolding
#' @param graph1 The first hypothesis
#' @param graph2 The second hypothesis
#' @param unfolding the data
#' @param prior the prior distribution (as a vector), defaults to c(.5,.5)
#' @param pow strength of causal links, defaults to .75
#' @param sar strengh of background causes, defaults to 0
#' @keywords likelihood ratio
#' @export
#' @examples
#' out<-posterior_pairwise(matrix(c(1,1,0,0),2,2,byrow=T), matrix(c(1,0,0,0),2,2,byrow=T),
#' generate_unfolding(matrix(c(1,1,0,0),2,2,byrow=T), c(1,0)))


posterior_pairwise<-function(graph1, graph2, unfolding, prior=c(.5,.5), pow=.75, sar=0)
{
  prior_rat=prior[1]/prior[2]
  li1<-(likelihood_unfolding(unfolding, graph1, pow=pow, sar=sar))
  li2<-(likelihood_unfolding(unfolding, graph2, pow=pow, sar=sar))
  
  tmp<-exp(li1-li2)*prior_rat
  
  out<-c(tmp/(tmp+1), 1/(tmp+1))
}