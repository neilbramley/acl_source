#' Generate a prior
#'
#' The function generates a prior in the form required for other functions, namely a column matrix
#' @param s what kind of prior do you want?  Can choose 'flat', 'random', 'flatSomeZero' and 'flatMostZero'
#' @param N number of hypotheses
#' @keywords prior
#' @export
#' @examples
#' prior('random', M)

prior <-
function(s, M)
{
  if(s=='flat')
  {  
    pG<-matrix(1,M,1)/(M)
  } else if (s=='random')
  {
    tmp<-runif(M)
    pG<-matrix(tmp, length(tmp),1)/sum(tmp)
  } else if (s=='flatSomeZero')
  {
    tmp<-round((M-3)*runif(1)+2)
    tmp2<-c(rep(1,tmp), rep(0, M-tmp))
    tmp3<-sample(tmp2, M)
    pG<-matrix(tmp3/tmp, M,1)
  } else if (s=='flatMostZero')
  {
    tmp<-round(3*runif(1)+2)
    tmp2<-c(rep(1,tmp), rep(0, M-tmp))
    tmp3<-sample(tmp2, M)
    pG<-matrix(tmp3/tmp, M,1)
  }
  pG
}
