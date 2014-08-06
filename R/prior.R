#' Generate a prior
#'
#' The function generates a prior in the form required for other functions, namely a column matrix
#' @param s what kind of prior do you want?  Can choose 'flat', 'random', 'flatSomeZero' and 'flatMostZero'
#' @param g matrix of hypothesis graphs, one written rowwise per row
#' @keywords prior
#' @export
#' @examples
#' prior('random', g)

prior <-
function(s, g)
{
  if(s=='flat')
  {  
    pG<-matrix(1,dim(g)[1],1)/dim(g)[1]
  } else if (s=='random')
  {
    tmp<-runif(dim(g)[1])
    pG<-matrix(tmp, length(tmp),1)/sum(tmp)
  } else if (s=='flatSomeZero')
  {
    tmp<-round((dim(g)[1]-3)*runif(1)+2)
    tmp2<-c(rep(1,tmp), rep(0, dim(g)[1]-tmp))
    tmp3<-sample(tmp2, dim(g)[1])
    pG<-matrix(tmp3/tmp, dim(g)[1],1)
  } else if (s=='flatMostZero')
  {
    tmp<-round(3*runif(1)+2)
    tmp2<-c(rep(1,tmp), rep(0, dim(g)[1]-tmp))
    tmp3<-sample(tmp2, dim(g)[1])
    pG<-matrix(tmp3/tmp, dim(g)[1],1)
  }
  pG
}
