#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

plotprog <-
function()
{
  par(mfrow=c(2,2), ask=T)
  
  #Expectancies
  tmp<-matrix(c((out[[1]]-min(out[[1]]))/sum(out[[1]]-min(out[[1]])),
                (out[[2]]-min(out[[2]]))/sum(out[[2]]-min(out[[2]])),
                (out[[3]]-min(out[[3]]))/sum(out[[3]]-min(out[[3]]))),dim(ints)[1])
  
  barplot(t(tmp),main='Expected values for the possible interventions'
          ,ylab='Normalised Expectancy', xlab='Intervention', col=c('red','blue','chartreuse3'), beside=T,
          legend.text=c("entropy", "probability", "utility"), names.arg=1:dim(ints)[1], cex.names=.7)
  
  #Max p
  plot(1:t, apply(as.matrix(PMFs[, 1:t]), 2, max), type='l', col='blue', lwd=2, xlab='Time point', ylab='Probability',
       main='Max Probability', xlim=c(1,t+1))
  text(t,max(PMFs[,t]), paste('Graph', as.character(which.max(PMFs[,t]))))
  
  #Max e
  plot(1:t,-apply(as.matrix(PMFs[, 1:t])*log2(as.matrix(PMFs[, 1:t])),2,sum), type='l', col='red', lwd=2, xlab='Time point', ylab='Entropy', main='Entropy')
  
  #PMF
  barplot(t(pG), xlab='Graphs', ylab='Probability', main='Probability distribution over graphs', names.arg=1:dim(g)[1], cex.names=.5)
  
  
  par(mfrow=c(1,1), ask=F)
  
}
