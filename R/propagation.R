#'Propagation function
#'
#' This function is a part of the process of calculating the outcome of a token intervention or observation
#' It makes little sense to include an example
#'
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

propagation <-
function(gr, active, st, power)
{
  for (j in 1:nrow(gr))
  {
    
    if (active[st]==1 && gr[st,j]==1 && runif(1)<=power && active[j]!=-1)
    {
      active[j]<-1
      active<-propagation(gr, active,j, power)
    }
    
  }
  active
}
