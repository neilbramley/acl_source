#' Creates error bars
#'
#' This function adds error bars to a bar plot.
#' @param x locations of bars (normally 1:n_bars).
#' @param y height of bars (pull this from the original plot by assigning the plot output to a variable).
#' @param upper extent of upper errorbar (e.g. compute standard errors and put these here).
#' @param lower extent of lower errorbar defaults to same as upper.
#' @param length width of bars
#' 
#' @keywords error bars
#' @export
#' @examples
#' barx<-barplot(1:3)
#' error_bar(barx, 1:3,c(.1,.1,.1))

error_bar <- 
  function(x, y, upper, lower=upper, length=0.1,...)
  {
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
      stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  }