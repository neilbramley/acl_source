#' Resave a rData file with all the previous stuff plus more
#'
#' Takes an exisiting rData file and adds to it
#' @param file the file to add to
#' @keywords resave
#' @export
#' @examples
#' resave(file='tmp_file.rdata', x=1)


resave <- function(..., list = character(), file)
{
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}