#' Turn numbers to binary
#'
#' This function just turns a number into a binary vector, useful for listing all permutations of two level conditions
#' @param number the number
#' @param noBits the number of bits to include, if omitted defaults to 32
#' @keywords bits
#' @export
#' @examples
#' cat_function()

num2bin = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}