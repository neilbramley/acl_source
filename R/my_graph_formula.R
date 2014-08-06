#' A slight mod of graph.formul() from igraphs to avoid the automatic removal of unconnected nodes
#'
#' A slight mod of graph.formul() from igraphs to avoid the automatic removal of unconnected nodes
#' See graph.formula for details
#' @keywords igraph
#' @export
#' @examples
#' g1<-my.graph.formula(A--+B, C)
#' plot(g1)

my.graph.formula<-function (...) 
{
  mf <- as.list(match.call())[-1]
  f <- function(x) {
    if (is.call(x)) {
      return(list(as.character(x[[1]]), lapply(x[-1], f)))
    }
    else {
      return(NULL)
    }
  }
  ops <- unlist(lapply(mf, f))
  if (all(ops %in% c("-", ":"))) {
    directed <- FALSE
  }
  else if (all(ops %in% c("-", "+", ":"))) {
    directed <- TRUE
  }
  else {
    stop("Invalid operator in formula")
  }
  f <- function(x) {
    if (is.call(x)) {
      if (length(x) == 3) {
        return(list(f(x[[2]]), op = as.character(x[[1]]), 
                    f(x[[3]])))
      }
      else {
        return(list(op = as.character(x[[1]]), f(x[[2]])))
      }
    }
    else {
      return(c(sym = as.character(x)))
    }
  }
  ret <- lapply(mf, function(x) unlist(f(x)))
  v <- unique(unlist(lapply(ret, function(x) {
    x[names(x) == "sym"]
  })))
  ret <- lapply(ret, function(x) {
    res <- list()
    for (i in seq(along = x)) {
      if (x[i] == ":" && names(x)[i] == "op") {
      }
      else if (i > 1 && x[i - 1] == ":" && names(x)[i - 
                                                      1] == "op") {
        res[[length(res)]] <- c(res[[length(res)]], unname(x[i]))
      }
      else {
        res <- c(res, x[i])
      }
    }
    res
  })
  edges <- numeric()
  for (i in seq(along = ret)) {
    prev.sym <- character()
    lhead <- rhead <- character()
    for (j in seq(along = ret[[i]])) {
      act <- ret[[i]][[j]]
      if (names(ret[[i]])[j] == "op") {
        if (length(lhead) == 0) {
          lhead <- rhead <- act
        }
        else {
          rhead <- act
        }
      }
      else if (names(ret[[i]])[j] == "sym") {
        for (ps in prev.sym) {
          for (ps2 in act) {
            if (lhead == "+") {
              edges <- c(edges, unname(c(ps2, ps)))
            }
            if (!directed || rhead == "+") {
              edges <- c(edges, unname(c(ps, ps2)))
            }
          }
        }
        lhead <- rhead <- character()
        prev.sym <- act
      }
    }
  }
  ids <- seq(along = v)
  names(ids) <- v
  res <- graph(unname(ids[edges]), n = length(v), directed = directed)
  #if (simplify) 
  #  res <- simplify(res)
  res <- set.vertex.attribute(res, "name", value = v)
  res
}
