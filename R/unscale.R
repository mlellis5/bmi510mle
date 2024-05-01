#' @title Reverse the Scaling/Centering of a Vector
#'
#' @description If the input vector `x` has been scaled and centered using the \code{\link{scale}} function, this function will reverse the centering and scaling to return the original unscaled vector. If the input vector has not been scaled or centered, it will be returned as-is.
#'
#' @param x A vector that has been scaled and/or centered.
#'
#' @return A vector with the original scale and centering reversed.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' scaled_vector = scale(1:10)
#' unscaled_vector = unscale(scaled_vector)
#'

unscale = function(x) {
  # create numeric vector without attributes
  xvec = as.vector(x)

  # has vector x been scaled/centered? (i.e. does it have scale attributes?)
  if(is.null(attr(x, "scaled:center"))==FALSE && is.null(attr(x,"scaled:scale"))==FALSE){
    mean = attr(x, "scaled:center")
    sd = attr(x,"scaled:scale")
    unscaled = xvec*sd+mean
  }
  # return vector as-is if it hasnt been scaled/centered
  else
    unscaled = xvec
  return(unscaled)

}
