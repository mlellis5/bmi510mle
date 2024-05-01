#' Calculate the parameter p that maximizes the log-likelihood of a Bernoulli distribution
#'
#' This function takes a vector of binary outcomes 'data' and performs a grid-based search to find
#' the parameter p that maximizes the log-likelihood of observing the data under a Bernoulli
#' distribution.
#'
#' @param  data A vector of binary outcomes (0 and 1).
#' @return The p value that maximizes and the log-likelihood of the vector of binary outcomes.
#' @export
#'
#' @examples
#' data = c(1, 0, 0, 0, 1, 1, 1)
#' logLikBernoulli(data)
#'
#' @importFrom seq log sum which.max

logLikBernoulli = function(data) {
  # Grid-based search for parameter p in steps of 0.001
  pvals = seq(0, 1, by = 0.001)
  LL = numeric(length(pvals))

  # Calculate log-likelihood for each p value
  #bernoulli f(x) = p^x * (1-p)^(1-x)
  for (i in seq_along(pvals)) {
    p = pvals[i]
    LL[i] = sum(data * log(p) + (1 - data) * log(1 - p))
  }

  # Find the maximum log-likelihood and corresponding p value
  maxLL = max(LL)
  LLp = pvals[which.max(LL)]
  return(LLp)
}
