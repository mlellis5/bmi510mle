#' @title Minimum Sample Size for T-Test
#'
#' @description This function takes in either one or 2 numerical vectors, calculates the minimum sample size needed for a t-test of the null hypotheses
#' that either mu_x1 == 0 for one sample input or mu_x1 == mu_x2 for two samples with 80% power at alpha=0.05. It is a wrapper around
#' the pwr::pwr.t2n.test function.
#'
#' @param x1 A numerical vector representing the first sample.
#' @param x2 A numerical vector representing the second sample (optional).
#'
#' @return The minimum sample size needed for the t-test.
#'
#' @importFrom pwr pwr.t.test pwr.t2n.test
#'
#' @export
#'
#' @examples
#' # Calculate minimum sample size for one-sample t-test
#' x1 = rnorm(50, mean = 0, sd = 1)
#' min_n = minimumN(x1)
#'
#' # Calculate minimum sample size for two-sample t-test
#' x1 = rnorm(50, mean = 0, sd = 1)
#' x2 = rnorm(60, mean = 0.5, sd = 1)
#' min_n = minimumN(x1, x2)
#'

minimumN = function(x1,x2 = NULL){
  # set power equal to 80% and alpha to 0.05
  power = 0.8
  alpha = 0.05

  # if there is only 1 set of prelim data: calculate min sample size for 1 sample t test
  if(is.null(x2)){
    d = abs(mean(x1)/sd(x1))
    result = pwr::pwr.t.test(d = d, sig.level = alpha, power = power, type = "one.sample")
    min_n = ceiling(result$n)
  }
  else {
    #calculate sample size of both samples
    n1 = length(x1)
    n2 = length(x2)

    # Calculate minimum sample size for two-sample t-test
    df = n1 + n2 - 2
    pooled_sd = sqrt(((n1-1)*stats::sd(x1)^2 + (n2-1)*stats::sd(x2)^2)/df)
    d = (mean(x1) - mean(x2)) / pooled_sd

    # function to calculate power of 2 sample t test using pwr::pwr.t2n.test
    calculatePower = function(n) {
      # If both samples are equal size, set both sample sizes equal to the minimum n
      if (n1 == n2)
        return(pwr::pwr.t2n.test(n1 = n, n2 = n, d = d, sig.level = alpha, power = NULL)$power)
      # If the minimum sample size n is greater than those of x1 and x2 and x1 is larger than x2, calculate power for minimum sample size of x1
      if( n1 < n && n2 < n && n1 > n2)
        return(pwr::pwr.t2n.test(n1 = n, n2 = n2, d = d, sig.level = alpha, power = NULL)$power)
      # If the minimum sample size n is greater than those of x1 and x2 and x1 is smaller than x2, calculate power for minimum sample size of x2
      if( n1 < n && n2 < n && n1 < n2)
        return(pwr::pwr.t2n.test(n1 = n1, n2 = n, d = d, sig.level = alpha, power = NULL)$power)
      else
        return(pwr::pwr.t2n.test(n1 = min(n1, n), n2 = min(n2, n), d = d, sig.level = alpha, power = NULL)$power)

    }

    # Start with the smallest possible sample size and calculate power
    min_n = 2
    current_power = calculatePower(min_n)


    # Increment the minimum sample size until power of 0.8 is reached
    while (current_power < power) {
      min_n = min_n + 1
      current_power = calculatePower(min_n)
    }
  }

  return(min_n)
}
