#' @title Calculate and Plot Survival Curve
#'
#' @description This function takes a numerical vector of status and a numerical vector of time, creates a data frame with columns \code{time} and \code{status}, sorts it by time in ascending order,
#' removes censored data, calculates survival probabilities, and plots a survival curve using the \code{plot} function.
#'
#' @param status A numerical vector indicating the status of each individual (0 = observation censored, 1 = event observed).
#' @param time A numerical vector indicating the time at which each event occurred or each individual was censored.
#'
#' @return A numerical vector representing the survival probabilities at each unique time point.
#'
#' @export
#'
#' @importFrom graphics plot
#'
#' @examples
#' # Generate example data
#' status = c(1, 1, 0, 0, 1)
#' time = c(10, 15, 20, 25, 30)
#'
#' # Plot survival curve
#' survCurv(status, time)
#'

survCurv = function(status, time) {
  # Create data frame with cols time and status
  d = data.frame(time = time, status = status)
  # Sort d by time in ascending order
  d = d[order(d$time), ]
  # Remove censored data
  d = d[d$status == 1, ]

  # Calculate survival probabilities
  n = length(d$status)
  time_points = sort(unique(d$time))
  events = numeric(length(time_points))
  at_risk = numeric(length(time_points))
  surv_prob = numeric(length(time_points))

  # Calculate num of events and num of at risk individuals for each unique time point
  for (i in seq_along(time_points)) {
    t = time_points[i]
    events[i] = sum(d$time == t)
    at_risk[i] = sum(d$time >= t)
  }

  # Calculate survival probability for every time point in d
  surv_prob = cumprod((at_risk - events)/at_risk)

  # Plot survival curve
  plot(time_points, surv_prob, type = "s",
       xlab = "Time", ylab = "Survival Probability",
       main = "Survival Curve")

  return(surv_prob)
}
