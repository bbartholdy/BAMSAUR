#' Prediction and confidence intervals for BAMSAUR
#'
#' This function is built into the main BAMSAUR functions and can be used to calculate the prediction and confidence intervals for individual age-at-death estimates
#' @param wear The average wear score from an individual, or a vector containing multiple scores.
#' @param wear.data A vector containing all the average wear scores from the sample used to create the age-at-death estimation equation.
#' @param interval The type of interval required. Can be "prediction" or "confidence".
#' @param level The level of the prediction or confidence interval. A number between 0 and 1 (not inclusive).
#' @param df The residual degrees of freedom from the regression analysis.
#' @param s Standard deviation of the fitted regression line.
#' @details Calculation of age intervals is conducted using the formulas for the "confidence interval for the mean value of y for a given value of x", and "the prediction interval for an individual" (Altman and Garner 2000: 76-79)
#' @return returns the age range (+- years) of the estimated age(s)-at-death.
#' @references Altman, D.G. and Gardner, M.J. (2000). Regression and correlation. In (eds) Altman, D. G., Machin, D., Bryant, T.N., and Gardner, M.J. (73-92). \emph{Statistics with Confidence}(2nd ed.). BMJ Books.
#' @importFrom stats qt sd

#prediction and confidence intervals
Interval <- function(wear, wear.data, interval, level, df, s) {
alpha <- 1 - level
n <- length(wear.data)

if(interval == "prediction"){
  Int.s <- s * sqrt(1 + (1 / n) + (((wear - mean(wear.data))^2) / ((n - 1)*sd(wear.data)^2)))
  Int <- qt((1-(alpha / 2)), df) * Int.s
  return(Int)
} else if(interval == "confidence") {
  Int.s <- s * sqrt((1 / n) + (((wear - mean(wear.data))^2) / ((n - 1)*sd(wear.data)^2)))
  Int <- qt((1-(alpha / 2)), df) * Int.s
  return(Int)
}

}
