#' Age-at-death estimation with MARS
#'
#' This function can be used to estimate age-at-death using multivariate adaptive regression splines (MARS), with the choice of using Middenbeemster as a reference population or incorporating a sample from the R-evnironment.
#' @param wear Numeric. The wear score of an individual. Can be a single value or a vector containing a list of wear scores from multiple individuals.
#' @param data Data frame containing one column for ages-at-death and a second column for the average wear scores. Not required when pop = "MB11" is selected.
#' @param pop Character. Indicates which reference population to use. "MB11" and "other" are supported. When "other" is selected, the data input is required.
#' @param level Numeric. Determines the level of confidence or prediction intervals. Is a number between 0 and 1 (not inclusive). 0.68, 0.90, or 0.95 is recommended. The default is set at 0.68.
#' @param varmod.method Character. Method for creating the variance model in the 'earth' function. See 'earth' package for more details.
#' @param nfold Numeric. The number of folds to be used in the cross validation.
#' @param ncross Numeric. Number of cross validations. Default is set at 3 to reduce computation time.
#' @param ... Additional arguments can be passed to the 'earth' function. See ?earth for more details.
#' @details This function uses the "earth" function for the MARS analyses, and age ranges are provided by the predict function.
#'
#' @return a data frame containing the following values:
#' \describe{
#'  \item{\code{wear}}{The wear score(s) initially applied to the function}
#'
#'  \item{\code{estimate}}{Age-at-death estimate(s) calculated from the wear score(s)}
#'
#'  \item{\code{range}}{the age range (+- years) of the estimate, determined by the type and level.}
#'
#'  \item{\code{lower}}{the lower bound of the age interval.}
#'
#'  \item{\code{upper}}{the upper bound of the age interval.}
#' }
#' @importFrom stats lm na.omit predict
#' @importFrom earth earth
#' @example inst/BAMSAUR.marsex.R
#' @export BAMSAUR.mars
#' @keywords Age-at-death estimation
BAMSAUR.mars <- function(wear, data = NULL, pop = "MB11", level = 0.68, varmod.method = "earth", nfold = n-1, ncross = 3,...){
  new.wear <- wear
  if(pop == "other"){
    data <- as.data.frame(data)
    data <- na.omit(data)
    age.data <- data[,1]
    wear.data <- data[,2]
  } else{
    data <- as.data.frame(BAMSAUR::MBsimple)
    age.data <- data[,1]
    wear.data <- data[,2]
  }
  n <- as.numeric(length(age.data))
  #MARS model
  message("Calculating...")
  age <- data[,1]
  wear <- data[,2]
  MARS <- earth(age ~ wear, data, varmod.method = varmod.method, nfold = nfold, ncross = ncross,...)
  pred.mars <- predict(MARS, newdata = new.wear, type = "earth", interval = "pint", level = level)
  age.est <- round(pred.mars[,1], 2)
  Int <- (pred.mars[,3] - pred.mars[,2])/2

#interval calculation
Int.low <- round(age.est - Int, 2)
Int.upp <- round(age.est + Int, 2)
Int <- round(Int,2)
#output
result <- cbind(new.wear, age.est, Int, Int.low, Int.upp)
colnames(result) <- c("wear", "estimate", "+- years", "lower", "upper")
result <- as.data.frame(result)
return(result)
}
