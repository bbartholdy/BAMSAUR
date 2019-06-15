#' Age-at-death estimation
#'
#' This function can be used to estimate age-at-death with the choice of using Middenbeemster as a reference population or incorporating a sample from the R-evnironment.
#' @param wear Numeric. The wear score of an individual. Can be a single value or a vector containing a list of wear scores from multiple individuals.
#' @param data Data frame containing one column for ages-at-death and a second column for the average wear scores. Not required when pop = "MB11" is selected.
#' @param rank Numeric. Indicates the rank of the polynomial regression. 1 = linear, 2 = quadratic, etc. Default is 2.
#' @param pop Character. Indicates which reference population to use. "MB11" and "other" are supported. When "other" is selected, the data input is required.
#' @param interval Character. The type of age interval used. Can be either "prediction" or "confidence" intervals. The default is set at "prediction".
#' @param level Numeric. Determines the level of confidence or prediction intervals. Is a number between 0 and 1 (not inclusive). 0.68, 0.90, or 0.95 is recommended. The default is set at 0.68.
#' @param mars.int Logical. Apply MARS-sized prediction intervals.
#' @param ... Can be used to pass additional arguments to the lm function.
#' @details This function uses the "lm" function for the regression analyses.
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
#' @example inst/BAMSAURex.R
#' @export BAMSAUR
#' @keywords Age-at-death estimation

BAMSAUR <- function(wear, data = NULL, rank = 2, pop = "MB11", class.cal = F, interval = "prediction", level = 0.68, mars.int = F, ...){

  wear <- as.data.frame(wear)
  colnames(wear) <- "Wear"

  if(pop == "other"){
    data <- as.data.frame(data)
    data <- na.omit(data)
    age.data <- data[,1]
    wear.data <- data[,2]
    colnames(data) <- c("Age", "Wear")
  } else{
    data <- as.data.frame(BAMSAUR::MBsimple)
    age.data <- data[,1]
    wear.data <- data[,2]
  }
  n <- as.numeric(length(age.data))
  maxAge <- max(age.data)
#regression models
  model <- lm(Age ~ poly(Wear, rank, raw = T), data = data, ...)
  pred <- predict(model, newdata = wear, interval = interval, level = level)
  age.est <- round(pred[,1], 2)


if(mars.int == TRUE){
  MARS <- earth(Age ~ Wear, data, varmod.method = "earth", nfold = n - 1, ncross = 3)
  pred.mars <- predict(MARS, newdata = wear, type = "earth", interval = "pint", level = level)
  Int <- (pred.mars[,3] - pred.mars[,2])/2
  low <- age.est - Int
  upp <- age.est + Int
} else {

  Int <- round((pred[,3] - pred[,2])/2, 2)
  low <- round(pred[,2],2)
  upp <- round(pred[,3],2)
}

maxEst <- max(age.est)
#output
if(maxEst > maxAge){
  warning("One or more of the predicted ages are beyond the range of the reference sample")
  }
  result <- cbind(wear, age.est, round(Int, 2), round(low, 2), round(upp,2))
  colnames(result) <- c("wear", "estimate", "+- years", "lower", "upper")
  result <- as.data.frame(result)
  return(result)
}

