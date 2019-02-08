#' Age-at-death estimation
#'
#' This function can be used to estimate age-at-death with the choice of using Middenbeemster as a reference population or incorporating a sample from the R-environment.
#' @param wear Numeric. The wear score of an individual. Can be a single value or a vector containing a list of wear scores from multiple individuals.
#' @param data Data frame containing one column for ages-at-death and a second column for the average wear scores. Not required when pop = "MB11" is selected.
#' @param pop Character. Indicates which reference population to use. "MB11" and "other" are supported. When "other" is selected, the data input is required.
#' @param model Character. The regression method used for the age-at-death estimation; "linear", "quadratic", "cubic", and "mars" are supported.
#' @param interval Character. The type of age interval used. Can be either "prediction" or "confidence" intervals. The default is set at "prediction".
#' @param level Numeric. Determines the level of confidence or prediction intervals. Is a number between 0 and 1 (not inclusive). 0.68, 0.90, or 0.95 is recommended. The default is set at 0.68.
#' @param varmod.method Character. Method for creating the variance model in the 'earth' function. See ?earth for more details.
#' @param nfold Numeric. The number of folds to be used in the cross validation.
#' @param ncross Numeric. Number of cross validations. Default is set at 3 to reduce computation time.
#' @param ... can be used to pass additional arguments to the 'lm' and 'earth' functions.
#' @details This function uses the 'lm' and 'earth' functions for the regression analysis, and the 'predict' function for the prediction of age and intervals.
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
#' @seealso \code{\link[earth]{earth.default}}
#' @seealso \code{\link[stats]{lm}}
#' @importFrom stats lm na.omit predict
#' @importFrom earth earth
#' @example inst/BAMSAURex.R
#' @export BAMSAUR

BAMSAUR <- function(wear, data = NULL, pop = "MB11", model = "quadratic", interval = "prediction", level = 0.68, varmod.method = "earth", nfold = n - 1, ncross = 3, ...) {

  new.wear <- wear
  if(model == "linear"){
    rank <- 1
  } else if(model == "quadratic"){
    rank <- 2
  } else if(model == "cubic"){
    rank <- 3
  } else if(model == "mars"){
    rank <- NULL
  }

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
  if(is.null(rank) == FALSE){
#regression models
    model <- lm(age.data ~ poly(wear.data, rank, raw = T), ...)
    output <- summary(model)
    rsq <- model$r.squared
    s <- output$sigma
    df <- model$df.residual
    SSres <- sum(output$residuals^2)
    intercept <- model$coefficients[[1]]
    a <- model$coefficients[[2]]
    if(rank == 1){
      age.est <- a * new.wear + intercept
      age.est <- round(age.est, digits = 2)
    } else if(rank == 2){
      a2 <- model$coefficients[[3]]
      age.est <- a2 * new.wear^2 + a * new.wear + intercept
      age.est <- round(age.est, digits = 2)
    } else if(rank == 3){
      a2 <- model$coefficients[[3]]
      a3 <- model$coefficients[[4]]
      age.est <- a3 * new.wear^3 + a2 * new.wear^2 + a * new.wear + intercept
      age.est <- round(age.est, digits = 2)
    }
#Calculation of age intervals
    Int <- Interval(new.wear, wear.data, interval, level, df, s)

  }else{
#MARS model
    if(interval == "confidence"){
      warning("Confidence intervals not supported for MARS models; returning prediction intervals")
    }
    message("Calculating...")
    age <- data[,1]
    wear <- data[,2]
    MARS <- earth(age ~ wear, data, varmod.method = varmod.method, nfold = nfold, ncross = ncross,...)
    pred.mars <- predict(MARS, newdata = new.wear, type = "earth", interval = "pint", level = level)
    age.est <- round(pred.mars[,1], 2)
    Int <- (pred.mars[,3] - pred.mars[,2])/2
  }
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
