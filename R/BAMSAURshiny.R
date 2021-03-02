#' Age-at-death estimation
#'
#' This function is used to estimate age-at-death in the BAMSAUR shiny app.
#' @param wear Numeric. The wear score of an individual. Can be a single value or a vector containing a list of wear scores from multiple individuals.
#' @param data Data frame containing one column for ages-at-death and a second column for the average wear scores. Not required when pop = "MB11" is selected.
#' @param model Character. "linear", "quadratic", "cubic", and "mars" are supported.
#' @param pop Character. Indicates which reference population to use. "MB11" and "other" are supported. When "other" is selected, the data input is required.
#' @param interval Character. The type of age interval used. Can be either "prediction" or "confidence" intervals. The default is set at "prediction".
#' @param level Numeric. Determines the level of confidence or prediction intervals. Is a number between 0 and 1 (not inclusive). 0.68, 0.90, or 0.95 is recommended. The default is set at 0.68.
#' @param varmod.method Method used to build the variance model. See ?earth for more details.
#' @param nfold Number of folds.
#' @param ncross Number of cross validations.
#' @param mars.int Logical. If 'TRUE', uses prediction intervals from the MARS model. Recommended if the data show some heteroscedasticity (but not enough to warrant a MARS model). Default set as 'TRUE'.
#' @details This function uses the "lm" function for the regression models, and "earth" for the MARS models.
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
#' @keywords Age-at-death estimation

BAMSAURshiny <- function(wear, data = NULL, model = "quadratic", pop = "MB11", interval = "prediction", level = 0.68, varmod.method = "earth", nfold = n-1, ncross = 3, mars.int= T){

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
    wear <- as.data.frame(wear)
    colnames(wear) <- "Wear"
    colnames(data) <- c("Age", "Wear")
    model <- lm(Age ~ poly(Wear, rank, raw = T), data = data)
    if(mars.int == TRUE){
      new.wear <- wear
      pred <- predict(model, newdata = wear, interval = interval, level = level)
      MARS <- earth(age.data ~ wear.data, data, varmod.method = varmod.method, nfold = nfold, ncross = ncross)
      pred.mars <- predict(MARS, newdata = new.wear, type = "earth", interval = "pint", level = level)
      age.est <- round(pred[,1], 2)
      Int <- round((pred.mars[,3] - pred.mars[,2])/2, 2)
      low <- round(pred.mars[,2],2)
      upp <- round(pred.mars[,3],2)
    }else{
      pred <- predict(model, newdata = wear, interval = interval, level = level)
      age.est <- round(pred[,1], 2)
      Int <- round((pred[,3] - pred[,2])/2, 2)
      low <- round(pred[,2],2)
      upp <- round(pred[,3],2)
    }
  }else{
    new.wear <- wear
    age.data <- data[,1]
    wear.data <- data[,2]
    MARS <- earth(age.data ~ wear.data, data, varmod.method = varmod.method, nfold = nfold, ncross = ncross)
    pred <- predict(MARS, newdata = new.wear, type = "earth", interval = "pint", level = level)
    age.est <- round(pred[,1], 2)
    Int <- round((pred[,3] - pred[,2])/2, 2)
    low <- round(pred[,2],2)
    upp <- round(pred[,3],2)
    }

#output
  result <- cbind(wear, age.est, Int, low, upp)
  colnames(result) <- c("wear", "estimate", "+- years", "lower", "upper")
  result <- as.data.frame(result)
  return(result)
}

