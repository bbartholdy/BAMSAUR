#' Leave-one-out cross validation function
#'
#' This function can be used to perform leave-on-out cross validation on regression models, and calculate the accuracy of a sample using the 'rstandard' and 'predict' functions.
#' @param object Object of type 'lm' or 'earth'. Linear, quadratic, and cubic regression is supported.
#' @param data Data frame containing the data used to obtain the regression model used in the 'object' field. Only necessary for 'earth' objects.
#' @param type Character. The type of prediction. "link", "response", or "earth".
#' @param level Numeric. Determines the level of confidence or prediction intervals. Can be any number between 0 and 1 (not inclusive), but one of 0.68, 0.90, or 0.95 is recommended.
#' @param ... Additional arguments can be passed to the 'earth' function. See ?earth for more details.
#' @details The main use of this function is within the BAMSAUR.bff function, but can also be used as a stand-alone function. The level of accuracy is determined by the type and level of the age intervals.
#' Accuracy is also determined by the percentage of cases that fall within 1 and 2 years of the age estimate, which is independent of the age interval size.
#' @return Returns a list containing the following:
#' \describe{
#'  \item{\code{out}}{the data from the LOOCV, including known age, estimated age, difference between estimated and known ages, and the lower and upper age range.}
#'
#'  \item{\code{accuracy}}{The percentage of estimates whose range contains the known age.}
#'
#'  \item{\code{accuracy.1}}{The percentage of estimates falling within one year of the known age.}
#'
#'  \item{\code{accuracy.2}}{The percentage of estimates falling within two years of the known age.}
#'}
#' @importFrom earth earth
#' @importFrom stats predict

BAMcv.mars <- function(object, data, type = "earth", level = 0.68, ...){

  age <- data[,1]
  wear <- data[,2]
  n <- length(age)

#prepare data frame for LOOCV
  cv.data <- as.data.frame(matrix(nrow = n, ncol = 4))
  colnames(cv.data) <- c("age", "age.est", "low.Int", "upp.Int")


#Leave-one-out loop function for MARS
  for(k in 1:n) {
    agecv <- age[-k]
    agekcv <- age[k]
    wearcv <- wear[-k]
    weark <- wear[k]
    cv <- earth(agecv ~ wearcv, varmod.method = "earth", nfold = (length(agecv) - 1), ncross = 3, ...)
    pred <- predict(cv, newdata = weark, interval = "pint", level = level)
    Int.cv <- (pred[,3] - pred[,2]) / 2
    upp.Int <- pred[,3]
    low.Int <- pred[,2]
    age.k <- pred[,1]

#calculation of prediction error

    age.k <- age.k
    upp.Int <- upp.Int
    low.Int <- low.Int

    cv.data[k,1] <- agekcv
    cv.data[k,2] <- age.k
    cv.data[k,3] <- low.Int
    cv.data[k,4] <- upp.Int
  }

  age.diff <- cv.data[,2] - age
  cv.out <- cbind(cv.data$age, cv.data$age.est, age.diff, cv.data$low.Int, cv.data$upp.Int)
  cv.out <- as.data.frame(cv.out)
  colnames(cv.out) <- c("age", "est", "diff", "low", "upp")
#Quantifying absolute accuracy, i.e. if the predicted range contains the actual known age
#The lower intervals are converted to integers in order to capture the appropriate accuracy (otherwise a prediction of 5.3 for a 5 year old would be classified as wrong)
  is.true <- (cv.out$age > cv.out$low | cv.out$age == as.integer(cv.out$low)) & (cv.out$age < cv.out$upp | cv.out$age == cv.out$upp)
  is.true <- as.numeric(is.true)
  accuracy <- mean(is.true) * 100
#Quantifying relative accuracy, i.e. if the predicted age is within 1, 2, 5, and 10 years of the known age (5 and 10 years included for potential use on adult individuals)
  is.true.1 <- (age.diff < 1 | age.diff == 1)
  is.true.2 <- (age.diff < 2 | age.diff == 2)
  is.true.5 <- (age.diff < 5 | age.diff == 5)
  is.true.10 <- (age.diff < 10 | age.diff == 10)
  is.true.1 <- as.numeric(is.true.1)
  is.true.2 <- as.numeric(is.true.2)
  is.true.5 <- as.numeric(is.true.5)
  is.true.10 <- as.numeric(is.true.10)
  accuracy.1 <- mean(is.true.1) * 100
  accuracy.2 <- mean(is.true.2) * 100
  accuracy.5 <- mean(is.true.5) * 100
  accuracy.10 <- mean(is.true.10) * 100
  cv.out <- round(cv.out,2)
  colnames(cv.out) <- c("age", "estimate", "difference", "lower", "upper")
  list("out" = cv.out, "accuracy" = accuracy, "accuracy.1" = accuracy.1, "accuracy.2" = accuracy.2, "accuracy.5" = accuracy.5, "accuracy.10" = accuracy.10)

}
