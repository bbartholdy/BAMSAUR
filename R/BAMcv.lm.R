#' Leave-one-out cross validation function
#'
#' This function can be used to perform leave-on-out cross validation on regression models, and calculate the accuracy of a sample using the 'rstandard' and 'predict' functions.
#' @param object Object of type 'lm' or 'earth'. Linear, quadratic, and cubic regression is supported.
#' @param interval Character. The type of age interval used. Can be either "prediction" or "confidence" intervals.
#' @param level Numeric. Determines the level of confidence or prediction intervals. Can be any number between 0 and 1 (not inclusive), but one of 0.68, 0.90, or 0.95 is recommended.
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
#' @importFrom stats lm predict rstandard

BAMcv.lm <- function(object, interval = "prediction", level = 0.68){
  data.lm <- object$model
  df <- object$df.residual
  summ.lm <- summary(object)
  s <- summ.lm$sigma
  age.data <- data.lm[,1]
  wear.data <- data.lm[,2]
  n <- as.numeric(length(age.data))

  #lm objects
  #LOOCV
  est <- predict(object)
  res <- est - age.data
  age.diff <- res
  int <- Interval(wear.data, wear.data, interval = interval, level = level, df = df, s = s)
  low <- est - int
  upp <- est + int
  cv.out <- cbind(age.data, est, age.diff, low, upp)
  colnames(cv.out) <- c("age", "est", "diff", "low", "upp")
  cv.out <- as.data.frame(cv.out)

  #Quantifying absolute accuracy, i.e. if the predicted range contains the actual known age
  #The lower intervals are converted to integers in order to capture the appropriate accuracy (otherwise a prediction of 5.3 for a 5 year old would be classified as wrong)
  is.true <- (cv.out$age > cv.out$low | cv.out$age == as.integer(cv.out$low)) & (cv.out$age < cv.out$upp | cv.out$age == cv.out$upp)
  is.true <- as.numeric(is.true)
  accuracy <- mean(is.true) * 100
  #Quantifying relative accuracy, i.e. if the predicted age is within 1, 2, 5, and 10 years of the known age
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
  colnames(cv.out) <- c("Age", "Estimate", "Difference", "Lower", "Upper")
  list("out" = cv.out, "accuracy" = accuracy, "accuracy.1" = accuracy.1, "accuracy.2" = accuracy.2, "accuracy.5" = accuracy.5, "accuracy.10" = accuracy.10)

}
