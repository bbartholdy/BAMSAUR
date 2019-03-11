#' Sample evaluation
#'
#' This function can be used to evaluate the viability of a sample for use as a reference population. It provides information on the accuracy of a sample using LOOCV, as well as details on the regression analyses performed on the sample.
#' @param data Data frame containing one column for ages-at-death and a second column for the associated wear scores.
#' @param interval Character. The type of age interval used. Can be either "prediction" or "confidence" intervals. Confidence intervals are not available for 'earth' class objects.
#' @param level Numeric. Determines level of confidence or prediction intervals. Can be any number between 0 and 1 (not inclusive), but 0.68, 0.90, or 0.95 is recommended. To run the function without intervals, enter 0.
#' @param varmod.method Character. Method for creating the variance model in the 'earth' function. See 'earth' package for more details.
#' @param nfold Numeric. The number of folds to be used in the cross validation.
#' @param ncross Numeric. Number of cross validations. Default is set at 3 to reduce computation time.
#' @param ... Additional arguments can be passed to the 'earth' function. See ?earth for more details.
#' @details BAMSAUR.bff uses the "lm" and "earth" functions for the regression analyses. It automatically provides the accuracy of the sample being evaluated, based on whether the actual age of an individual falls within the estimated age range from linear and quadratic regression analyses (using the described LOOCV method).
#' It also provides the mean of the estimated age ranges, as an indicator of method precision, as well as the predicted residual error sum of squares (PRESS), the r-squared values, Akaike information criterion (AIC; AICc for sample sizes below 80), Bayes information criterion(BIC), and generalised cross validation (GCV).
#'
#' @return The function automatically returns a summary of the analysis, with the overall accuracy, the average age range (precision), the sum of squares of the predictive residuals (PRESS),
#' the R-squared value, AIC(c), and BIC.
#' It also includes a list containing the following items:
#'
#' \describe{
#'  \item{\code{lin.plot}}{a plot of the linear regression with prediction/confidence intervals}
#'
#'  \item{\code{quad.plot}}{a plot of the quadratic regression with prediction/confidence intervals}
#'
#'  \item{\code{cub.plot}}{a plot of the cubic regression with prediction/confidence intervals}
#'
#'  \item{\code{mars.plot}}{a plot of the MARS with prediction/confidence intervals}
#'
#'  \item{\code{linear}}{class 'lm' object from the linear regression analysis.}
#'
#'  \item{\code{quadratic}}{class 'lm' object from the quadratic regression analysis.}
#'
#'  \item{\code{cubic}}{class 'lm' object from the cubic regression analysis.}
#'
#'  \item{\code{mars}}{class 'earth' object from the MARS analysis.}
#'
#'  \item{\code{lin.data}}{the data from the linear LOOCV analysis.}
#'
#'  \item{\code{quad.data}}{the data from the quadratic LOOCV analysis.}
#'
#'  \item{\code{cub.data}}{the data from the quadratic LOOCV analysis.}
#'
#'  \item{\code{mars.data}}{the data from the MARS LOOCV analysis.}
#'
#'  \item{\code{accuracy}}{A table with the percentage of estimates whose age ranges contain the actual age, the percentage of estimates within 1 year of the actual age, and within 2 years of the actual age.}
#' }
#' @note If the slope of the regression is not statistically different (2 standard deviations) from 0 or 1, a warning message will appear.
#' @seealso \code{\link[earth]{earth.formula}}
#' @seealso \code{\link[stats]{lm}}
#' @example inst/BAMSAURbffex.R
#' @import AICcmodavg ggplot2
#' @importFrom earth earth
#' @importFrom grDevices rgb
#' @importFrom stats lm na.omit AIC BIC rstandard predict
#' @importFrom graphics plot
#' @export BAMSAUR.bff
BAMSAUR.bff <- function(data, interval = "prediction", level = 0.68, varmod.method = "earth", nfold = n-1, ncross = 3, ...) {
  if(interval == "confidence"){
    warning("Only prediction intervals are available for MARS objects")
  }
  cat("Calculating..."); cat("\n")
    data <- as.data.frame(data)
    data <- na.omit(data)
    age.data <- data[,1]
    wear.data <- data[,2]
    n <- as.numeric(length(age.data))
#MARS
    MARS <- earth(age.data ~ wear.data, data, varmod.method = varmod.method, nfold = nfold, ncross = ncross, ...)
    pred.mars <- predict(MARS, type = "earth", interval = "pint", level = level)
    pred.mars <- cbind(age.data, pred.mars$fit, (pred.mars$upr - pred.mars$lwr)/2, pred.mars$lwr, pred.mars$upr)
    colnames <- c("age", "estimate", "+- years", "lower", "upper")
    pred.mars <- as.data.frame(pred.mars)
    mars.plot <- BAM.plot(MARS, data, interval, level)
    mars.rsq <- round(MARS$rsq,2)
    mars.gcv <- round(MARS$gcv,2)
    int.mars <- round(pred.mars[,3], 2)
    mars.prec <- round(mean(int.mars),2)
#cubic regression
    wear2.data <- wear.data^2
    wear3.data <- wear.data^3
    cub <- lm(age.data ~ wear.data + wear2.data + wear3.data)
    out.cub <- summary(cub)
    cub.rsq <- out.cub$r.squared
    cub.s <- out.cub$sigma
    cub.SSres <- round(sum(cub$residuals^2), digits = 2)
    cub.fitted <- cub$fitted.values
    cub.df <- cub$df.residual
    intercept <- cub$coefficients[[1]]
    a <- cub$coefficients[[2]]
    a2 <- cub$coefficients[[3]]
    a3 <- cub$coefficients[[4]]

  if(n/2 > 40){
      cub.aic <- AIC(cub)
    } else {
      cub.aic <- AICcmodavg::AICc(cub)
    }
    cub.bic <- BIC(cub)
    cub.PRESS <- sum(rstandard(cub, type = "predictive")^2)
    coef.sig <- a +- (2*out.cub$coefficients[[2,2]]) != 1|0
    coef2.sig <- a2 +- (2*out.cub$coefficients[[3,2]]) != 1|0
    coef3.sig <- a3 +- (2*out.cub$coefficients[[4,2]]) != 1|0

    if(coef.sig == "FALSE" | coef2.sig == "FALSE" | coef3.sig == "FALSE"){
      warning("slope of the cubic regression is not significantly different from zero")
    }

  #quadratic regression
  wear2.data <- wear.data^2
  quad <- lm(age.data ~ wear.data + wear2.data)
  out.quad <- summary(quad)
  quad.rsq <- out.quad$r.squared
  quad.s <- out.quad$sigma
  quad.SSres <- round(sum(quad$residuals^2), digits = 2)
  quad.fitted <- quad$fitted.values
  quad.df <- quad$df.residual
  intercept <- quad$coefficients[[1]]
  a <- quad$coefficients[[2]]
  a2 <- quad$coefficients[[3]]

  if(n/2 > 40){
  quad.aic <- AIC(quad)
  } else {
  quad.aic <- AICcmodavg::AICc(quad)
  }
  quad.bic <- BIC(quad)
  quad.PRESS <- sum(rstandard(quad, type = "predictive")^2)
  coef.sig <- a +- (2*out.quad$coefficients[[2,2]]) != 1|0
  coef2.sig <- a2 +- (2*out.quad$coefficients[[3,2]]) != 1|0

  if(coef.sig == "FALSE" | coef2.sig == "FALSE"){
    warning("slope of the quadratic regression is not significantly different from zero")
  }

  #linear regression
  lin <- lm(age.data ~ wear.data)
  out.lin <- summary(lin)
  lin.rsq <- out.lin$r.squared
  lin.s <- out.lin$sigma
  lin.SSres <- round(sum(lin$residuals^2), digits = 2)
  lin.fitted <- lin$fitted.values
  lin.df <- lin$df.residual
  intercept.lin <- lin$coefficients[[1]]
  a.lin <- lin$coefficients[[2]]

  if(n/2){
    lin.aic <- AIC(lin)
  } else {
    lin.aic <- AICcmodavg::AICc(lin)
  }
  lin.bic <- BIC(lin)
  lin.PRESS <- sum(rstandard(lin, type = "predictive")^2)
#is the slope of the regression significantly different from 0 or 1?
  coef.sig.lin <- a.lin +- (2*out.lin$coefficients[[2,2]]) != 1|0
    if(coef.sig == "FALSE"){
    warning("slope of the linear regression is not significantly different from zero")
  }

#LOOCV of the data
CV.cub <- BAMSAUR:::BAMcv.lm(cub, interval = interval, level = level)
#CV.cub <- BAMSAUR.LOOCV(cub, interval=interval, level=level)

CV.quad <- BAMSAUR.LOOCV(quad, interval=interval, level = level)
CV.lin <- BAMSAUR.LOOCV(lin, interval = interval, level = level)

#CV.mars <- BAMSAUR.LOOCV(MARS, data, interval = "prediction", level = level)
CV.mars <- BAMSAUR:::BAMcv.mars(MARS, data = data, level = level)
mars.PRESS <- sum(CV.mars$out$difference^2)
#Accuracy table output
acc.table <- as.data.frame(matrix(nrow = 4, ncol = 3), row.names = c("linear", "quadratic", "cubic", "MARS"))
acc.table[1,1] <- CV.lin$accuracy
acc.table[1,2] <- CV.lin$accuracy.1
acc.table[1,3] <- CV.lin$accuracy.2
acc.table[2,1] <- CV.quad$accuracy
acc.table[2,2] <- CV.quad$accuracy.1
acc.table[2,3] <- CV.quad$accuracy.2
acc.table[3,1] <- CV.cub$accuracy
acc.table[3,2] <- CV.cub$accuracy.1
acc.table[3,3] <- CV.cub$accuracy.2
acc.table[4,1] <- CV.mars$accuracy
acc.table[4,2] <- CV.mars$accuracy.1
acc.table[4,3] <- CV.mars$accuracy.2
colnames(acc.table) <- c("total", "1yr", "2yrs")

#Creation of the plots
lin.plot <- BAM.plot(lin, interval=interval, level=level)
quad.plot <- BAM.plot(quad, interval=interval, level=level)
cub.plot <- BAM.plot(cub, interval=interval, level=level)

#Age ranges for calculation of precision
pred.cub <- suppressWarnings(predict(cub, interval = interval, level = level))
Int.cub <- (pred.cub[,3] - pred.cub[,2]) / 2
#Int.cub <- Interval(wear = wear.data, wear.data = wear.data, interval = interval, level = level, df = quad.df, s = quad.s)
  upp.int.cub <- cub.fitted + Int.cub
  low.int.cub <- cub.fitted - Int.cub

  pred.quad <- suppressWarnings(predict(quad, interval = interval, level = level))
  Int.quad <- (pred.quad[,3] - pred.quad[,2]) / 2
  #Int.quad <- Interval(wear = wear.data, wear.data = wear.data, interval = interval, level = level, df = quad.df, s = quad.s)
  upp.int.quad <- quad.fitted + Int.quad
  low.int.quad <- quad.fitted - Int.quad

  pred.lin <- suppressWarnings(predict(lin, interval = interval, level = level))
  Int.lin <- (pred.lin[,3] - pred.lin[,2])/2
  #Int.lin <- Interval(wear = wear.data, wear.data = wear.data, interval = interval, level = level, df = lin.df, s = lin.s)
  upp.int.lin <- lin.fitted + Int.lin
  low.int.lin <- lin.fitted - Int.lin
Int.lin <- round(Int.lin, 2)
lin.prec <- round(mean(Int.lin), 2)
Int.quad<- round(Int.quad, 2)
quad.prec <- round(mean(Int.quad), 2)
Int.cub <- round(Int.cub, 2)
cub.prec <- round(mean(Int.cub), 2)
#output
  cat("\n")
  cat(message("Linear model"));
  cat("R-squared:"); cat("\t"); cat(round(lin.rsq, digits = 2)); cat("\n")
  cat("PRESS:"); cat("\t"); cat("\t"); cat(round(lin.PRESS, digits = 2)); cat("\n")
  cat("AIC:"); cat("\t"); cat("\t"); cat(lin.aic); cat("\n")
  cat("BIC:"); cat("\t"); cat("\t"); cat(lin.bic); cat("\n")
  cat("Accuracy:"); cat("\t"); cat(round(CV.lin$accuracy, digits = 2)); cat("%"); cat("\n")
  cat("Average range:"); cat("\t"); cat("+-"); cat(" "); cat(lin.prec); cat(" "); cat("years"); cat("\n")
  cat("\t"); cat("(min, max: "); cat(min(Int.lin));cat(", "); cat(max(Int.lin)); cat(")"); cat("\n")
  cat("\n"); cat(message("Quadratic model"));
  cat("R-squared:"); cat("\t"); cat(round(quad.rsq, digits = 2)); cat("\n")
  cat("PRESS:"); cat("\t"); cat("\t"); cat(round(quad.PRESS, digits = 2)); cat("\n")
  cat("AIC:"); cat("\t"); cat("\t"); cat(quad.aic); cat("\n")
  cat("BIC:"); cat("\t"); cat("\t"); cat(quad.bic); cat("\n")
  cat("Accuracy:"); cat("\t"); cat(round(CV.quad$accuracy, digits = 2)); cat("%"); cat("\n")
  cat("Average range:"); cat("\t"); cat("+-"); cat(" "); cat(quad.prec); cat(" "); cat("years"); cat("\n")
  cat("\t"); cat("(min, max: "); cat(min(Int.quad));cat(", "); cat(max(Int.quad)); cat(")"); cat("\n")
  cat("\n"); cat(message("Cubic model"));
  cat("R-squared:"); cat("\t"); cat(round(cub.rsq, digits = 2)); cat("\n")
  cat("PRESS:"); cat("\t"); cat("\t"); cat(round(cub.PRESS, digits = 2)); cat("\n")
  cat("AIC:"); cat("\t"); cat("\t"); cat(round(cub.aic, 2)); cat("\n")
  cat("BIC:"); cat("\t"); cat("\t"); cat(round(cub.bic, 2)); cat("\n")
  cat("Accuracy:"); cat("\t"); cat(round(CV.cub$accuracy, digits = 2)); cat("%"); cat("\n")
  cat("Average range:"); cat("\t"); cat("+-"); cat(" "); cat(cub.prec); cat(" "); cat("years"); cat("\n")
  cat("\t"); cat("(min, max: "); cat(min(Int.cub));cat(", "); cat(max(Int.cub)); cat(")"); cat("\n")
  cat("\n"); cat(message("MARS"));
  cat("R-squared:"); cat("\t"); cat(mars.rsq); cat("\n")
  cat("PRESS:"); cat("\t"); cat("\t"); cat(round(mars.PRESS, 2)); cat("\n")
  cat("GCV:"); cat("\t"); cat("\t"); cat(mars.gcv); cat("\n")
  cat("Accuracy:"); cat("\t"); cat(round(CV.mars$accuracy, digits = 2)); cat("%"); cat("\n")
  cat("Average range"); cat("\t"); cat("+-"); cat(" "); cat(mars.prec); cat(" "); cat("years"); cat("\n")
  cat("\t"); cat("(min, max: "); cat(min(int.mars));cat(", "); cat(max(int.mars)); cat(")"); cat("\n")

  invisible(list("lin.plot" = lin.plot, "quad.plot" =  quad.plot, "cub.plot" = cub.plot, "mars.plot" = mars.plot, "linear" = lin, "quadratic" = quad,
                "cubic" = cub, "mars" = MARS, "lin.data" = CV.lin$out, "quad.data" = CV.quad$out, "cub.data" = CV.cub$out, "mars.data" = CV.mars$out, "accuracy" = acc.table))
  }
