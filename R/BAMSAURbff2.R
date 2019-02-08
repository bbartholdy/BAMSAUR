 #' Sample evaluation
#'
#' This function is a modified version of the BAMSAUR.bff function developed to work in the BAMSAUR shiny app, and is not intended for use in the R-console.
#' @param data Data frame, or object that can be coerced into a data frame, containing the ages and wear scores of individuals from a sample.
#' @param interval Character. The type of age interval used. Can be either "prediction" or "confidence" intervals.
#' @param level Numeric. Determines the level of confidence or prediction intervals. A number between 0 and 1 (not inclusive).
#' @param varmod.method Character. Method for creating the variance model in the 'earth' function. See 'earth' package for more details.
#' @param nfold Numeric. The number of folds to be used in the cross validation.
#' @param ncross Numeric. Number of cross validations. Default is set at 3 to reduce computation time.

#' @details BAMSAUR.bff uses the "lm" function for the regression analyses. It automatically provides the accuracy of the sample being evaluated, based on whether the actual age of an individual falls within the estimated age range from linear and quadratic regression analyses (using the described LOOCV method).
#' It also provides the mean of the estimated age ranges, as an indicator of method precision, as well as the sum of squares of the residuals, and the r-squared value, for both the linear and quadratic regressions.
#'
#' @note If the slope of the regression is significantly different (2 standard deviations) from 0 or 1, a warning message will appear.
#' @import AICcmodavg ggplot2 ggfortify
#' @importFrom earth earth
#' @importFrom grDevices rgb
#' @importFrom stats lm na.omit AIC BIC rstandard
#' @importFrom graphics plot
#Function specifically for the BAMSAUR app
BAMSAUR.bff2 <- function(data, interval = "prediction", level = 0.68, varmod.method = "earth", nfold = n-1, ncross = 3) {
  data <- na.omit(data)
  age.data <- as.numeric(data[,1])
  wear.data <- as.numeric(data[,2])
  n <- as.numeric(length(age.data))
#MARS
  MARS <- earth(age.data ~ wear.data, data, varmod.method = varmod.method, nfold = nfold, ncross = ncross)
  pred.mars <- predict(MARS, wear.data, type = "earth", interval = "pint", level = level)
  pred.mars <- cbind(age.data, pred.mars$fit, (pred.mars$upr - pred.mars$lwr)/2, pred.mars$lwr, pred.mars$upr)
  colnames <- c("age", "estimate", "+- years", "lower", "upper")
  out.mars <- summary(MARS)
  mars.PRESS <- round(sum(MARS$residuals^2),2)
  mars.plot <- BAM.plot(MARS, data, interval, level)
  mars.rsq <- round(MARS$rsq,2)
  mars.gcv <- round(MARS$gcv,2)
  int.mars <- round(pred.mars[,3], 2)
  mars.prec <- mean(int.mars)
  mars.eval <- plot(MARS)
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
  cub.PRESS <- round(sum(rstandard(cub, type = "predictive")^2),2)

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
  quad.bic <- round(BIC(quad), digits = 2)
  quad.PRESS <- round(sum(rstandard(quad, type = "predictive")^2),2)

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
  lin.bic <- round(BIC(lin), digits = 2)
  lin.PRESS <- round(sum(rstandard(lin, type = "predictive")^2),2)

#is the slope of the regression significantly different from 0 or 1?
  coef.sig.lin <- a.lin +- (2*out.lin$coefficients[[2,2]]) != 1|0
    if(coef.sig == "FALSE"){
    warning("slope of the linear regression is not significantly different from zero")
  }

#LOOCV of the data
CV.cub <- BAMSAUR.LOOCV(cub, interval = interval, level = level)
CV.quad <- BAMSAUR.LOOCV(quad, interval = interval, level = level)
CV.lin <- BAMSAUR.LOOCV(lin, interval = interval, level = level)
CV.mars <- BAMSAUR.LOOCV(MARS, data = data, interval = interval, level = level)
#Creation of the plots
Int.cub <- Interval(wear = wear.data, wear.data = wear.data, interval = interval, level = level, df = quad.df, s = quad.s)
  upp.int.cub <- cub.fitted + Int.cub
  low.int.cub <- cub.fitted - Int.cub

Int.quad <- Interval(wear = wear.data, wear.data = wear.data, interval = interval, level = level, df = quad.df, s = quad.s)
  upp.int.quad <- quad.fitted + Int.quad
  low.int.quad <- quad.fitted - Int.quad

Int.lin <- Interval(wear = wear.data, wear.data = wear.data, interval = interval, level = level, df = lin.df, s = lin.s)
  upp.int.lin <- lin.fitted + Int.lin
  low.int.lin <- lin.fitted - Int.lin
Int.lin <- round(Int.lin, 2)
Int.quad<- round(Int.quad, 2)
Int.cub <- round(Int.cub, 2)

requireNamespace("ggfortify")
cub.eval <- autoplot(cub)
quad.eval <- autoplot(quad)
lin.eval <- autoplot(lin)

#Plots
lin.plot <- BAM.plot(lin, interval = interval, level = level)
quad.plot <- BAM.plot(quad, interval = interval, level = level)
cub.plot <- BAM.plot(cub, interval = interval, level = level)

cat("\n")
cat("Linear model"); cat("\n")
cat("R-squared:"); cat("\t"); cat(round(lin.rsq, digits = 2)); cat("\n")
cat("PRESS:"); cat("\t"); cat("\t"); cat(round(lin.PRESS, digits = 2)); cat("\n")
cat("AIC:"); cat("\t"); cat("\t"); cat(lin.aic); cat("\n")
cat("BIC:"); cat("\t"); cat("\t"); cat(lin.bic); cat("\n")
cat("Accuracy:"); cat("\t"); cat(round(CV.lin$accuracy, digits = 2)); cat("%"); cat("\n")
cat("Average range:"); cat("\t"); cat("+-"); cat(" "); cat(round(mean(Int.lin),2)); cat(" "); cat("years"); cat("\n")
cat("\t"); cat("(min, max: "); cat(min(Int.lin));cat(", "); cat(max(Int.lin)); cat(")"); cat("\n")
cat("\n"); cat("Quadratic model"); cat("\n")
cat("R-squared:"); cat("\t"); cat(round(quad.rsq, digits = 2)); cat("\n")
cat("PRESS:"); cat("\t"); cat("\t"); cat(round(quad.PRESS, digits = 2)); cat("\n")
cat("AIC:"); cat("\t"); cat("\t"); cat(quad.aic); cat("\n")
cat("BIC:"); cat("\t"); cat("\t"); cat(quad.bic); cat("\n")
cat("Accuracy:"); cat("\t"); cat(round(CV.quad$accuracy, digits = 2)); cat("%"); cat("\n")
cat("Average range:"); cat("\t"); cat("+-"); cat(" "); cat(round(mean(Int.quad),2)); cat(" "); cat("years"); cat("\n")
cat("\t"); cat("(min, max: "); cat(min(Int.quad));cat(", "); cat(max(Int.quad)); cat(")"); cat("\n")
cat("\n"); cat("Cubic model"); cat("\n")
cat("R-squared:"); cat("\t"); cat(round(cub.rsq, digits = 2)); cat("\n")
cat("PRESS:"); cat("\t"); cat("\t"); cat(round(cub.PRESS, digits = 2)); cat("\n")
cat("AIC:"); cat("\t"); cat("\t"); cat(cub.aic); cat("\n")
cat("BIC:"); cat("\t"); cat("\t"); cat(cub.bic); cat("\n")
cat("Accuracy:"); cat("\t"); cat(round(CV.cub$accuracy, digits = 2)); cat("%"); cat("\n")
cat("Average range:"); cat("\t"); cat("+-"); cat(" "); cat(round(mean(Int.cub), digits = 2)); cat(" "); cat("years"); cat("\n")
cat("\t"); cat("(min, max: "); cat(min(Int.cub));cat(", "); cat(max(Int.cub)); cat(")"); cat("\n")
cat("\n"); cat("MARS"); cat("\n")
cat("R-squared:"); cat("\t"); cat(mars.rsq); cat("\n")
cat("PRESS:"); cat("\t"); cat("\t"); cat(mars.PRESS); cat("\n")
cat("GCV:"); cat("\t"); cat("\t"); cat(mars.gcv); cat("\n")
cat("Accuracy:"); cat("\t"); cat(round(CV.mars$accuracy, digits = 2)); cat("%"); cat("\n")
cat("Average range"); cat("\t"); cat("+-"); cat(" "); cat(mars.prec); cat(" "); cat("years"); cat("\n")
cat("\t"); cat("(min, max: "); cat(min(int.mars));cat(", "); cat(max(int.mars)); cat(")"); cat("\n")

  invisible(list("lin.plot" = lin.plot, "quad.plot" =  quad.plot, "cub.plot" = cub.plot, "mars.plot" = mars.plot, "linplot.eval" = lin.eval, "quadplot.eval" = quad.eval, "cubplot.eval" = cub.eval, "marsplot.eval" = mars.eval,
                 "linear" = out.lin, "quadratic" = out.quad, "cubic" = out.cub, "mars" = out.mars, "mars.model" = MARS,
                 "linAIC" = lin.aic, "quadAIC" = quad.aic, "cubAIC" = cub.aic, "linBIC" = lin.bic, "quadBIC" = quad.bic, "cubBIC" = cub.bic, "linPRESS" = lin.PRESS, "quadPRESS" = quad.SSres, "cubPRESS" = cub.PRESS, "marsPRESS" = mars.PRESS,
                 "lin.data" = CV.lin$out, "quad.data" = CV.quad$out, "cub.data" = CV.cub$out, "mars.data" = CV.mars$out,
                 "acc.lin" = CV.lin$accuracy, "acc.lin.1" = CV.lin$accuracy.1, "acc.lin.2" = CV.lin$accuracy.2, "acc.quad" = CV.quad$accuracy, "acc.quad.1" = CV.quad$accuracy.1, "acc.quad.2" = CV.quad$accuracy.2, "acc.cub" = CV.cub$accuracy, "acc.cub.1" = CV.cub$accuracy.1, "acc.cub.2" = CV.cub$accuracy.2, "acc.mars" = CV.mars$accuracy, "acc.mars.1" = CV.mars$accuracy.1, "acc.mars.2" = CV.mars$accuracy.2))
}
