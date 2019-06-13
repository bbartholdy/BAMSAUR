#'Creates a plot from dental wear data
#'
#'This function is a modified version of BAM.plot for compatibility with the shiny app and is not intended for use in the R-console.
#'@param x Numeric. Vector of wear scores.
#'@param y Numeric. Vector of ages-at-death associated with the wear scores.
#'@param model Character. Choice of regression model. "linear", "quadratic", and "cubic" are supported.
#'@param interval The type of age interval used. Can be either "prediction" or "confidence" intervals.
#'@param level Determines the level of confidence or prediction intervals. A number between 0 and 1 (not inclusive).
#'@return A plot of the entered data.
#'@import ggplot2
#'@importFrom grDevices rgb
#'@importFrom stats lm qt sd
#Base plot
BAM.plot2 <- function(x, y, model, interval, level){
  data <- as.data.frame(cbind(y, x))
  n <- as.numeric(length(y))
   x2 <- x^2
  x3 <- x^3
  lin <- lm(y ~ x)
  quad <- lm(y ~ x + x2)
  cub <- lm(y ~ x + x2 + x3)
  lin.out <- summary(lin)
  quad.out <- summary(quad)
  cub.out <- summary(cub)
  s.lin <- lin.out$sigma
  s.quad <- quad.out$sigma
  s.cub <- cub.out$sigma
#Linear shaded region
  Int.lin <- Interval(x, x, interval = interval, level = level, df = lin$df.residual, s = lin.out$sigma)
  upp.int.lin <- lin$fitted.values + Int.lin
  low.int.lin <- lin$fitted.values - Int.lin
#Quadratic shaded region
  Int.quad <- Interval(x, x, interval = interval, level = level, df = quad$df.residual, s = quad.out$sigma)
  upp.int.quad <- quad$fitted.values + Int.quad
  low.int.quad <- quad$fitted.values - Int.quad
#Cubic shaded region
  Int.cub <- Interval(x, x, interval = interval, level = level, df = quad$df.residual, s = quad.out$sigma)
  upp.int.cub <- cub$fitted.values + Int.cub
  low.int.cub <- cub$fitted.values - Int.cub

#Base plot
pl <- ggplot(data) + geom_point(aes(x = x, y = y), size =1.5, colour = rgb(0,0.4,0.8)) +
  xlab("Wear score") + ylab ("Age (years)") +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black")) +
  theme(panel.grid.major = element_line(colour = "grey")) +
  theme(panel.grid.minor = element_line(colour = "grey"))

if(model == "linear"){
  lin.plot <- pl +
    geom_ribbon(data = data, aes(x = x, ymin=low.int.lin, ymax=upp.int.lin), alpha=0.1, inherit.aes=F, fill="112233") +
    geom_line(data = data, aes(x = x, y = lin$fitted.values), colour = "#555555", size = 1)
  return(lin.plot)
} else if(model == "quadratic") {
  quad.plot <- pl +
    geom_ribbon(data = data, aes(x = x, ymin = low.int.quad, ymax=upp.int.quad), alpha=0.1, inherit.aes=F, fill="112233") +
    geom_line(data = data, aes(x = x, y = quad$fitted.values), colour = "#555555", size = 1)
  return(quad.plot)
} else if(model == "cubic"){
  cub.plot <- pl +
    geom_ribbon(data = data, aes(x = x, ymin = low.int.cub, ymax=upp.int.cub), alpha=0.1, inherit.aes=F, fill="112233") +
    geom_line(data = data, aes(x = x, y = cub$fitted.values), colour = "#555555", size = 1)
  return(cub.plot)

}
}
