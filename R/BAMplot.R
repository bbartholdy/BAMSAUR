#'Creates a plot from dental wear data
#'
#'This function plots dental wear data using ggplot and can depict the confidence or prediction intervals.
#'@param object object of type 'lm' or 'earth'.
#'@param data Data frame containing the data used to create the plot.
#'@param interval The type of age interval used. Can be either "prediction" or "confidence" intervals.
#'@param level Determines the level of confidence or prediction intervals.
#'@return A plot of the entered data.
#'@import ggfortify ggplot2
#'@importFrom grDevices rgb
#'@importFrom stats lm qt sd

BAM.plot <- function(object, data, interval = interval, level = level){

if(class(object) == "lm"){
  rank <- object$rank
  data <- object$model[,1:rank]
  out <- summary(object)
  fit <- object$fitted.values
  df <- object$df.residual
  s <- out$sigma
  y <- data[,1]
  x <- data[,2]

Int <- Interval(x, x, interval = interval, level = level, df = df, s = s)

}else if(class(object) == "earth"){
interval <- "pint"
fit <- object$fitted.values
 y <- data[,1]
 x <- data[,2]
 int <- predict(object, type = "earth", interval = interval, level = level)
Int <- (int$upr - int$lwr)/2
  }
#Base plot
  pl <- ggplot(data) + geom_point(aes(x = x, y = y), size =1.5, colour = rgb(0,0.4,0.8)) +
    xlab("Wear score") + ylab ("Age (years)") +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(fill = NA, colour = "black")) +
    theme(panel.grid.major = element_line(colour = "grey")) +
    theme(panel.grid.minor = element_line(colour = "grey"))

#regression plot

   upp.int <- fit + Int
   low.int <- fit - Int
  plot <- pl +
    geom_ribbon(data = data, aes(x = x, ymin=low.int, ymax=upp.int), alpha=0.1, inherit.aes=F, fill="112233") +
    geom_line(data = data, aes(x = x, y = fit), colour = "#555555", size = 1)

  print(plot)
}
