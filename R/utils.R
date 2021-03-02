# a script for utilities used inside multiple functions

# e.g. interval function

print.bamsaur <- function(x){
  print(x$estimate)
}

print.validataur <- function(x){
  print(x$aic)
}

#' @details 'plot_bamsaur' uses ggplot2, so layers can be added to the plot to
#' customise the appearance (see examples).
#' @param object An object of type 'bamsaur' or 'validator'.
plot_bamsaur <- function(object, ...){ # allow theme etc. to be passed through

  if(class(object) == "bamsaur"){

        interval <- "pred" # needs to be able to incorporate the same interval as bamsaur object

    data <- object$model_frame
    model <- object$model
    pred_model <- predict(model, interval = interval)
    upr <- pred_model[,3]
    lwr <- pred_model[,2]
    fit <- model$fitted.values
    degree <- model$rank - 1
    #Base plot
    pl <- ggplot(data, aes(x = data[,2], y = data[,1])) +
      geom_point(size =1.5) +
      xlab(names(data)[1]) + ylab (names(data)[2])

    #regression plot

    # upp.int <- fit + Int
    # low.int <- fit - Int
      pl_out <- pl +
        geom_smooth(method = "lm", formula = y ~ poly(x, degree))
        geom_ribbon(data = data, aes(x = data[,1], ymin = lwr, ymax = upr), alpha=0.1, inherit.aes=F, fill="112233") +
      # geom_line(data = data, aes(x = data[,1], y = fit), colour = "#555555", size = 1)

    return(pl_out)

  } else if(class(object) == "validataur"){
    models <- object$models
    (lin_plot <- ggplot2::autoplot(models$lin))
    (quad_plot <- ggplot2::autoplot(models$quad))
    (cub_plot <- ggplot2::autoplot(models$cub))
  }
}

# build model for name()

# models <- for(i in 1:3){
#   model <- lm(response ~ predictor, data = datf)
# }

#lapply(models, lm, data = datf)

# cross-validation for name()



interval <- function(wear, wear_data, type, level, df, s){

}

interval <- function(wear, wear.data, type, level, df, s) {
  alpha <- 1 - level
  n <- length(wear.data)

  if(type == "prediction"){
    Int.s <- s * sqrt(1 + (1 / n) + (((wear - mean(wear.data))^2) / ((n - 1)*sd(wear.data)^2)))
    Int <- qt((1-(alpha / 2)), df) * Int.s
    return(Int)
  } else if(type == "confidence") {
    Int.s <- s * sqrt((1 / n) + (((wear - mean(wear.data))^2) / ((n - 1)*sd(wear.data)^2)))
    Int <- qt((1-(alpha / 2)), df) * Int.s
    return(Int)
  }

}
