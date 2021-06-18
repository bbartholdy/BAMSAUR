# a script for utilities used inside multiple functions

# e.g. interval function
print.bamsaur <- function(x){
  print(x$estimate)
}

print.validataur <- function(x){
  invisible(x)
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

# Calculating the accuracy of age estimates
accusaur <- function(pred_obj, y){
  x <- as.data.frame(pred_obj)
  diff <- x$fit - y

  # Quantifying absolute accuracy, i.e. if the predicted range contains the actual known age
  # The lower intervals are converted to integers in order to capture the appropriate accuracy
    # (otherwise a prediction of 5.3 for a 5 year old would be classified as wrong)
  test_acc <- (y > as.integer(x$lwr) | y == x$lwr) &
    (y < x$upr | y == x$upr)
  test_acc <- as.numeric(test_acc)
  accuracy <- mean(test_acc) * 100

  # Quantifying relative accuracy, i.e. if the predicted age is within 1, 2, 5, and 10 years of the known age
  test_acc1 <- (diff < 1 | diff == 1)
  test_acc2 <- (diff < 2 | diff == 2)
  test_acc5 <- (diff < 5 | diff == 5)
  test_acc10 <- (diff < 10 | diff == 10)
  acc_list <- list(test_acc, test_acc1, test_acc2, test_acc5, test_acc10)
  acc <- lapply(acc_list, function(x) mean(as.numeric(x)) * 100)
  names(acc) <- c("accuracy", "accuracy.1", "accuracy.2", "accuracy.5", "accuracy.10")
  accusaur_out <- data.frame(x, diff)
  colnames(accusaur_out) <- c("estimate", "lwr", "upr", "difference")
  #list("out" = accusaur_out, "accuracy" = accuracy, "accuracy.1" = accuracy1, "accuracy.2" = accuracy2, "accuracy.5" = accuracy5, "accuracy.10" = accuracy10)
  return(list("out" = accusaur_out, "acc" = acc))
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
