# depricated functions
#' \lifecycle{depricated}
#' @export
BAMSAUR <- function(wear, data = NULL, rank = 2, pop = "MB11", class.cal = F, interval = "prediction", level = 0.68, mars.int = F, ...){
warning("`BAMSAUR()` is deprecated as of BAMSAUR 0.1.0. Please use `bamsaur()` instead")
  wear <- as.data.frame(wear)
  colnames(wear) <- "Wear"

  if(pop == "other"){
    data <- as.data.frame(data)
    data <- na.omit(data)
    age.data <- data[,1]
    wear.data <- data[,2]
    colnames(data) <- c("Age", "Wear")
  } else{
    data <- as.data.frame(BAMSAUR::MBsimple)
    age.data <- data[,1]
    wear.data <- data[,2]
  }
  n <- as.numeric(length(age.data))
  maxAge <- max(age.data)
  #regression models
  model <- lm(Age ~ poly(Wear, rank, raw = T), data = data, ...)
  pred <- predict(model, newdata = wear, interval = interval, level = level)
  age.est <- round(pred[,1], 2)

  #Create prediction intervals based on a MARS model of the data
  if(mars.int == TRUE){
    MARS <- earth(Age ~ Wear, data, varmod.method = "earth", nfold = n - 1, ncross = 3)
    pred.mars <- predict(MARS, newdata = wear, type = "earth", interval = "pint", level = level)
    Int <- (pred.mars[,3] - pred.mars[,2])/2
    low <- age.est - Int
    upp <- age.est + Int
  } else {

    Int <- round((pred[,3] - pred[,2])/2, 2)
    low <- round(pred[,2],2)
    upp <- round(pred[,3],2)
  }

  maxEst <- max(age.est)
  #output
  if(maxEst > maxAge){
    warning("One or more of the predicted ages are beyond the range of the reference sample")
  }
  result <- cbind(wear, age.est, round(Int, 2), round(low, 2), round(upp,2))
  colnames(result) <- c("wear", "estimate", "+- years", "lower", "upper")
  result <- as.data.frame(result)
  return(result)
}

