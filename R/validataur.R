#'
#'
#'
#' @export

# Allow multiple regression?

# Output includes: bam object from the reference used for validation, that can be used
  # as a reference population in bamsaur function

validataur <- function(...){
  UseMethod("validataur")
}

#' @importFrom earth earth
#' @importFrom stats rstandard
#' @method validataur formula
#' @export
validataur.formula <- function(formula, data, interval = "pred", level = 0.68,
                               nfold = n - 1, ncross = 3, ...){

  if(level >= 1){
    stop("'level' must be a number between 0 and 1 (not inclusive)")
  }
  int_type <- c("confidence", "prediction")
  interval <- match.arg(interval, int_type)
  if(interval == "confidence"){
    warning("Only prediction intervals are available for MARS objects")
  }
  input_data <- data
  cat("Calculating..."); cat("\n")

# Models ------------------------------------------------------------------

model_terms <- all.vars(formula)
if(length(model_terms) > 2){
  stop("'validataur' does not currently support multiple regression")
}
  # lm
lm_formula <- paste(model_terms[1], " ~ ",
                  paste0("poly(", model_terms[2], ",", 1:3, ", raw = T)"))
lm_models <- lapply(lm_formula, lm, data = data)
#lin_model <- lm(formula, data = input_data)
names(lm_models) <- c("lin", "quad", "cub")
n <- length(lm_models$lin$residuals)
ref_pop <- lm_models$lin$model
colnames(ref_pop) <- c(model_terms[1], model_terms[2])
#lm_models$lin$call$data <- noquote("input_data")
#n <- length(lin_model$residuals)
pred_lm <- lapply(lm_models, predict, interval = interval, level = level)
#ref_pop <- lin_model$model


  # MARS

if(interval == "prediction"){
  mars_int <- "pint"
} else if (interval == "confidence"){
  mars_int <- "cint"
}

mars_model <- earth::earth(formula, data = input_data, nfold = nfold, ncross = ncross, varmod.method = "earth", ...)
pred_mars <- predict(mars_model, interval = mars_int, level = level, ...)

# Validation --------------------------------------------------------------



  # cross-validation
if(nfold == n - 1){
  lm_models_loocv <- lapply(lm_models, FUN = function(x) sum(rstandard(x)^2))
  lm_models_press <- lapply(lm_models, FUN = function(x) sum(rstandard(x, type = "pred")^2))
} else {
  stop("validataur only supports leave-one-out cross-validation. For now...")
}

  # AIC and BIC

  if(n / 2 < 40){
    lm_models_aic <- lapply(lm_models, AICcmodavg::AICc)
  } else {
    lm_models_aic <- lapply(lm_models, AIC)
  }

  lm_models_bic <- lapply(lm_models, BIC)

# Accuracy of the models
  # calculated as % of correctly predicted ages. An estimation is considered correct
  # if the actual age is within the interval of the prediction.

  acc_models <- pred_lm
  acc_models$mars <- pred_mars
  acc <- lapply(acc_models, accusaur, y = input_data[model_terms[1]])

  out <- list()
  out$lin <- do.call(rbind, acc$lin[2])
  out$quad <- do.call(rbind, acc$quad[2])
  out$cub <- do.call(rbind, acc$cub[2])
  out$mars <- do.call(rbind, acc$mars[2])
  acc_datf <- do.call(rbind, out)

  row.names(acc_datf) <- c("lin", "quad", "cub", "mars")

  validataur_out <- list("model_accuracy" = acc_datf, "lin_model" = lm_models$lin,
                         "quad_model" = lm_models$quad, "cub_model" = lm_models$cub,
                         "aic" = lm_models_aic, "bic" = lm_models_bic,
                        "earth_model" = mars_model, "ref_pop" = ref_pop)
  class(validataur_out) <- c("validataur", class(validataur_out))

# output should include a data frame that can be used in bamsaur() (like MBsimple)
  #list(acc, cbind(pred_mars, input_data[,1]))
  # estimate and actual age
  return(validataur_out)
}

