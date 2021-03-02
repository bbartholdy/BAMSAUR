#'
#'
#'
#' @export

# BAMSAUR.bff

# New name:
  # bam_validate
  # validsaur
  # validataur
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


# Models ------------------------------------------------------------------

model_terms <- all.vars(formula)
if(length(model_terms) > 2){
  stop("'validataur' does not currently support multiple regression")
}
  # lm
lm_formula <- paste(model_terms[1], " ~ ",
                 paste0("poly(", model_terms[2], ",", 1:3, ", raw = T)"))
lm_models <- lapply(lm_formula, lm, data = data)
names(lm_models) <- c("lin", "quad", "cub")
n <- length(lm_models$lin$residuals)

  # MARS

mars_model <- earth::earth(formula, data = data, nfold = nfold, ncross = ncross, varmod.method = "earth", ...)
pred_mars <- predict(mars_model, interval = "cint", level = level, ...)

# Validation --------------------------------------------------------------



  # cross-validation
if(nfold == n - 1){
  models_loocv <- lapply(lm_models, FUN = function(x) sum(rstandard(x)^2))
  models_press <- lapply(lm_models, FUN = function(x) sum(rstandard(x, type = "pred")^2))
}

  # AIC and BIC

if(n / 2 < 40){
  models_aic <- lapply(lm_models, AICcmodavg::AICc)
} else {
  models_aic <- lapply(lm_models, AIC)
}

models_bic <- lapply(lm_models, BIC)

# Accuracy of the models
  # calculated as % of correctly predicted ages. An estimation is considered correct
  # if the actual age is within the interval of the prediction.

validataur_out <- list("lm_models" = lm_models, "aic" = models_aic, "bic" = models_bic,
                       "earth_model" = mars_model)
class(validataur_out) <- "validataur"

  return(validataur_out)
}

