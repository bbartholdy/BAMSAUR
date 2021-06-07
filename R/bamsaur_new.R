#' Function to estimate age based on dental wear
#'
#' This function can be used to estimate age-at-death with the choice of using
#' Middenbeemster as the default reference population or incorporating a different
#' reference population.
#' @param wear Numeric. Dental wear scores for age estimation. Can be a single
#' number or an array.
#' @param ref_pop Reference population used to estimate age from wear scores.
#' Default is the built-in "MB11" population. Other reference populations can be
#' supplied with a data frame/matrix containing ages-at-death in the first column
#' and wear scores in the second column.
#' @param degree Numeric. Degree of polynomial for the model. 1 = linear regression,
#' 2+ = polynomial regression. Default is 2.
#' @param interval Character. Type of interval for age estimates. Options are
#' "prediction" and "confidence".
#' @param level Numeric. Level for the confidence and prediction intervals.
#' Must be a number between 0 and 1.
#'
#' @importFrom stats lm predict
#'
#' @export

bamsaur <- function(wear, ref_pop = NULL, degree = 2, interval = "prediction",
                    level = 0.68, ...){

if(!is.numeric(wear)){
  stop("'wear' is not numeric")
}

if(!is.null(ref_pop)){
  if(dim(ref_pop)[2] != 2){
    stop("ref_pop must be a data frame/matrix with age in the first column and
         wear scores in the second column")
  }
} else {
  ref_pop <- BAMSAUR::MBsimple
}

if(level >= 1){
  stop("'level' must be a number between 0 and 1 (not inclusive)")
}

wear_input <- data.frame("wear" = wear)
colnames(ref_pop) <- c("age", "wear")
model <- lm(formula = age ~ poly(wear, degree, raw = T),
            data = ref_pop, ...)
age_estimate <- predict(model, newdata = wear_input, interval = interval,
                        level = level, ...)
mf <- data.frame("age" = model$model[,1], "wear" = model$model[,2])
for(i in 2:(degree + 1)){
  names(model$coefficients)[i] <- paste0("wear^", i - 1)
}

# include diagnostics in output (model, model frame, etc. that can be used in other
  # functions)
wear_datf <- wear_input
wear_datf$age_estimate <- age_estimate[,1]
wear_datf$lower <- age_estimate[,2]
wear_datf$upper <- age_estimate[,3]
bam_out <- list("estimate" = wear_datf, "model" = model, "model_frame" = mf,
                "ref_pop" = ref_pop)

max_ref <- max(ref_pop$age)
range_ref <- paste(range(ref_pop$age), collapse = "-")
max_estimate <- signif(max(wear_datf$age_estimate),3)
if(max_ref < max_estimate){
  warning(paste("One or more of the estimated ages exceed the age range of the
          reference population (", range_ref,
          ") resulting in extrapolation of some estimated ages."))
}
class(bam_out) <- c("bamsaur", class(bam_out))
return(bam_out)
}
