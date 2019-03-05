# BAMSAUR

This package provides functions for the age-at-death estimation of nonadult human skeletal remains based on dental wear. The method was developed using a Dutch post-Medieval reference population; however, the package also allows the inclusion of other (more appropriate) reference samples. The main function allows the age-at-death estimation of non-adults based on a single wear score calculated from all available teeth, and additional functions allow the user to upload and evaluate their own reference sample, from which to base the age-at-death estimations.
Users who are not familiar with the R-environment can also take advantage of a user-friendly graphic user interface (GUI).

# Installation
1. To install BAMSAUR, R must first be installed (https://cran.r-project.org/).
```r
#2. Open R, and install the package "devtools" by entering the following into the R-console:
install.packages("devtools")

#3. The BAMSAUR package can then be downloaded and installed:
devtools::install_github("bbartholdy/BAMSAUR")
library(BAMSAUR)
```
# Usage

The method was developed on Middenbeemster, a 19th century Dutch skeletal collection containing individuals with documented age-at-death. The method uses a 9-stage dental wear scoring system, applicable to the entire deciduous and permanent (excluding third molars) dentitions. The average wear scores were used to develop a quadratic regression model from which to base age-at-death predictions. Age ranges are calculated using 68% prediction intervals (PIs). Users can also elect to use linear and cubic regression, or multivariate adaptive regression splines (MARS) for age-at-death predictions. Additional functions can be used to evaluate potential reference samples by developing linear, quadratic, and cubic regression models, as well as a MARS model (using the 'earth' package), and provides information on the developed models such as r-squared values, accuracies from leave-one-out cross validations (LOOCV), predictive residual error sum of squares (PRESS), and precision (based on the average size of predicted age ranges). It also returns plots (using 'ggplot2') for each of the regression models.

# Functions

BAMSAUR age-at-death estimation

`BAMSAUR(wear, data = NULL, pop = "MB11", rank = 2, interval = "prediction", level = 0.68, ...)`
`BAMSAUR.mars(wear, data = NULL, pop = "MB11", level = 0.68, varmod.method = "earth", nfold = n-1, ncross = 3)`

The main function which can be used to predict age-at-death based on dental wear. The Middenbeemster data is built-in, so if pop = "MB11" is chosen, the data input is not required. If pop = "other", a data frame with a column for age and a second column for wear scores can be inserted. The function returns the input wear score, the age prediction, age range as +- years, and lower and upper intervals. The default method is a quadratic (rank = 2) model with 68%PIs, but "confidence" intervals and any level between 0 and 1 (0 - 100%), not inclusive, can be chosen. The BAMSAUR.mars function can be used to apply a MARS model for the age-at-death estimation. Only "prediction" intervals are supported for MARS models. The default number of cross validations (ncross) is 3 to reduce computation time.

Examples:

```r
#The following example calculates the ages-at-death with 68%PIs of 10 random wear scores
wear <- runif(10,0,16)
BAMSAUR(wear)
#Ages-at-death using a linear model and 95%CIs
BAMSAUR(wear, rank = 1, interval = "confidence", level = 0.95)

#Age-at-death calculation using MARS
BAMSAUR.mars(wear)
```

BAMSAUR best-fit function

`BAMSAUR.bff(data, interval = "prediction", level = 0.68, varmod.method = "earth", nfold = n-1, ncross = 3, ...)`

This function can be used to evaluate a dataset to determine its feasibility as a reference sample. The data input must contain one column with known ages-at-death (whether osteologically estimated or documented age) and a second column with the associated wear scores. The function returns a summary of the developed linear, quadratic, and cubic regression models, as well as the MARS model, including r-squared values, accuracies from the LOOCV, PRESS, AIC, BIC, average age range (to indicate precision), and GCV.

Examples:

```r
#the following expression evaluates the sample with 68% prediction intervals as age ranges
MBex <- BAMSAUR.bff(MBsimple)
#the following code accesses the quadratic plot
MBex$quad.plot
```

Leave-one-out cross validation

`BAMSAUR.LOOCV(object, data, interval = "prediction", level = 0.68)`
Leave-one-out cross validation function to calculate accuracies of the regression models. The calculated age ranges are incorporated into the LOOCV, and a case is considered accurate if the known age is contained within the age range for the predicted age. This function is incorporated into the BAMSAUR.bff function.

Examples:
```r
age <- MBsimple$age
wear <- MBsimple$wear
lin <- lm(age ~ wear)
BAMSAUR.LOOCV(lin)
```

BAMSAUR shiny app

`runBAM()`

This launches the BAMSAUR shiny app, which contains the same functionality as the individual functions.


Example:
```r
##Not run: runBAM()
```

# References

Milborrow, S. Derived from mda:mars by Trevor Hastie and Rob Tibshirani. Uses Alan Miller's Fortran utilities with Thomas Lumley's leaps wrapper. (2019). earth: Multivariate adaptive regression splines. R package version 4.7.0. https://CRAN.R-project.org/package=earth
publication

R Core Team (2018). R: A language and environment for statistical computing. R Foundation for 
Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Wickham, H. (2016). ggplot2: Elegant graphics for data analysis. New York: Springer-Verlag.
