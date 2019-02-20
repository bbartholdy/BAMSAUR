# BAMSAUR

This package provides functions for the age-at-death estimation of nonadult human skeletal remains based on dental wear. The method was developed using a Dutch post-Medieval reference population; however, the package also allows the inclusion of other (more appropriate) reference samples. The main function allows the age-at-death estimation of non-adults based on a single wear score calculated from all available teeth, and additional functions allow the user to upload and evaluate their own reference sample, from which to base the age-at-death estimations.
Users who are not familiar with the R-environment can also take advantage of a user-friendly graphic user interface (GUI).

# Installation
1. To install BAMSAUR, R must first be installed (https://cran.r-project.org/).

2. Open R, and install the package "devtools" by entering the following into the R-console.
install.packages("devtools")

3. The BAMSAUR package can then be downloaded and installed:
devtools::install_github("bbartholdy/BAMSAUR")
library(BAMSAUR)

# Usage

The method was developed on Middenbeemster, a 19th century Dutch skeletal collection containing individuals with documented age-at-death. The method uses a 9-stage dental wear scoring system, applicable to the entire deciduous and permanent (excluding third molars) dentitions. The average wear scores were used to develop a quadratic regression model from which to base age-at-death predictions. Age ranges are calculated using 68% prediction intervals. Users can also elect to use linear and cubic regression, or multivariate adaptive regression splines (MARS) for age-at-death predictions. Additional functions can be used to evaluate potential reference samples by developing linear, quadratic, and cubic regression models, as well as a MARS model, and provides information on the developed models such as r-squared values, accuracies from leave-one-out cross validations (LOOCV), predictive residual error sum of squares (PRESS), and precision (based on the average size of predicted age ranges).

# Functions

BAMSAUR

BAMSAUR.bff

BAMSAUR.LOOCV

runBAM
This launches the GUI.
