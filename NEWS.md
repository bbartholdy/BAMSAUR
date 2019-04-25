# NEWS

Updates to the BAMSAUR package

## BAMSAUR_0.0.0.9001
### 11-04-2019

* Erroneous code corrected in the BAMcv.lm function, which was providing incorrect accuracy rates.

## BAMSAUR_0.0.0.9000
### 11-03-2019

* Added option to apply MARS-sized prediction intervals to the simple regression models. This is recommeded when using the MB11 population, as the age range provided by the MARS model can better account for the error of the added undocumented-age individuals.

* Leave-one-out cross validation now performed on MARS models, allowing PRESS to be calculated.

* General housekeeping.
