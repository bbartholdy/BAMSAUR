#The following example calculates the ages-at-death with 90%PIs of 10 random wear scores
wear <- runif(10,0,16)
BAMSAUR.mars(wear, level = 0.90)
