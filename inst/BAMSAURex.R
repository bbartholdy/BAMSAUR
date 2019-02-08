#The following example calculates the ages-at-death with 90%PIs of 10 random wear scores
wear <- runif(10,0,16)
BAMSAUR(wear, interval = "prediction", level = 0.90)

#Age-at-death calculation using MARS
BAMSAUR(wear, model = "mars", interval = "prediciton", level = 0.90)
