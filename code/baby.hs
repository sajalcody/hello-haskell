doubleMe x  = x + x

doubleSmallNum x = if x < 100
    then 2 * x
    else x

doubleAndAdd x = doubleSmallNum x + 1