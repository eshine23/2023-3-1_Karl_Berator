
# this code calculates the sample size required for the experiment outlined in the 
# Readme using the method outlined in Cohen 1988. 

pacman::p_load(pwr)


# theory: chapter 9 
# https://www.utstat.toronto.edu/~brunner/oldclass/378f16/readings/CohenPower.pdf#%5B%7B%22num%22%3A1747%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22Fit%22%7D%5D


# multiple linear reg with control pred (not used but kept for reference)
# https://stats.oarc.ucla.edu/other/gpower/multiple-regression-power-analysis/



# calculate v (second degree of freedom for f dist)
# u =5 as has 5 predictors 
# R = 0.1 (given)
# effect (f2) = R/(R-1)
# sig level = 0.05 (given)
# power = 0.9 (given)


v <- pwr.f2.test(u=5, f2=(0.1)/(1-0.1), sig.level=0.05, power=0.90)

# calculate N 
# using v = (N-u-1)
N <- ceiling(v$v +5 +1)



