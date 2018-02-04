library(pwr)

# For a one-way ANOVA comparing 5 groups, calculate the
# sample size needed in each group to obtain a power of
# 0.80, when the effect size is moderate (0.25) and a
# significance level of 0.05 is employed.

pwr.anova.test(k=5,f=.25,sig.level=.05,power=.8)
pwr.anova.test(k=6,f=.25,sig.level=.05,power=.8)

# What is the power of a one-tailed t-test, with a
# significance level of 0.01, 25 people in each group, 
# and an effect size equal to 0.75?

pwr.t.test(n=25,d=0.75,sig.level=.01,alternative="greater")

# Using a two-tailed test proportions, and assuming a
# significance level of 0.01 and a common sample size of 
# 30 for each proportion, what effect size can be detected 
# with a power of .75? 

pwr.2p.test(n=30,sig.level=0.01,power=0.75)