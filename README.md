# Risk-Index
A demonstration on how the Risk Index was developed from 10 different behavioral outputs by condensing it into one output.

Goal:
The goal of this formula is to figure out the level of riskiness for each invoice. The main reason why we want to use several elements instead of basing off the riskiness by one element is because we want a balanced look 
 
There are 6 steps to consider and do if we want to build a complete formula
 
Step 1: Output
The output we that we desire is a number ranking from 20-80 for each invoice line. 
Its achieved by finding the sum of all of the input variables by with weights added to each variable
 
Step 2: What we feed into the formula
Dataset: Form 471 modified
13 variables to input
4 categorical variables
7 numerical variables
Used Quantile Regressions, Random Forest, Ratios, and differences to find the variables
Step 2.5: How the risks were predetermined
Explain the quantile regressions, etc
Step 2.7: Normalizing the variables together
What it does: It ensures that all the variables have the same scale/measurement when adding the results together
Ex. All variables would be scaled form 0 – 1
Why its needed
The variables all have difference max and min number which could skew the data to one side only because one variable is much higher than the rest
Ex. Total funding has 120,000,000 but unmet ratio has only 1.5 in the numbers
Step 3: Determining the priorities/weight we should give based on client's feedback
Ex. Total funding request should be top priority or 10, while nslp varaince is lowest priority or 1
 
Step 4:  Turning the priorities into weight
W = count of weights / sum(weights)
This will allow us to directly make a variable have higher significance/impact on the formula than other variables
What happens if we do not add in the weights: Some variables (A) that actually has more contribution than others will be drowned by other variables (B) if the numbers are higher than (A). This could lead to a misleading result that could not help us in the future.
 
Step 5: Ensuring the Data types are scaled
Something we need to take into account is the variance difference between Binary (Yes/No) and Numerical variables (1-1000)
Variance: Measure of how far a set of numbers/data is spread out from the average
Variance Difference
Binary has higher variance (0.23 for ex)
Continuous has lower variance (0.03 for ex)
What happens if we don't scale the variance
The higher the variance, the more impact it has on the result when multiplied with the weights
Ex. A weight of 4 in a binary variable (consultant Yes No) would still have a higher impact than a weight of 9 on continuous variable (ex. Total funding request) 
How we fixed it
Dividing each invoice line results with the standard deviation of the results. This can ensure that the variances are scaled the same with the other variables
 
Step 6: 
Multiply the outputs in each variable by the weights, then normalize the results to finally get the answer
W(v) + w(v)/ max(W(v))

![image](https://github.com/DanielKim15/Risk-Index/assets/59937761/15e70dbb-cf3c-4407-8906-efe3cbb19f8a)

