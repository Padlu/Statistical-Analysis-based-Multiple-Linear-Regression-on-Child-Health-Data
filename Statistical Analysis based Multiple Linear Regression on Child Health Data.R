setwd("/Users/abhishekpadalkar/Documents/IRELAND Admissions/NCI/Course Modules/Modules/Sem 1/Statistics/Project CA/WHO data/Child Health/")

# Get the data
child_data <- read.csv("Child_health.csv", header = T)
# new_data <- read.csv("New_filled_na.csv", header = T)
# install.packages("psych")

# Import the required libraries
library(psych)
library(leaps)
library(car)

# Check the data
str(child_data)
describe(child_data)

# Here, we have 5 independent variables, Infants_provided_minimum_accepted_diet_1 | Health_Service_Coverage | Early_initial_breastfeed | Underweight_children | Post_birth_contact_health_provider, and our dependent variable Mortality rate. All are float values in percent except for health service coverage which is int but accounts for percent only(check!).

# Check the matrix scatter plot of all the variables to get a vague understanding of what to expect and if our dependent variable has a linear relationship with our IVs. And correlation between all the variables.
pairs.panels(child_data[, 2:7],
             panel = panel.smooth,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE  # show density plots
)
# We see that there's a +ve correlation between underweight children % and mortality rate. Whereas, a slightly strong -ve correlation between infants with minimum diet feeded and mortality rate. There's also -ve low correlation between health service coverage, post birth contact with health provider and early initial breastfeed with mortality rate. It is seen that there's no significant collinearity between independent variables, except for a slightly higher -ve collinearity between underweight and infants provided with minimum diet. So we need to keep an eye on that.

# Now we move forward to check how individual varibles impact mortality rate with simple linear regression. Measurement is Rˆ2 value.

# Mortality rate with Health service coverage model
health_model <- lm(Mortality_Rate~Health_Service_Coverage, data = child_data)
summary(health_model)
# Rˆ2 => 0.08938 | p-value = 0.00785 < 0.01 **

# Mortality rate with Early initial breastfeed model
health_model <- lm(Mortality_Rate~Early_initial_breastfeed, data = child_data)
summary(health_model)
# Rˆ2 => 0.02433 | p-value = 0.17267 > 0.05

# Mortality rate with Post-birth contact with health provider model
health_model <- lm(Mortality_Rate~Post_birth_contact_health_provider, data = child_data)
summary(health_model)
# Rˆ2 => 0.05978 | p-value = 0.031 * < 0.05

# Mortality rate with Underweight children % model
health_model <- lm(Mortality_Rate~Underweight_children, data = child_data)
summary(health_model)
# Rˆ2 => 0.5705 | p-value = 1.352e-15 *** < 0.000

# Mortality rate with Infants provided with minimum accepted diet model
health_model <- lm(Mortality_Rate~Infants_provided_minimum_accepted_diet, data = child_data)
summary(health_model)
# Rˆ2 => 0.5718 | p-value = 1.198e-15 *** < 0.000

# We can see that individually, the first three variables are not that effective on the dependent variable in terms of variablity. And, the last two are significant as well as provided with a considerably good Rˆ2 value. Now to see which combinations of the variables make a better Adjusted Rˆ2 value to collectively impact on the variability on Mortality rate. For that we use Best subsets algorithm with forward step method.
Best_fit <- regsubsets(Mortality_Rate~Infants_provided_minimum_accepted_diet
                       +Health_Service_Coverage
                       +Early_initial_breastfeed
                       +Underweight_children
                       +Post_birth_contact_health_provider,
                       data = child_data, nbest=2)
par(mfrow=c(1,1))
subsets(Best_fit, statistic="adjr2")
plot(Best_fit, scale="adjr2")

# By interpreting the above best subsets, we see that combination of 4 subsets are contributing to the variability of Mortality rate equally or more than all the 5 variables together. The combination is I-H-E-U. So we select these variables, and drop Post_birth_contact_health_provider, to the next stage of our model development.
best_subset_model <- lm(Mortality_Rate~Infants_provided_minimum_accepted_diet
                        +Health_Service_Coverage
                        +Early_initial_breastfeed
                        +Underweight_children,
                        data = child_data)
summary(best_subset_model)
vif(best_subset_model)
# We check that there's no multicollinearity in any of our IVs here. Now, since we'll try to check interactions between our IVs to see if our model can get any better, it is bound to increase multicollinearity in the future. We'll rectify it by doing standardization then, but for now, we know there's no multi-collinearity in our actual IVs.

# Adjusted Rˆ2 => 0.7109, All the variables are significant except for Early initial breastfeed. But, it is close to 0.05. So we keep an eye on it for now, and check whether there are an interactions between any of these variables which can make our model predict better. And increase the adjusted Rˆ2. We do this to check if the relation is not just additive and more than that to make our model stronger. We also do this to check if any variable has variability over any other. Like lower health services may be the reason for children to be more underweight or ...(think of any other)
best_subset_interactiion_model <- lm(Mortality_Rate~Infants_provided_minimum_accepted_diet
                                     *Health_Service_Coverage
                                     *Early_initial_breastfeed
                                     *Underweight_children,
                                     data = child_data)

# With 4 IV, we will have total of 16 combinations of models to check, so to check that, we use step function which will give us Best possible model with lowest AIC and highest adjusted Rˆ2.

stepped_best_subset_interaction_model <- step(best_subset_interactiion_model)
summary(stepped_best_subset_interaction_model)
# Our Adjusted Rˆ2 is 0.7735 which jumped from 0.7109. But, now our model have total of 12 IVs. Of which many are insignificant. So we start dropping them one by one and check our Adjusted Rˆ2.
updated_stepped_1 <- update(stepped_best_subset_interaction_model, ~.-Infants_provided_minimum_accepted_diet:Health_Service_Coverage) # It's p-value is 0.150
summary(updated_stepped_1)
# Our Adjusted Rˆ2 value dropped from 0.7735 to 0.7697

updated_stepped_2 <- update(updated_stepped_1, ~.-Health_Service_Coverage:Early_initial_breastfeed) # Since It's p-value is 0.614 (highest in Interactive terms)
summary(updated_stepped_2)
# Our Adjusted Rˆ2 value went up from 0.7697 to 0.7722

updated_stepped_3 <- update(updated_stepped_2, ~.-Infants_provided_minimum_accepted_diet:Health_Service_Coverage:Early_initial_breastfeed) # Since It's p-value is 0.255 (highest in Interactive terms)
summary(updated_stepped_3)
# Our Adjusted Rˆ2 value went down from 0.7722 to 0.7712

updated_stepped_4 <- update(updated_stepped_3, ~.-Infants_provided_minimum_accepted_diet:Early_initial_breastfeed) # Since It's p-value is 0.277 (highest in Interactive terms)
summary(updated_stepped_4)
# Our Adjusted Rˆ2 value dropped from 0.7712 to 0.7705

updated_stepped_5 <- update(updated_stepped_4, ~.-Infants_provided_minimum_accepted_diet:Early_initial_breastfeed:Underweight_children) # Since It's p-value is 0.369 (highest in Interactive terms)
summary(updated_stepped_5)
# Our Adjusted Rˆ2 value went up from 0.7705 to 0.7711

updated_stepped_6 <- update(updated_stepped_5, ~.-Infants_provided_minimum_accepted_diet:Underweight_children) # Since It's p-value is 0.18 (highest in Interactive terms)
summary(updated_stepped_6)
# Our Adjusted Rˆ2 value dropped from 0.7711 to 0.7684. Which is 0.0027.
# Our starting model was at Adjusted Rˆ2 of 0.7722 with total of 12 variables which contributed very less to our model. We reduced the variables from 12 to 6 and the current Adjusted Rˆ2 is 0.7684. So the total reduction from 12 to 6 variables in our adjusted Rˆ2 value is 0.0038 i.e. 0.38%. This is not that significant of a drop. And our Adjusted Rˆ2 is floating around 77 throughout the 6 steps of removal, and we have 4 significant variables. Thus, it can be said that this is pretty far our model can go in improving on Rˆ2 value with interactions.

updated_stepped_7 <- update(updated_stepped_6, ~.-Early_initial_breastfeed:Underweight_children) # Highest p-value but significant. Just to check how far our Adjusted Rˆ2 value drops
summary(updated_stepped_7)
# We can see that now our model has affected strongly with drop from 0.7684 to 0.7502 i.e. 1.82%. So we include that interaction again and finalize the above model. 
# So we started from Adjusted Rˆ2 of 0.7109 with best_subsets, and now by including interacting terms our model's Adjusted Rˆ2 is 0.7684. The jump is of 5.75%.

# Checking from our initial scatterplot matrix, we saw that there is a chance of curvilinear effect of infants provided with minimum accepted diet with mortality rate and also underweight children with mortality rate. There is also a possibility of the same for health service coverage and early initial breastfeed. So we check for that by including polynomials.
stepped_final <- lm(Mortality_Rate~Infants_provided_minimum_accepted_diet
                    +Health_Service_Coverage
                    +Early_initial_breastfeed
                    +Underweight_children
                    +Health_Service_Coverage:Underweight_children
                    +Early_initial_breastfeed:Underweight_children,
                    data = child_data)
summary(stepped_final)
stepped_final_curvi <- lm(Mortality_Rate~Infants_provided_minimum_accepted_diet
                          +Health_Service_Coverage
                          +Early_initial_breastfeed
                          +Underweight_children
                          +Health_Service_Coverage:Underweight_children
                          +Early_initial_breastfeed:Underweight_children
                          +I(Infants_provided_minimum_accepted_diet^2)
                          +I(Health_Service_Coverage^2)
                          +I(Early_initial_breastfeed^2)
                          +I(Underweight_children^2),
                          data = child_data)
summary(stepped_final_curvi)
# Wow, as it is seen, our adjusted Rˆ2 went up from 0.7684 to 0.8284. It is seen that Early initial breastfeeding which was not significant earlier, now by including a polynomial of it, both the terms have become significant with significance <0.01. We can see that, Health Service coverage ^2 is not contributing to the curvi nature as it's p-value is 0.820489, pretty high. So we drop that variable.
stepped_final_curvi_1 <- update(stepped_final_curvi, ~.-I(Health_Service_Coverage^2)) # Since it's p-value is very high of 0.820489
summary(stepped_final_curvi_1)
# Amazing, by dropping the insignificant Health Service coverage ^2, the model's Adjusted Rˆ2 went up to 0.8308 from 0.8284
# We can see that all the variables are contributing to the variability of the dependent variable Mortality rate. So now we cannot go further beyond this stage, and the best fit with highest adjusted Rˆ2 value our model can have is 0.8308.
# Now, our total variables is 9 from our initial 4 variables. 

# Now we check if our assumptions of OLS are met or not. If not we try to rectify them.
final_model <- stepped_final_curvi_1
summary(final_model)
par(mfrow=c(2,2))
plot(final_model)

# Diagnostics:
# Linearity :: From the Residual vs Fitted plot, it can be seen that our model is following a linear random spread over the residuals. Thus, that is checked!
# Homoscedasticity :: From the Scale-Location chart, we can see that there is a slight problem of heteroscedasticity in our model. Let's try to rectify that using sqrt on our dependent variable.
final_model <- lm(sqrt(Mortality_Rate)~Infants_provided_minimum_accepted_diet
                  +Health_Service_Coverage
                  +Early_initial_breastfeed
                  +Underweight_children
                  +Health_Service_Coverage:Underweight_children
                  +Early_initial_breastfeed:Underweight_children
                  +I(Infants_provided_minimum_accepted_diet^2)
                  +I(Early_initial_breastfeed^2)
                  +I(Underweight_children^2),
                  data = child_data)
summary(final_model)
plot(final_model)
ncvTest(final_model)
# Here, we can see that we've solved the problem of heteroscedasticity, as the plot shows the spread and random nature of residuals and p-value of ncvTest is 0.83441 i.e >0.05. So, not heteroscedasticity present. But by taking the sqrt value has affected the adjusted Rˆ2 value of our model to fall. It has dropped to 0.8062 from 0.8308 i.e. 2.46%.
# We check whether there is no autocorrelation between our residual errors. We do it by performing durbin-watson test.
durbinWatsonTest(final_model)
# We can see that our value is 2.0319 which is closer to 2 which indicates that there's no autocorrelation in our errors.
# Now we check if the errors are normally distributed or not. We can check that from the Normal Q-Q plot from our diagnostic model, it is seen that our residuals are following a normal distribution.
# Now we check for multicollinearity by checking the VIF value for each variable.
vif(final_model)
# This was bound to happen as it was discussed earlier. We rectify it by standardizing all our IVs which will in-turn have a same effect and our coefficients will also not be affected. We do it by subtracting values of variables by their respective means. As follows:
# https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/
# https://statisticsbyjim.com/regression/standardize-variables-regression/
# After trying standardizing, it seems that it's not possible to decrease the VIF.

final_model <- update(final_model, ~.-I(Early_initial_breastfeed^2)) # since p-value 0.122446
summary(final_model) # Adjusted Rˆ2 dropped from 0.8062 to 0.8021
plot(final_model)
vif(final_model)

final_model <- update(final_model, ~.-Early_initial_breastfeed:Underweight_children) # since p-value 0.301217
summary(final_model) # Adjusted Rˆ2 dropped from 0.8021 to 0.8019
plot(final_model)
vif(final_model)

final_model <- update(final_model, ~.-Health_Service_Coverage:Underweight_children) # since p-value 0.200820
summary(final_model) # Adjusted Rˆ2 dropped from 0.8019 to 0.80
plot(final_model)
vif(final_model)

final_model <- update(final_model, ~.-I(Infants_provided_minimum_accepted_diet^2)) # vif-value 14.309330
summary(final_model) # Adjusted Rˆ2 dropped from 0.80 to 0.7824
plot(final_model)
vif(final_model)

final_model <- update(final_model, ~.-I(Underweight_children^2)) # vif-value 8.825227
summary(final_model) # Adjusted Rˆ2 dropped from 0.7824 to 0.7414
plot(final_model)
vif(final_model)
# Here, we can see that for our model to be generalized and thus our IVs for not to collinear with each other, we had to reduce our IVs back to initial 4 variables. We can see that Early_initial_breastfeed is not that significant now. So we drop that variable as of Parsimony's Rule of significance.

final_model <- update(final_model, ~.-Early_initial_breastfeed) # Since p=value 0.21279
summary(final_model) # Adjusted Rˆ2 dropped from 0.7414 to 0.7394
plot(final_model)
vif(final_model)  # All values are below 5 => No multicollinearity problem between variables.
ncvTest(final_model)  # p-value = 0.07379 > 0.05 => No problem of heteroscedasticity
durbinWatsonTest(final_model) # DW statistic = 2.288 => closer to 2, thus no Autocorrelation between residuals.
# Our model still holds normal distribution of residuals and linearity in residual plot.
# Now, lastly we check for any influential datapoint which can throw off our linear model, i.e. has leverage upon our model.
# We look at cooks distance plot, i.e. Residuals vs Leverage. We can see that no datapoint is actually influential or an potential outlier as cook's distance for all the points is less that 0.2, which is far from 0.5 and 1. Thus, no outliers.

# This is our final model which is generalised for any data. But, as seen above, for this particular data, we can make it more personalised to this data by having our multi-collinear model.
