# Install packages (if needed)
install.packages("tidyverse")
install.packages("xlsx")
install.packages("writexl")



# Start afresh
rm(list=ls()) # Clear de "Global Environment"

# Load the packages you need
library(tidyverse) 
library(readxl)
library(writexl)



# Working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM Research School/Session") # Mac
setwd("W:/Documents/FRAN/Teaching/QM Research School/Session")                # Windows
  # RStudio menu helps finding the path: 
  #   Session/Set working directory/Choose directory
  #   Copy and paste the chosen path from the console to the script


# Import data
paisley_data <- read_excel("/Volumes/francijb/Documents/FRAN/Teaching/QM Research School/Datasets/paisley_data.xls")
paisley_data <- read_excel("W:/Documents/FRAN/Teaching/QM Research School/Datasets/paisley_data.xls")
    # if the data is in the working directory, you don't need the path (just the file name)

paisley_data <- read_rds("paisley_v2.rds")
  # the one with the changes already made

######### REGRESSION ANALYSIS ############

# Let's explore the relationship between age and weight (for adults)
paisley_data %>%  
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg)) + 
  geom_point()

# Correlation analysis assesses the direction and strength of the relationship 
  # within a scale between 0-1 (whether two variables move together)
paisley_data %>%  
  filter(age >= 20) %>%
  summarize(cor = cor(age, weight_kg, method = "pearson", use = "complete.obs")) 
  # -0.185: a weak (negative) relationship

# Regression analysis goes beyond and allows:
  # (1) assessing the actual impact of X on Y: coefficient b
  # (2) computing how much of the variation in Y is explained by X (or Xs): R-squared
  # (3) allows controlling directly for the effect of other variables

paisley_data %>%  
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
    # Regression analysis basically finds the best line to fit the data (OLS: Ordinary Least Squares)
    # (the line minimising the sum of the deviations between the observed and predicted values)
    #   (the deviations are squared in order to compare negative and positive deviations)
    # "lm" refers to "linear model"
    # y is the variable we want to "explain" (weight; dependent variable)
    # x is the explanatory variable (age)
    # se refers to standard errors (the confidence intervals) - let's not report then for now

### Estimate the regression line (intercept/slope)  
  # R (or any other statistical software) does the job for you
library(modelr)

reg1 <- lm(weight_kg ~ age, data = filter(paisley_data, age>=20))
  # where age is the explanatory variable (x) and weight_kg the dependent variable (y)
# or with pipe
reg1 <- paisley_data %>%
  filter(age>=20) %>%
  lm(weight_kg ~ age, data = .)

  # check the results
coef(reg1)
reg1
  # interpret the results:
    # y = a + b*x
    # weight = 66.8 - 0.13*age
    # intercept: if x = 0 -> weight = 66.8
    # slope: one-unit increase in X (age: 1 year) reduces Y (weight) by 0.13 units (kgs.)
    # (always think about of units of measurement both in X and Y)

  # the model is basically predicting what Y will be depending on X
  # add the predicted values to the dataset (another column/variable)    
paisley_data <- paisley_data %>% 
  add_predictions(reg1, var = "pred1")  

  # check
view(paisley_data)

paisley_data %>% 
  filter(!is.na(weight)) %>%
  subset(select=c(age, weight, pred1)) %>%
  print(n = 25) 

# Plot the predicted values
paisley_data %>%  
  filter(age >= 20) %>%
  ggplot(aes(age)) +
  geom_point(aes(y = weight_kg)) +
  geom_line(aes(y = pred1), colour = "red", size = 1)
  # basically the same as we plot above with ggplot & geom_smooth

#### R-squared
  # Fraction of the variation on Y that is explained by the model 
  # It ranges between 0 and 1
  # Low R2 does not neccessarily mean that the model is useless
  # It has to be interpreted based on expectations
  # The coefficient "b" can still provide useful info about the effect of X on Y

# Report the R-squared:
summary(reg1)$r.squared
  # R2 = 0.03 -> we (age) explains 3% of the variation in adult weight

# Let's look at those prisoners below age 20

  # Visually
paisley_data %>%  
  filter(age < 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg)) + 
  geom_point()

  # Compute the regression line
reg2 <- paisley_data %>%
  filter(age < 20) %>%
  lm(weight_kg ~ age, data = .)

reg2
  # How do you interpret these figures??

  # Plot the predicted values
paisley_data <- paisley_data %>% 
  add_predictions(reg2, var = "pred2")  

paisley_data %>%  
  filter(age < 20) %>%
  ggplot(aes(age)) +
  geom_point(aes(y = weight_kg)) +
  geom_line(aes(y = pred2), colour = "red", size = 1)

  # R-squared:
summary(reg2)$r.squared
  # R2 = 0.69 -> interpretation?


# The difference between the predicted and the observed values are the residuals
  # Sometimes positive, sometimes negative (the average should be around 0)

  # add the residuals to the dataset (another column/variable)    
paisley_data <- paisley_data %>% 
  add_residuals(reg2, var = "resid2")  

  # check: observed values - predicted values - residuals
paisley_data %>% 
  filter(age < 20 & !is.na(weight)) %>%
  subset(select=c(age, weight, pred2, resid2)) %>%
  print(n = 25) 

# Plot the residuals:
  
  # Distribution of the residuals
paisley_data %>%  
  filter(age < 20) %>%
  ggplot(aes(resid2)) + 
  geom_histogram(binwidth = 1)

  # residuals vs X (in a good model, residuals should revolve randomly around 0)
paisley_data %>%  
  filter(age < 20) %>%
  ggplot(aes(age)) +
  geom_point(aes(y = resid2)) +
  geom_ref_line(h = 0)


# A more refined analysis: does the previous relationship change by sex? 
# Males
reg_males <- paisley_data %>%
  filter(age>=20 & sex=="male") %>%
  lm(weight_kg ~ age, data = .)

coef(reg_males)
reg_males                       # height = 67.2 - 0.07*age
summary(reg_males)$r.squared    # R2 = 0.011

# Females
reg_females <- paisley_data %>%
  filter(age>=20 & sex=="female") %>%
  lm(weight_kg ~ age, data = .)

coef(reg_females)
reg_females                     # height = 63.3 - 0.18*age
summary(reg_females)$r.squared  # R2 = 0.114

# Visually
paisley_data %>%  
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg, color = sex)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

### Qualitative variables 
  # categorical (or ordinal) variable can be used in regression analysis as dummy variables (0/1)
  # 1 category acts as a "reference category"
      # so the coefficient is interpreted "against" that category
  # if you have n categories, you need (n-1) dummies
      # i.e. sex has two categories: male/female, so you just need 1 dummy
          # either being male or being female 
  # no need to create dummy variables to include categorical info (R does it for you)

reg3 <- paisley_data %>%
  filter(age >= 20) %>%
  lm(weight_kg ~ sex, data = .)

reg3 # Y = 57.08  + 7.83 X -- Interpretation?? (think about units of measurement)

# How would you run a regression on the link between country of birth and weight??


##### Multiple explanatory variables
  # We are interested in them (you want to better understand Y)
  # We want to make sure that our result is not driven by other variables
view(paisley_data)

  # Implementation and interpretation is basically the same

# e.g. how age, sex and literacy affect weight?
reg4 <- paisley_data %>%
  filter(age >= 20) %>%
  lm(weight_kg ~ age + sex + lit_adj, data = .)
    # notice that "+" is need for each additional explanatory variable 

reg4
  # interpreting the coefficients??
    # age: -0.10
    # male: 7.2
    # read: 0.8
    # write 4.3

summary(reg4)$r.squared   
  # R2 = 0.227 (notice that it was 0.03 when only age was used as X)
  
### What happens to the model when we add more explanatory variables??
  # (1) it usually increases the explanatory power: R-squared (as above)
  # (2) the effect of the other variables may change (see on this below)
  # (3) the estimations may become noisier (more on this tomorrow)

### Let's focus on (2): How (and why) the coefficient may change when introducing other variables

# What happens when we add "height" to the previous model? 
  # height can proxy for biological factors (taller people are heavier)
  # but also for socio-economic factors (richer people (eat better/work less) are taller)
reg5 <- paisley_data %>%
  filter(age >= 20) %>%
  lm(weight_kg ~ age + sex + lit_adj + height, data = .)
reg5
  # before:       now:
  #   age: -0.10    -0.08  
  #   male: 7.2     -0.58 
  #   read: 0.8     -0.63 
  #   write 4.3      0.89
  #   height         0.66          

  # If the omitted variable (let's call it Z) is correlated with both X and Y
  # the X was capturing the effect of X on Y and partly the effect of Z on Y
  # thus it is biased (different from the true value)
  # either upwards/downwards depending on the direction of the associations involved

# males are taller (and therefore heavier):
  # when height is not in the model, the coefficient "male" captures both 
    # the effect of being male and being taller

# richer people are more literate and better fed (taller and heavier)  
  # when height is not in the model, the coefficients on "read" and "write" capture both
    # the effect of literacy and of being taller (probably because richer and better fed)


### Choosing which variable introduce in the analysis should be theoretically justified


### Plotting the effects: 
  # controlling for the other ones (holding them constant)
install.packages("sjPlot")
library(sjPlot)

# Plotting the (standardised) effects
  # the effect is measured in standard deviations 
  # (to compare variables that are measured differently)
  # allows assesing which variable has a larger effect
plot_model(reg5, ci.lvl = NA)

# Plotting the predicted effect of one variable 
plot_model(reg5, type = "pred", terms = "age", ci.lvl = NA)
plot_model(reg5, type = "pred", terms = "lit_adj", ci.lvl = NA)



### Time as a explanatory variable
  # Given the "linear" nature of regression analysis, it will be treated as a trend
reg6 <- paisley_data %>%
  filter(age >= 20) %>%
  lm(weight_kg ~ age + sex + year, data = .)
reg6

plot_model(reg6, type = "pred", terms = "year", ci.lvl = NA)
  # This can be useful but it misses the complexity of changes over time

# Use dummy variables to estimate the effect of each year (or period)
  # convert first year into a factor variable, so each year can be treated as separate categories
paisley_data <- paisley_data %>%
  mutate(d_year = factor(year))
  # check
levels(paisley_data$d_year)

  # run the regression
reg7 <- paisley_data %>%
  filter(age >= 20) %>%
  lm(weight_kg ~ age + sex + d_year, data = .)
reg7
  # interpret the coefficients?? (reference category??)

  # visualise the effects
plot_model(reg7, type = "pred", terms = "d_year", ci.lvl = NA)

# or smooting the underlying trend
plot_model(reg7, type = "pred", terms = "d_year", ci.lvl = NA) +
  geom_smooth(se = FALSE)

# If individual years provide very noisy estimations,
  # group them into periods (i.e. 5-year period)
  # and convert them into dummies as done above


### Dummy variables as dependent variables (Logit models)

# Express the Y first as a dummy variable: 0/1
paisley_data$lit_adj
paisley_data <- paisley_data %>% 
  mutate(write = ifelse(str_detect(lit_adj, "write"), 1, 0))
paisley_data$write

logit1 <- paisley_data %>%
  filter(age >= 20) %>%
  glm(write ~ age + sex, data = ., family = "binomial")
logit1 
  # interpreting the coefficients is not straightforward
    # because it is not a linear model

# Better to predict them directly
plot_model(logit1, type = "pred", terms = "age", ci.lvl = NA)
plot_model(logit1, type = "pred", terms = "sex", ci.lvl = NA)

### Challenges to regression analysis
  # Non-linear relationships (functional form)
  # Parameter stability
  # Outliers

### Correlation is not causation
# Role of omitted variables
# Reverse causality

### Other issues
# Number of observations (noise)
# Categories employed
# Garbage in, garbage out (the results are as good as the data itself)




