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


setwd("~/FRAN/Teaching/QM Research School/Datasets")
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM Research School/Datasets") # Mac


paisley_data <- read_excel("paisley_data.xls")

setwd("~/FRAN/Teaching/QM Research School/Session")
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM Research School/Session") # Mac



#################### Dealing with uncertainty ###################

# What we observe is influenced by chance
# What would have happened if we would have observed a different set of records (sample)

# We draw conclusion about the characteristics of the population
  # based on a sample of observations
  # this sample statistic however will deviate from the true value
  # --> sampling error, which depends on:
    # the variability within the population (st. dev.)
    # the number of observations in the sample
  # --> we use the standard error to compute:
    # confidence intervals
      # it will cover the true value with certain probability
    # testing hypothesis:
      # assume a hypothesis to be true
      # assess the probability (p-value) of what is observed
        # based on the previous assumption
          # the probability that the observed outcome would
            # have happened by chance

# Some preparations
paisley_data$feet[paisley_data$feet == 50] <- 5

paisley_data <- mutate(paisley_data, 
                       height = 30.48*feet+2.54*inches)

paisley_data <- mutate(paisley_data, 
                       weight_kg = 0.453592*weight)


########## CONFIDENCE INTERVALS (MEANS) ########## 
library(dplyr)

options(digits = 4)

ci <- paisley_data %>%
  dplyr::group_by(countryb) %>%
  dplyr::summarise(
    N = sum(!is.na(height)), 
    mean = mean(height, na.rm = TRUE),
    lb_ci = t.test(height, conf.level = 0.95)$conf.int[1],      # CI lower bound
    ub_ci = t.test(height, conf.level = 0.95)$conf.int[2]) %>%  # CI upper bound
    dplyr::mutate_if(is.numeric, format, 1) # add 1 decimal place
ci

ci %>%  
  ggplot() +
  geom_point(aes(x = countryb, y = mean)) +
  geom_errorbar(aes(x=countryb, ymin=lb_ci, ymax= ub_ci), width = 0.4)



paisley_data %>%  
  filter(age >= 18) %>%
  ggplot(aes(x = year, y = weight_kg, group=sex)) +
  geom_point(mapping = aes(color=sex)) +
  geom_smooth(mapping = aes(color=sex), se = TRUE)


#### HYPOTHESIS TESTING ##########
summary(paisley_data$weight_kg)
  # mean = 61

# One-sample test
t.test(paisley_data$weight_kg, y = NULL, 
       mu = 63,  
       conf.level = 0.95)

# Two-sample test: comparing groups
t.test(weight_kg ~ sex, data = paisley_data,
       conf.level = 0.95)


######## CORRELATION ANALYSIS #############

paisley_data %>%  
  filter(age >= 20) %>%
  group_by(sex) %>%
  summarize(cor=cor(age, weight_kg, method = "pearson", use = "complete.obs"),
            cor_ci_lb = cor.test(age, weight_kg)$conf.int[1],
            cor_ci_up = cor.test(age, weight_kg)$conf.int[2]
            ) 
  # -0.106 for males / -0.338 for females

install.packages("metan")
library(metan)
library(dplyr)

paisley_data %>%  
  filter(age >= 20) %>%
  group_by(sex) %>%
  corr_ci(age, weight_kg) %>%
  plot_ci()


#### REGRESSION ANALYSIS

reg <- paisley_data %>%
  filter(age >= 20) %>%
  lm(weight_kg ~ age+ countryb + sex + height, data = .)
summary(reg)

# Plotting the effects
install.packages("sjPlot")
library(sjPlot)

plot_model(reg, ci.lvl = NA) 
  # to plot the CIs, remove "ci.lvl = NA"

# Plotting the predicted effect of one variable 
plot_model(reg, type = "pred", terms = "age")
plot_model(reg, type = "pred", terms = "sex")
plot_model(reg, type = "pred", terms = "countryb")

### Time as a explanatory variable

# Create categories for each year (instead of a continuous variable)
paisley_data <- paisley_data %>%
  mutate(d_year = factor(year))

reg2 <- paisley_data %>%
  filter(age >= 20) %>%
  lm(weight_kg ~ age + sex + d_year, data = .)
summary(reg2)

plot_model(reg2, terms = "d_year [c(1841:1870)]") 
plot_model(reg2, type = "pred", terms = "d_year")
?plot_model

# or smooting the underlying trend
plot_model(reg2, type = "pred", terms = "d_year", ci.lvl = NA) +
  geom_smooth(se = TRUE)




