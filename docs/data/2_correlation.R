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



#################### CORRELATION (BI-VARIATE STATISTICS) ###################

#### Numerical variables ####

# Describing the data first: age, weight
paisley_data %>% 
  summarize(count_age = sum(!is.na(age)),   
            mean_age = mean(age, na.rm = TRUE),
            count_weight = sum(!is.na(weight)),   
            mean_weight = mean(weight, na.rm = TRUE)
  )

# Let's express weight in kgs (instead of pounds)
paisley_data <- mutate(paisley_data, 
                       weight_kg = 0.453592*weight)
paisley_data %>% 
  summarize(mean_kgs = mean(weight_kg, na.rm = TRUE),
            min_kgs = min(weight, na.rm = TRUE),
            max_kgs = max(weight, na.rm = TRUE),
  )

### Visually
  # Scatterplot: Plotting the relationship between two variables
paisley_data %>%  
  ggplot() +
  geom_point(mapping = aes(x = age, y = weight_kg)) 

  # focusing only on those younger thann 20
paisley_data %>%  
  filter(age <= 19) %>%
  ggplot() + 
    geom_point(mapping = aes(x = age, y = weight_kg)) 

  ## edit how the xaxis looks like
paisley_data %>%  
  filter(age <= 19) %>%
  ggplot() + 
    geom_point(mapping = aes(x = age, y = weight_kg)) + 
    scale_x_continuous(limits = c(9,19), breaks = seq(9,19, by = 1)) +
    scale_y_continuous(limits = c(20,80), breaks = seq(20,80, by = 10))
      # sscale_x_discrete when the variable is discrete

  # focusing now on those aged 20 and older
paisley_data %>%  
  filter(age >= 20) %>%
  ggplot() + 
  geom_point(mapping = aes(x = age, y = weight_kg)) 


#### Evolution over time (history!!)

# i.e. how heights were changing over time (for males and females)
height_year <- paisley_data %>%  
  filter(age >= 18) %>%
  group_by(sex, year) %>%
  summarise(count = sum(!is.na(height)),   
            mean = mean(height, na.rm = TRUE) 
  )

view(height_year)

# Graph
height_year %>%  
  ggplot(aes(x = year, y = mean, group=sex)) +
  geom_line(mapping = aes(color=sex))

# Smoothing the year-to-year variation (reduce the noise)
ggplot(data = height_year, aes(x = year, y = mean, group=sex)) +
  geom_line(mapping = aes(color=sex)) +
  geom_smooth(mapping = aes(color=sex), se = FALSE)
    # no need to report standard errors yet (se)



#### Qualitative variables ####

# Sex & employment
paisley_data %>% 
  filter(!is.na(employed)) %>%
  group_by(sex, employed) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

# Literacy & employment
paisley_data %>% 
  filter(!is.na(employed)) %>%
  group_by(lit_adj, employed) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

### Visually - bar graph
paisley_data %>%  
  filter(!is.na(employed)) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = lit_adj, y = stat(prop), group = 1)) +
  facet_wrap(~ employed, nrow = 2) +
  coord_flip()



# Correlation coefficient: 
  # it measures the direction and the strenght of the association between two variables
  # it ranges between -1 to 1 being:
      #  1 - perfect positive correlation
      #     (strong - moderate - weak)
      #  0 - no correlation
      #     (strong - moderate - weak)
      # -1 - perfect negative correlation
  
  # it can be computed for numerical, ordinal and qualitative variables 
    # but employing different methods

# Pearson's (numerical data)
  # age & weight (both numerical)
paisley_data %>%  
  filter(age >= 20) %>%
  summarize(cor = cor(age, weight_kg, method = "pearson", use = "complete.obs")) 
  # -0.185
    # "use" indicates how to handle missing values
    # other options: "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"

paisley_data %>%  
  filter(age >= 20) %>%
  group_by(sex) %>%
  summarize(cor=cor(age, weight_kg, method = "pearson", use = "complete.obs")) 
  # -0.106 for males / -0.338 for females
  # the negative link between age and weight is much stronger in women
  
  # visually:
paisley_data %>%  
  filter(age >= 20) %>%
  ggplot() + 
  geom_point(mapping = aes(x = age, y = weight_kg)) +
  facet_wrap(~ sex, nrow = 2)

# Spearman's (rank/ordinal data)
  # lit_adj & employed (both ranked variables; although "employed" has not been "ranked" yet)
# Literacy & employment
paisley_data %>% 
  filter(!is.na(employed)) %>%
  group_by(lit_adj, employed) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

paisley_data$lit_adj <- factor(paisley_data$lit_adj, 
                                levels = c("illiterate", "read", "write"), 
                                ordered = TRUE)

paisley_data$employed <- factor(paisley_data$employed, 
                               levels = c("unemployed", "employed"), 
                               ordered = TRUE)
  # check
paisley_data$lit_adj
paisley_data$employed
  # the variables need to be numeric though
  # even though the distance between those numbers is not known

  # transform factor into a number
paisley_data <- paisley_data %>% 
  mutate(employed_num = as.numeric(paisley_data$employed))

  # check
paisley_data %>% 
  subset(select=c(employed, employed_num)) %>%
  print(n = 25) # display 25 rows

paisley_data <- paisley_data %>% 
  mutate(lit_adj_num = as.numeric(paisley_data$lit_adj))

paisley_data %>% 
  subset(select=c(lit_adj, lit_adj_num)) %>%
  print(n = 25) 

### Spearman correlation coefficient
paisley_data %>%  
  filter(age >= 20) %>%
  summarize(cor=cor(lit_adj_num, employed_num, method = "spearman", use = "complete.obs")) 
  # Correlation coef. = 0.07


### Cramer's V (categorical data): 
  # association between sex and having "marks"   
  
  # first we need to identify those with no marks
paisley_data %>% 
  group_by(marks) %>%
  summarize(n = n()) %>% 
  print(n = Inf) # display all rows

paisley_data <- paisley_data %>% 
  mutate(marks2 = 1)      # new variable called "marks2": all values set to 1
view(paisley_data)

paisley_data$marks2[str_detect(paisley_data$marks, "no mark")] <- 0
  # note that this identifies those "no mark" and "no marks"
paisley_data$marks2[str_detect(paisley_data$marks, "nomark")] <- 0
paisley_data$marks2[str_detect(paisley_data$marks, "none")] <- 0
paisley_data$marks2[is.na(paisley_data$marks)] <- NA
  # this last one is very important
  # otherwise, missing values (NA) are assumed to be 0 (equalling those with marks)

  # check:
paisley_data %>% 
  subset(select=c(marks, marks2)) %>%
  print(n = 200) 

# Is there a link between sex and having marks?
  # have a sense of the data first
paisley_data %>% 
  filter(!is.na(marks2)) %>%
  group_by(sex, marks2) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))


### Cramer's V
install.packages("lsr")
library(lsr)

cramersV(paisley_data$sex, paisley_data$marks2)
  # 0.11

# Another example (how to do it by sex????????????????????????????????????????):
cramersV(paisley_data$sex, paisley_data$employed) 
  # 0.19



                                                                        
### Visualising qualitative info (dummy variables)

# Creating dummy variables (0/1)
  # Any qualitative dimension can be expressed as 0/1

# i.e. being male:
table(paisley_data$sex) 
paisley_data$male <- ifelse(paisley_data$sex == "male", 1, 0)
  # it creates a new variable ("male") and
  # assigns 1 to those that meet the condition (sex=="male") and 0 otherwise
  # importantly, ifelse don't affect the missing values (NA)
  # (not important in this case, because all the prisoners have info on sex)
  
    # check the results
paisley_data %>% 
  subset(select=c(forename, sex, male)) %>%
  print(n = 50) 

paisley_data %>% 
  group_by(male) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

# i.e. being able to read and write
paisley_data %>% 
  group_by(lit_adj) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

paisley_data$write <- ifelse(paisley_data$lit_adj == "write", 1, 0)
  # creates a new variable ("write") with values 1/0
  # check
paisley_data %>% 
  group_by(write) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))



### Dummy variables can easily be expressed as fractions or probabilities
# The mean (average) is actually the fraction of the sample fulfilling that condition

# i.e. probability of being literate by age groups
lit_by_age <- paisley_data %>%   # note that we are now creating an object (not just reporting)
  group_by(age) %>%
  summarize(count = sum(!is.na(write)), 
            fr_write = mean(write, na.rm = TRUE)
  )
view(lit_by_age)

  # Visually
lit_by_age %>%
  filter(!is.na(age)) %>%
  ggplot(mapping = aes(x = age, y = fr_write)) +
  geom_col()

  # Perhaps better to group age in larger bins
summary(paisley_data$age, na.rm = T)

paisley_data <- paisley_data %>% 
  mutate(age_class = cut(age, breaks = c(9, 20, 30, 40, 50, 60, Inf)))

lit_by_age2 <- paisley_data %>%   
  group_by(age_class) %>%
  summarize(count = sum(!is.na(write)), 
            fr_write = mean(write, na.rm = TRUE)
  )
view(lit_by_age2)

# Visually
lit_by_age2 %>%
  filter(!is.na(age_class)) %>%
  ggplot() +
  geom_col(mapping = aes(x = age_class, y = fr_write))

# Evolution over time (history!)
view(paisley_data)
lit_by_year <- paisley_data %>%   
  group_by(year) %>%
  summarize(count = sum(!is.na(write)), 
            fr_write = mean(write, na.rm = TRUE)
  )
view(lit_by_year)

lit_by_year %>%
  filter(!is.na(year)) %>%
  ggplot() +
  geom_col(mapping = aes(x = year, y = fr_write))



### Correlation is not causation
  # Omitted variables (including compositional effects: age, sex)
  # The direction of the causality is sometimes unclear: employed <--> literacy

### Other issues
  # Number of observations
  # Outliers
  # Categories employed
  # Garbage in, garbage out (the results are as good as the data itself)

### There are ways of automatically creating dummies when many categories
install.packages("fastDummies")
library("fastDummies")

subset(paisley_data, select=c(forename, sex, countryb))
paisley_data <- dummy_cols(paisley_data, select_columns = 'countryb')
  # create new variables and assign 0/1 using all categories
  # missing values (NA) are respected

  # check the results
subset(paisley_data, 
       select=c(forename, sex, countryb, countryb_england, countryb_ireland, countryb_scotland, countryb_overseas)
       )

  # check what happens with the missing values (NA: not available)
subset(paisley_data, is.na(paisley_data$countryb), 
       select=c(forename, sex, countryb, countryb_england, countryb_ireland, countryb_scotland, countryb_overseas)
       )





