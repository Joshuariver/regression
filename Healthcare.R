rm(list=ls())
setwd("~/R/Regression/exercise")

library(tidyverse)
library(RNHANES)
library(ggplot2)

d07 = nhanes_load_data("DEMO_E", "2007-2008") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR) %>% 
  left_join(nhanes_load_data("VID_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS) %>% 
  left_join(nhanes_load_data("BIOPRO_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, LBXSCA) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA)

d09 = nhanes_load_data("DEMO_F", "2009-2010") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR) %>% 
  left_join(nhanes_load_data("VID_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS) %>% 
  left_join(nhanes_load_data("BIOPRO_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD,  LBXSCA) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA)

dat = rbind(d07, d09)

all = dat %>% 
  # exclude missings
  filter(!is.na(vitD), !is.na(Calcium)) %>% 
  mutate(Gender = recode_factor(RIAGENDR, 
                                `1` = "Males", 
                                `2` = "Females"))

head(all)

# Distribution of calcium level:

ggplot(data = all) + 
  geom_histogram(aes(Calcium), binwidth = 0.2)

# The model
# I will use the function lm() to create a linear regression model. In the # first model I will not adjust for confunders, insted, I will do a 
# univariate model.

fit1 <- lm(Calcium ~ vitD, data = all)

# To see the results, estimates, pvalues etc use summary function.

summary(fit1)  


# The 95% confidence interval:

confint(fit1)


# Intepretation

# From the results, I find that vitamin D is associated with calcium in the 
# blood because the p-value is less than 0.05. Next, I see the direction of 
# the association. The positive beta estimate (\(\beta\) = 0.0016) indicate 
# that with increasing vitamin D in the blood, the levels of calcium also 
# increases.

# To visualize this association I will use the ggplot and the function 
# geom_smooth. See below:

ggplot(all, aes(x = vitD, y = Calcium)) +
  geom_point() +
  geom_smooth(method="lm")


# Multivariable adjusted models

# Often, a significant association could be explained by confounders. 
# According to Wikipedia, a confounder is a variable that influences both 
# the dependent variable and independent variable, causing a spurious 
# association. Therefore, it is important to adjust for major confounders 
# such as age and gender. The levels of vitamin D in the blood are 
# dependent to age because older adults have lower vitamin D in blood 
# compared to young adults.

# To conduct a multivariable-adjusted model I add other variables to the 
# model, in this example, I will add age and gender.            

fit2 <- lm(Calcium ~ vitD + Gender + RIDAGEYR, data = all)
summary(fit2)

# Stratifing analysis
# To evaluate the association separately in men and women is necessary to 
# conduct a stratified analysis. For this, I need to separate men and women 
# into two different datasets and run linear regression for each group.

allfem = all %>% 
  filter(Gender == "Females")
allmal = all %>% 
  filter(Gender == "Males")

# Linear regression in women and men

fitfem <- lm(Calcium ~ vitD + RIDAGEYR, data = allfem)
summary(fitfem)

fitmal <- lm(Calcium ~ vitD + RIDAGEYR, data = allmal)
summary(fitmal)

