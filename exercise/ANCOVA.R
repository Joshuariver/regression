# ANCOVA

rm(list=ls())
setwd("~/R/Regression/exercise")


# ANOVA is a subset of the general linear model and specifically
# in R aov is just a wrapper around lm. I can’t tell you the
# number of times I read that and it’s true. But, in many
# disciplines and for me it is an important subset worthy of it’s own
# time. You won’t find any info here about how to do things with lm
# although you certainly could.

# contrasts can play an important role in understanding your
# results. This is especially true if you happen to have an unbalanced
# design. I’m only going to glance on the topic here since I’m going
# to run a balanced design and therefore contrasts are tangential. For
# a very good academic discussion of contrasts especially using R and
# especially at the introductory level I very strongly recommend
# Learning Statistics with R,
# (search and ye shall find) where Danielle has done a great job of
# covering the topic in several places. The text is free for the
# download and is invaluable although it doesn’t cover ANCOVA per se.

# I’m going to use packages above and beyond base and stats as
# needed. Everything could be done with the basics but this is a
# practical approach not so much a theoretical approach. I’ll even put
# in a very subtle plug for a function I wrote and maintain on CRAN
# although it’s by no means required.

# https://www.r-bloggers.com/ancova-example-april-18-2019/


require(car) # get the right sums of squares calculations
require(dplyr) # for manipulating our data
require(ggplot2) # for plotting and for our dataset
require(sjstats) # save us time computing key ANOVA stats beyond car
require(broom) # nice for making our results in neat tibbles
require(emmeans) # for marginal means calculations
# a shameless plug for a function I wrote called Plot2WayANOVA
# optional for you
# devtools::install_github("ibecav/CGPfunctions")
library(CGPfunctions)
theme_set(theme_bw()) # set theme
str(diamonds)


set.seed(1234)
diamonds2 <- filter(diamonds, 
                    cut %in% c("Fair", "Good") & 
                      color %in% c("E", "F", "G") & 
                      carat < 1.75) 
diamonds2 <- droplevels(diamonds2)
one <- diamonds2 %>% filter(cut == "Fair" & color == "E") %>% sample_n(218)
two <- diamonds2 %>% filter(cut == "Fair" & color == "F") %>% sample_n(218)
three <- diamonds2 %>% filter(cut == "Fair" & color == "G") %>% sample_n(218)
four <- diamonds2 %>% filter(cut == "Good" & color == "E") %>% sample_n(218)
five <- diamonds2 %>% filter(cut == "Good" & color == "F") %>% sample_n(218)
six <- diamonds2 %>% filter(cut == "Good" & color == "G") %>% sample_n(218)
diamonds2 <- bind_rows(one, two, three, four, five, six)
str(diamonds2)


Plot2WayANOVA(price ~ color * cut, diamonds2, mean.label = TRUE)

ggplot(diamonds2, aes(x = carat, y= price)) + 
  geom_point(alpha = 1/10) + 
  geom_smooth(method = "loess", color = "red") + 
  geom_smooth(method = "lm", color = "blue")

broom::glance(lm(price ~ carat, diamonds2))

broom::glance(lm(carat ~ cut, diamonds2))

broom::glance(lm(carat ~ color, diamonds2))

caratonly <- lm(price ~ carat, diamonds2)

noCOVmodel <- aov(price ~ cut * color, diamonds2)
car::Anova(noCOVmodel, type = 2)

broom::glance(noCOVmodel)

sjstats::anova_stats(car::Anova(noCOVmodel, type = 2)) %>% select(1:7)

COVmodel <- aov(price ~ cut * color + carat, diamonds2)
car::Anova(COVmodel, type = 2)

broomExtra::glance(COVmodel)

sjstats::anova_stats(car::Anova(COVmodel, type = 2)) %>% select(1:7)

diamonds3 <- diamonds2 %>% 
  mutate(OriginalPred = predict(noCOVmodel), 
         WithCaratPred = predict(COVmodel), 
         CaratOnlyPred = predict(caratonly)) %>% 
  select(price, cut, color, OriginalPred, carat, CaratOnlyPred, WithCaratPred)
diamonds3

# first the means
emmeans::pmmeans(noCOVmodel, "cut", by = "color")

emmeans::emmip(noCOVmodel, cut ~ color, CIs = TRUE)

# pairwise comparisons
pairs(emmeans::pmmeans(noCOVmodel, "color", by = "cut"), adjust = "scheffe")

# not done above you can easily specify just one factor
emmeans::pmmeans(noCOVmodel, "cut")

# or the other factor
emmeans::pmmeans(noCOVmodel, "color")

# Controlling for carat

emmeans::pmmeans(COVmodel, "carat")

emmeans::pmmeans(COVmodel, "cut", by = "color")

emmip(COVmodel, cut ~ color, CIs = TRUE)

pairs(emmeans::pmmeans(COVmodel, "cut", by = "color"), adjust = "scheffe")

pairs(emmeans::pmmeans(COVmodel, "color", by = "cut"), adjust = "scheffe")

pairs(emmeans::pmmeans(COVmodel, "color"), adjust = "scheffe")

pairs(emmeans::pmmeans(COVmodel, "cut"), adjust = "scheffe")

withCOV <- broom::tidy(emmeans::pmmeans(COVmodel, "cut", by = "color"))
noCOV <- broom::tidy(emmeans::pmmeans(noCOVmodel, "cut", by = "color"))
ggplot(data = withCOV, 
       aes(x = color, 
           y = estimate, 
           group = cut, 
           color = cut)) +
  geom_point(shape = 18, 
             size = 4) +
  geom_line(size = 2) +
  ggrepel::geom_label_repel(aes(label = round(estimate, 2)),
                            nudge_x = -.35,
                            color = "black") +
  geom_point(data = noCOV,
             aes(x = color, 
                 y = estimate, 
                 group = cut, 
                 color = cut)) +
  geom_line(data = noCOV,
            aes(x =color, 
                y = estimate, 
                group = cut, 
                color = cut),
            linetype = 2) +
  ggrepel::geom_label_repel(data = noCOV,
                            aes(label = round(estimate, 2)),
                            nudge_x = .35,
                            color = "black") +
  labs(title = "Estimated Mean Diamond Price",
       subtitle = "Dashed line without carat as a covariate",
       color = "Cut") +
  ylab("Price") +
  xlab("Color") +
  expand_limits(y = c(2700,4200))

COVmodelError <- aov(price ~ cut * color + Error(carat), diamonds2)
summary(COVmodelError)

car::Anova(COVmodelError$Within, type = 2)

broomExtra::glance(COVmodelError$Within)

sjstats::anova_stats(car::Anova(COVmodelError$Within, type = 2))  %>% select(1:7)

