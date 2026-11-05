# Title:  Econometrics Lab 4: Observational Studies in R
# Author: Neil Lloyd
# Date:   1 November 2026
#
# R translation of ec3301-lab-4.do (version for students who can install packages).
# Packages: haven, dplyr, ggplot2, fixest, modelsummary.
# Install the last two once with: install.packages(c("fixest", "modelsummary"))


# ============================================================================ #
# Load libraries
library("haven")
library("dplyr")
library("ggplot2")
library("fixest")
library("modelsummary")
library("lmtest")

# Clear all objects from the R environment
rm(list = ls())

# Set working directory
setwd("[YOUR FILE PATH]")

# Open data
yelp_dta <- read_dta("yelp_restaurants.dta")

# Table settings used throughout: Econ-standard stars, and report N and R2
stars <- c("*" = .1, "**" = .05, "***" = .01)
gof   <- c("nobs", "r.squared")


# ============================================================================ #
# Sample selection

## Keep only open restaurants
yelp_dta %>% count(is_open)              # count() includes NA, like -tab, m-

yelp_dta <- yelp_dta %>%
  filter(is_open == 1)

## What type of variable is state, and how many values does it take on?
class(yelp_dta$state)                    # = describe state
length(unique(yelp_dta$state))           # = codebook state

  # 16 states

## Keep restaurants in one state, for which there is a reasonable sample size.
yelp_dta %>% count(state)

yelp_dta <- yelp_dta %>%
  filter(state == "FL")


# ============================================================================ #
# Assignment

## What share of restaurants in the sample offer delivery
yelp_dta %>%
  count(delivery) %>%
  mutate(prop = n / sum(n))

## Which category of food has the highest propensity to offer delivery
# Equivalent of the -foreach- loop: mean of delivery among restaurants in each category
yelp_dta %>%
  summarise(across(cat_fastfood:cat_bar, ~ mean(delivery[.x == 1], na.rm = TRUE)))

  # for florida it's fastfood

## Create a table that shows the share of restaurants registered under each
## category (and the number of categories) by delivery status. Include a test of significance.
datasummary_balance(~ delivery,
                    data = yelp_dta %>%
                      filter(!is.na(delivery)) %>%
                      select(delivery, cat_fastfood, cat_pizza, cat_burgers, cat_sandwich, cat_chinese, cat_mexican,
                             cat_italian, cat_japanese, cat_breakfast, cat_cafe, cat_bar, n_categories),
                    dinm_statistic = "p.value", fmt = 3)

  # p-values use an unequal-variance (Welch) t-test, so they can differ
  # slightly from Stata's dtable, which assumes equal variances

## Create a bar chart showing the distribution of price_range by delivery
# Equivalent of -graph bar, over(price_range) by(delivery)-: percent of restaurants
# in each price range, separately by delivery status
price_shares <- yelp_dta %>%
  filter(!is.na(price_range), !is.na(delivery)) %>%
  count(delivery, price_range) %>%
  group_by(delivery) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

ggplot(price_shares, aes(x = factor(price_range), y = percent)) +
  geom_col(fill = "lightblue", color = "white") +
  facet_wrap(~ delivery, labeller = label_both) +
  labs(x = "price_range", y = "percent")

## Create a histogram of weekly_hours
ggplot(filter(yelp_dta, !is.na(delivery)), aes(x = weekly_hours)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "white", bins = 30) +
  facet_wrap(~ delivery, labeller = label_both)


# ============================================================================ #
# Correlation

## Create a table that shows the mean stars by whether a restaurant offers delivery
yelp_dta %>%
  group_by(delivery) %>%
  summarise(mean = mean(stars, na.rm = TRUE),
            sd = sd(stars, na.rm = TRUE),
            n = sum(!is.na(stars)),
            .groups = "drop")

## Plot
# Equivalent of -lab def- / -lab val-: create labelled factor versions for graphs,
# keeping the original 0/1 variables for the regressions
yelp_dta <- yelp_dta %>%
  mutate(delivery_lbl = factor(delivery, levels = c(0, 1),
                               labels = c("Delivery not offered", "Delivery offered")))

means <- yelp_dta %>%
  filter(!is.na(delivery_lbl)) %>%
  group_by(delivery_lbl) %>%
  summarise(mean_stars = mean(stars, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = delivery_lbl, y = mean_stars)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(x = NULL, y = "Stars", title = "Average Yelp stars by delivery status")

## Test whether restaurants that offer delivery have a statistically different
## average number of stars to those who do not.
t.test(stars ~ delivery, data = yelp_dta)          # Welch test = ttest ..., unequal

## Same for review_count
yelp_dta %>%
  group_by(delivery) %>%
  summarise(mean = mean(review_count, na.rm = TRUE),
            sd = sd(review_count, na.rm = TRUE),
            n = sum(!is.na(review_count)),
            .groups = "drop")

means <- yelp_dta %>%
  filter(!is.na(delivery_lbl)) %>%
  group_by(delivery_lbl) %>%
  summarise(mean_reviews = mean(review_count, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = delivery_lbl, y = mean_reviews)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(x = NULL, y = "Reviews", title = "Average number of Yelp reviews by delivery status")

t.test(review_count ~ delivery, data = yelp_dta)

## By cat_fastfood
yelp_dta <- yelp_dta %>%
  mutate(cat_fastfood_lbl = factor(cat_fastfood, levels = c(0, 1),
                                   labels = c("non-fastfood", "fastfood")))

means <- yelp_dta %>%
  filter(!is.na(delivery_lbl)) %>%
  group_by(cat_fastfood_lbl, delivery_lbl) %>%
  summarise(mean_stars = mean(stars, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = delivery_lbl, y = mean_stars)) +
  geom_col(fill = "lightblue", color = "white") +
  facet_wrap(~ cat_fastfood_lbl) +
  labs(x = NULL, y = "Stars")

## By price_range
yelp_dta <- yelp_dta %>%
  mutate(price_range_lbl = factor(price_range, levels = 1:4,
                                  labels = c("Least expensive", "Less expensive",
                                             "More expensive", "Most expensive")))

means <- yelp_dta %>%
  filter(!is.na(delivery_lbl), !is.na(price_range_lbl)) %>%
  group_by(price_range_lbl, delivery_lbl) %>%
  summarise(mean_reviews = mean(review_count, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = delivery_lbl, y = mean_reviews)) +
  geom_col(fill = "lightblue", color = "white") +
  facet_wrap(~ price_range_lbl, nrow = 2) +
  labs(x = NULL, y = "Reviews")


# ============================================================================ #
# Regression analysis of stars and review_count

## Estimate a simple regression with HC1 robust SEs. Is there a significant relationship?
model1 <- feols(stars ~ delivery,
                data = yelp_dta, vcov = "hetero")
summary(model1)

## Add controls that you think are good controls. Justify your choice
model2 <- feols(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
                data = yelp_dta, vcov = "hetero")

model3 <- feols(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
                data = yelp_dta, vcov = "hetero")

modelsummary(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
             coef_map = "delivery", stars = stars, gof_map = gof, fmt = 4)

## Fix changes in observations
# Equivalent of -gen sample = e(sample)-: obs() returns the rows used by feols()
model3 <- feols(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
                data = yelp_dta, vcov = "hetero")
yelp_dta$sample <- as.numeric(1:nrow(yelp_dta) %in% obs(model3))

model1 <- feols(stars ~ delivery,
                data = filter(yelp_dta, sample == 1), vcov = "hetero")
model2 <- feols(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
                data = filter(yelp_dta, sample == 1), vcov = "hetero")
model3 <- feols(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
                data = filter(yelp_dta, sample == 1), vcov = "hetero")

modelsummary(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
             coef_map = "delivery", stars = stars, gof_map = gof, fmt = 4)

## For review_count
model1 <- feols(review_count ~ delivery,
                data = filter(yelp_dta, sample == 1), vcov = "hetero")
model2 <- feols(review_count ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
                data = filter(yelp_dta, sample == 1), vcov = "hetero")
model3 <- feols(review_count ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
                data = filter(yelp_dta, sample == 1), vcov = "hetero")

modelsummary(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
             coef_map = "delivery", stars = stars, gof_map = gof, fmt = 4)

## Log of review_count
yelp_dta <- yelp_dta %>%
  mutate(ln_reviews = log(review_count))

model1 <- feols(ln_reviews ~ delivery,
                data = filter(yelp_dta, sample == 1), vcov = "hetero")
model2 <- feols(ln_reviews ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
                data = filter(yelp_dta, sample == 1), vcov = "hetero")
model3 <- feols(ln_reviews ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
                data = filter(yelp_dta, sample == 1), vcov = "hetero")

modelsummary(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
             coef_map = "delivery", stars = stars, gof_map = gof, fmt = 4)

## RESET test
resettest(model3, power = 2:4, type = "fitted")