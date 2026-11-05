# Title:  Econometrics Lab 4: Observational Studies in R
# Author: Neil Lloyd
# Date:   1 November 2026
#
# R translation of ec3301-lab-4.do (version for RStudio on AppsAnywhere).
# Packages: haven, dplyr, ggplot2, sandwich, lmtest, broom, knitr.

# ============================================================================ #
# Load libraries
library("haven")
library("dplyr")
library("ggplot2")
library("sandwich")
library("lmtest")
library("broom")
library("knitr")

# Clear all objects from the R environment
rm(list = ls())

# Set working directory
setwd("[YOUR FILE PATH]")

# Open data
yelp_dta <- read_dta("yelp_restaurants.dta")


# ============================================================================ #
# Helper functions for tables (copied from Lab 3; run these once)
# ============================================================================ #

# Variance matrix for a fitted lm, mirroring Stata's vce() options.
#   "classical" = Stata default; "HC1" = vce(robust); "HC2" = vce(hc2)
get_vcov <- function(m, type = "classical") {
  if (type == "classical") return(vcov(m))
  vcovHC(m, type = type)
}

# Coefficient table with the chosen SEs
coef_table <- function(m, type = "classical") {
  tidy(coeftest(m, vcov. = get_vcov(m, type), df = m$df.residual))
}

# Side-by-side regression table (replaces  modelsummary).
#   models: named list of lm objects
#   types:  one SE type per model (recycled if length 1)
#   keep:   terms to show (NULL = all except the intercept)
#   note:   optional table note
reg_table <- function(models, types = "classical", keep = NULL, note = NULL, digits = 4) {
  types <- rep(types, length.out = length(models))
  stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
  tabs  <- Map(coef_table, models, types)
  terms <- if (is.null(keep)) {
    unique(unlist(lapply(tabs, `[[`, "term")))
  } else keep
  terms <- terms[!grepl("Intercept", terms)]

  rows <- lapply(terms, function(t) {
    b  <- sapply(tabs, function(x) { r <- x[x$term == t, ]
      if (nrow(r)) paste0(formatC(r$estimate, digits, format = "f"), stars(r$p.value)) else "" })
    se <- sapply(tabs, function(x) { r <- x[x$term == t, ]
      if (nrow(r)) paste0("(", formatC(r$std.error, digits, format = "f"), ")") else "" })
    rbind(c(t, b), c("", se))
  })
  out <- do.call(rbind, rows)
  out <- rbind(out,
               c("N",  sapply(models, nobs)),
               c("R2", sapply(models, function(m) formatC(summary(m)$r.squared, 3, format = "f"))),
               c("SE", types))
  colnames(out) <- c("", names(models))
  print(kable(out, align = c("l", rep("c", length(models)))))
  cat("* p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses.\n")
  if (!is.null(note)) cat(note, "\n")
}

# Balance table: means by group,
# difference, and p-value from an unequal-variance t-test (same as dtable).
balance_table <- function(data, vars, by = "treat", digits = 3) {
  data <- data[!is.na(data[[by]]), ]
  out <- t(sapply(vars, function(v) {
    x <- data[[v]]; g <- data[[by]]
    tt <- t.test(x ~ g, var.equal = FALSE)
    c(Group0     = mean(x[g == 0], na.rm = TRUE),
      Group1     = mean(x[g == 1], na.rm = TRUE),
      Difference = mean(x[g == 1], na.rm = TRUE) - mean(x[g == 0], na.rm = TRUE),
      p.value    = tt$p.value,
      N          = sum(!is.na(x)))
  }))
  colnames(out)[1:2] <- paste0(by, " = ", 0:1)
  print(kable(out, digits = digits))
}


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
balance_table(yelp_dta,
              c("cat_fastfood", "cat_pizza", "cat_burgers", "cat_sandwich", "cat_chinese", "cat_mexican",
                "cat_italian", "cat_japanese", "cat_breakfast", "cat_cafe", "cat_bar", "n_categories"),
              by = "delivery")

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
model1 <- lm(stars ~ delivery,
             data = yelp_dta)
coeftest(model1, vcov. = vcovHC(model1, type = "HC1"))

## Add controls that you think are good controls. Justify your choice
model2 <- lm(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
             data = yelp_dta)

model3 <- lm(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
             data = yelp_dta)

reg_table(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
          types = "HC1", keep = "delivery")

## Fix changes in observations
# Equivalent of -gen sample = e(sample)-: lm() records which rows it dropped
# because of missing values in na.action(); every other row is in the sample.
model3 <- lm(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
             data = yelp_dta)
yelp_dta$sample <- as.numeric(!(1:nrow(yelp_dta) %in% na.action(model3)))

model1 <- lm(stars ~ delivery,
             data = filter(yelp_dta, sample == 1))
model2 <- lm(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
             data = filter(yelp_dta, sample == 1))
model3 <- lm(stars ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
             data = filter(yelp_dta, sample == 1))

reg_table(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
          types = "HC1", keep = "delivery")

## For review_count
model1 <- lm(review_count ~ delivery,
             data = filter(yelp_dta, sample == 1))
model2 <- lm(review_count ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
             data = filter(yelp_dta, sample == 1))
model3 <- lm(review_count ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
             data = filter(yelp_dta, sample == 1))

reg_table(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
          types = "HC1", keep = "delivery")

## Log of review_count
yelp_dta <- yelp_dta %>%
  mutate(ln_reviews = log(review_count))

model1 <- lm(ln_reviews ~ delivery,
             data = filter(yelp_dta, sample == 1))
model2 <- lm(ln_reviews ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain,
             data = filter(yelp_dta, sample == 1))
model3 <- lm(ln_reviews ~ delivery + cat_cafe + cat_bar + cat_breakfast + cat_fastfood + n_categories + weekly_hours + chain + factor(price_range) + alcohol + factor(noise),
             data = filter(yelp_dta, sample == 1))

reg_table(list("(1)" = model1, "(2)" = model2, "(3)" = model3),
          types = "HC1", keep = "delivery")

## RESET test
resettest(model3, power = 2:4, type = "fitted")