# Title:  Econometrics Lab 3: RCT Replication in R (fixest + modelsummary version)
# Author: Neil Lloyd
# Date:   25 October 2026


# ============================================================================ #
# Load libraries
library("haven")
library("dplyr")
library("ggplot2")
library("fixest")
library("modelsummary")

# Clear all objects from the R environment
rm(list = ls())

# Set working directory
setwd("[INSERT FILE PATH]")

# Open data (zap_labels turns Stata value labels into plain numbers)
dr19_dta <- read_dta("dizonross2019.dta") %>% zap_labels()

  # zap_labels() turns Stata value labels into plain numbers
  # It can be removed. You may get a warning when using fixest about labelled data

# Table settings used throughout: Econ-standard stars, and report N and R2
stars <- c("*" = .1, "**" = .05, "***" = .01)
gof   <- c("nobs", "r.squared")


# ============================================================================ #
# Create between-sibling score gap (child 1 minus child 2, same value on both rows)

dr19_dta <- dr19_dta %>%
  group_by(hhid) %>%
  arrange(child, .by_group = TRUE) %>%
  mutate(ave_12 = first(score_ave) - nth(score_ave, 2)) %>%
  ungroup()


# ============================================================================ #
# Assignment

## How many schools are there?
length(unique(dr19_dta$school))

## How many households?
# Equivalent of -duplicates report hhid-: how many households appear once, twice, ...
dr19_dta %>%
  count(hhid, name = "copies") %>%
  count(copies, name = "households")

##
table(dr19_dta$treat, useNA = "ifany")

  # almost exactly 50-50

## Replicate the first four rows of Table 1, the balance table
datasummary_balance(~treat,
                    data = select(dr19_dta, treat, resp_female, resp_decides, resp_age, par_educ),
                    dinm_statistic = "p.value", fmt = 3)

  # p-values use an unequal-variance (Welch) t-test, so they can differ
  # slightly from Stata's dtable, which assumes equal variances

## Replicate the first four rows of Table 1, the balance table
datasummary_balance(~treat,
                    data = select(dr19_dta, treat, grade, child_age, child_female, attend_bl),
                    dinm_statistic = "p.value", fmt = 3)


# ============================================================================ #
# Graphical analysis

## Replicate panel A of Figure 3
means <- dr19_dta %>%
  mutate(treat = factor(treat, labels = c("Control", "Treat"))) %>%
  group_by(treat) %>%
  summarise(mean_err = mean(belief_err_bl, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = treat, y = mean_err)) +
  geom_col(fill = "lightblue", color = "white") +
  geom_text(aes(label = round(mean_err, 2)), vjust = -0.4) +
  labs(x = NULL, y = "mean of belief_err_bl")

## Replicate Panel C
means <- dr19_dta %>%
  mutate(treat = factor(treat, labels = c("Control", "Treat"))) %>%
  group_by(treat) %>%
  summarise(mean_err = mean(belief_err_end, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = treat, y = mean_err)) +
  geom_col(fill = "lightblue", color = "white") +
  geom_text(aes(label = round(mean_err, 2)), vjust = -0.4) +
  labs(x = NULL, y = "mean of belief_err_end")

## Replicate panel B
ggplot(dr19_dta, aes(x = score_ave, y = belief_bl,
                     colour = factor(treat, labels = c("Control", "Treat")),
                     linetype = factor(treat, labels = c("Control", "Treat")))) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50") +
  scale_colour_manual(values = c("blue", "green")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(colour = NULL, linetype = NULL) +
  theme(legend.position = c(0.85, 0.15))

## Replicate panel D
ggplot(dr19_dta, aes(x = score_ave, y = belief_end,
                     colour = factor(treat, labels = c("Control", "Treat")),
                     linetype = factor(treat, labels = c("Control", "Treat")))) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50") +
  scale_colour_manual(values = c("blue", "green")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(colour = NULL, linetype = NULL) +
  theme(legend.position = c(0.85, 0.15))


# ============================================================================ #
# ATE

## Estimate ATE
t.test(belief_end ~ treat, data = dr19_dta, var.equal = TRUE)   # = ttest belief_end, by(treat)
t.test(belief_end ~ treat, data = dr19_dta)                     # = ttest ..., unequal (Welch)

## Estimate ATE using linear regression
summary(feols(belief_end ~ treat, data = dr19_dta, vcov = "iid"))

## Estimate ATE using linear regression
summary(feols(belief_end ~ treat, data = dr19_dta, vcov = "hetero"))   # = vce(robust)

# fixest does not offer HC2, but modelsummary can compute it for an lm() model
modelsummary(lm(belief_end ~ treat, data = dr19_dta),
             vcov = "HC2", stars = stars, gof_map = gof, fmt = 4)       # = vce(hc2)


# ============================================================================ #
# Regression adjustment

## Recode wb_math and wb_engl: (-1 = 0) (0 = 100) (1 = 200)
dr19_dta <- dr19_dta %>%
  mutate(wb_math = case_when(wb_math == -1 ~ 0,
                             wb_math == 0  ~ 100,
                             wb_math == 1  ~ 200),
         wb_engl = case_when(wb_engl == -1 ~ 0,
                             wb_engl == 0  ~ 100,
                             wb_engl == 1  ~ 200))


## Replicate Table A.1, with regression adjustment; excluding school FEs
model1 <- feols(belief_end ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model2 <- feols(wb_math ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model3 <- feols(wb_engl ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model4 <- feols(lnwtp_math_engl ~ treat + par_educ + resp_literate + score_ave + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model5 <- feols(enrolled ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model6 <- feols(ln_educ_exp_end ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model7 <- feols(attend_admin ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")

modelsummary(list(belief_end = model1, wb_math = model2, wb_engl = model3,
                  lnwtp_math_engl = model4, enrolled = model5,
                  ln_educ_exp_end = model6, attend_admin = model7),
             coef_map = "treat", stars = stars, gof_map = gof, fmt = 4)

  # coef_map = "treat" only displays the coefficient on treat


## Estimate simple model controlling for school FEs
# "| school" absorbs the school FEs, like areg. With FEs, feols() clusters by
# the FE variable by default, so vcov = "iid" is needed to match areg.
summary(feols(belief_end ~ treat + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female | school,
              data = dr19_dta, vcov = "iid"))

## Clustered SEs
summary(feols(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade) | school,
              data = dr19_dta, cluster = ~hhid))


## Collect regression results and create a table
model1 <- feols(belief_end ~ treat,
                data = dr19_dta, vcov = "hetero")
model2 <- feols(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade),
                data = dr19_dta, vcov = "hetero")
model3 <- feols(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade) | school,
                data = dr19_dta, vcov = "hetero")
model4 <- feols(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade) | school,
                data = dr19_dta, cluster = ~hhid)

modelsummary(list("(1)" = model1, "(2)" = model2, "(3) school FE" = model3, "(4) school FE" = model4),
             coef_map = "treat", stars = stars, gof_map = gof, fmt = 4)
# Note: R2 in columns 3-4 includes the school FEs, as areg's does.


# ============================================================================ #
# Interacted model

## Estimate interacted model
summary(feols(belief_end ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
              data = dr19_dta, vcov = "iid"))

## Replicate the results in models 1-4
model1 <- feols(belief_end ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model2 <- feols(wb_math ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model3 <- feols(wb_engl ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model4 <- feols(lnwtp_math_engl ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")

modelsummary(list(belief_end = model1, wb_math = model2, wb_engl = model3, lnwtp_math_engl = model4),
             coef_map = c("treat", "treat:score_ave", "score_ave"),
             stars = stars, gof_map = gof, fmt = 4)


## Replicate Table 3 (Panel A)
model1 <- feols(enrolled ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model2 <- feols(ln_educ_exp_end ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model3 <- feols(attend_admin ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")

modelsummary(list(enrolled = model1, ln_educ_exp_end = model2, attend_admin = model3),
             coef_map = c("treat", "treat:score_ave", "score_ave"),
             stars = stars, gof_map = gof, fmt = 4)

## Replicate Table 3 (Panel B)
summary(dr19_dta$score_ave)
med <- median(dr19_dta$score_ave, na.rm = TRUE)
dr19_dta <- dr19_dta %>%
  mutate(above_median = if_else(score_ave > med, 1, 0))   # NA if score_ave is NA

model1 <- feols(enrolled ~ treat * above_median + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model2 <- feols(ln_educ_exp_end ~ treat * above_median + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model3 <- feols(attend_admin ~ treat * above_median + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")

modelsummary(list(enrolled = model1, ln_educ_exp_end = model2, attend_admin = model3),
             coef_map = c("treat", "treat:above_median", "above_median"),
             stars = stars, gof_map = gof, fmt = 4)


# ============================================================================ #
# Heterogeneity

## Replicate Table 4 (Panel B)
model1 <- feols(belief_end ~ treat * score_ave * par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model2 <- feols(wb_math ~ treat * score_math * par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")
model3 <- feols(wb_engl ~ treat * score_engl * par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta, vcov = "hetero")

modelsummary(list(belief_end = model1, wb_math = model2, wb_engl = model3),
             coef_omit = "Intercept|grade", stars = stars, gof_map = gof, fmt = 4)

  # coef_omit hides the intercept and grade dummies
