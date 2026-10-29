# Title:  Econometrics Lab 3: RCT Replication in R (AppsAnywhere version)
# Author: Neil Lloyd
# Date:   25 October 2026


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
setwd("[INSERT FILE PATH]")

# Open data 
dr19_dta <- read_dta("dizonross2019.dta") %>% zap_labels()

  # zap_labels() turns Stata value labels into plain numbers
  # It can be removed.


# ============================================================================ #
# Helper functions for tables (run these once; students don't need to edit them)
# ============================================================================ #

# Variance matrix for a fitted lm, mirroring Stata's vce() options.
#   "classical" = Stata default; "HC1" = vce(robust); "HC2" = vce(hc2);
#   "cluster" = vce(cluster hhid)
get_vcov <- function(m, type = "classical") {
  if (type == "classical") return(vcov(m))
  if (type == "cluster")   return(vcovCL(m, cluster = ~hhid, type = "HC1"))
  vcovHC(m, type = type)
}

# Coefficient table with the chosen SEs. For clustered SEs, p-values use G-1
# degrees of freedom, as Stata does.
coef_table <- function(m, type = "classical") {
  df <- if (type == "cluster") {
    length(unique(expand.model.frame(m, ~hhid)$hhid)) - 1   # number of clusters - 1
  } else {
    m$df.residual
  }
  tidy(coeftest(m, vcov. = get_vcov(m, type), df = df))
}

# Side-by-side regression table (replaces etable).
#   models: named list of lm objects
#   types:  one SE type per model (recycled if length 1)
#   keep:   terms to show (NULL = all except intercept and grade/school dummies)
reg_table <- function(models, types = "classical", keep = NULL, digits = 4) {
  types <- rep(types, length.out = length(models))
  stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
  tabs  <- Map(coef_table, models, types)
  terms <- if (is.null(keep)) {
    unique(unlist(lapply(tabs, `[[`, "term")))
  } else keep
  terms <- terms[!grepl("Intercept|factor\\(grade\\)|factor\\(school\\)", terms)]

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
}

# Balance table (replaces dtable, by(treat, tests)): means by group,
# difference, and p-value from an equal-variance t-test (same as reg x treat).
balance_table <- function(data, vars, by = "treat", digits = 3) {
  out <- t(sapply(vars, function(v) {
    x <- data[[v]]; g <- data[[by]]
    tt <- t.test(x ~ g, var.equal = FALSE)
    c(Control    = mean(x[g == 0], na.rm = TRUE),
      Treatment  = mean(x[g == 1], na.rm = TRUE),
      Difference = mean(x[g == 1], na.rm = TRUE) - mean(x[g == 0], na.rm = TRUE),
      p.value    = tt$p.value,
      N          = sum(!is.na(x)))
  }))
  print(kable(out, digits = digits))
}


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
balance_table(dr19_dta, c("resp_female", "resp_decides", "resp_age", "par_educ"))

## Replicate the first four rows of Table 1, the balance table
balance_table(dr19_dta, c("grade", "child_age", "child_female", "attend_bl"))


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
model_ate <- lm(belief_end ~ treat, data = dr19_dta)
summary(model_ate)

## Estimate ATE using linear regression
coeftest(model_ate, vcov. = vcovHC(model_ate, type = "HC1"))   # = vce(robust)
coeftest(model_ate, vcov. = vcovHC(model_ate, type = "HC2"))   # = vce(hc2)


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
model1 <- lm(belief_end ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model2 <- lm(wb_math ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model3 <- lm(wb_engl ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model4 <- lm(lnwtp_math_engl ~ treat + par_educ + resp_literate + score_ave + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model5 <- lm(enrolled ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model6 <- lm(ln_educ_exp_end ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model7 <- lm(attend_admin ~ treat + par_educ + resp_literate + ave_12 + score_ave + child_female + factor(grade) + resp_female,
             data = dr19_dta)

reg_table(list(belief_end = model1, wb_math = model2, wb_engl = model3,
               lnwtp_math_engl = model4, enrolled = model5,
               ln_educ_exp_end = model6, attend_admin = model7),
          types = "HC1", keep = "treat")


## Estimate simple model controlling for school FEs
model_fe <- lm(belief_end ~ treat + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female + factor(school),
               data = dr19_dta)
summary(model_fe)

## Clustered SEs
model_fe_cl <- lm(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade) + factor(school),
                  data = dr19_dta)
coeftest(model_fe_cl, vcov. = vcovCL(model_fe_cl, cluster = ~hhid))


## Collect regression results and create a table
model1 <- lm(belief_end ~ treat,
             data = dr19_dta)
model2 <- lm(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade),
             data = dr19_dta)
model3 <- lm(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade) + factor(school),
             data = dr19_dta)
model4 <- lm(belief_end ~ treat + par_educ + resp_female + resp_literate + ave_12 + score_ave + child_female + factor(grade) + factor(school),
             data = dr19_dta)

reg_table(list("(1)" = model1, "(2)" = model2, "(3) school FE" = model3, "(4) school FE" = model4),
          types = c("HC1", "HC1", "HC1", "cluster"), keep = "treat")
# Note: R2 in columns 3-4 includes the school dummies, as areg's does.


# ============================================================================ #
# Interacted model

## Estimate interacted model
model_int <- lm(belief_end ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
                data = dr19_dta)
summary(model_int)

## Replicate the results in models 1-4
model1 <- lm(belief_end ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model2 <- lm(wb_math ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model3 <- lm(wb_engl ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model4 <- lm(lnwtp_math_engl ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)

reg_table(list(belief_end = model1, wb_math = model2, wb_engl = model3, lnwtp_math_engl = model4),
          types = "HC1", keep = c("treat", "treat:score_ave", "score_ave"))


## Replicate Table 3 (Panel A)
model1 <- lm(enrolled ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model2 <- lm(ln_educ_exp_end ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model3 <- lm(attend_admin ~ treat * score_ave + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)

reg_table(list(enrolled = model1, ln_educ_exp_end = model2, attend_admin = model3),
          types = "HC1", keep = c("treat", "treat:score_ave", "score_ave"))

## Replicate Table 3 (Panel B)
summary(dr19_dta$score_ave)
med <- median(dr19_dta$score_ave, na.rm = TRUE)
dr19_dta <- dr19_dta %>%
  mutate(above_median = if_else(score_ave > med, 1, 0))   # NA if score_ave is NA

model1 <- lm(enrolled ~ treat * above_median + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model2 <- lm(ln_educ_exp_end ~ treat * above_median + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model3 <- lm(attend_admin ~ treat * above_median + par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)

reg_table(list(enrolled = model1, ln_educ_exp_end = model2, attend_admin = model3),
          types = "HC1", keep = c("treat", "treat:above_median", "above_median"))


# ============================================================================ #
# Heterogeneity

## Replicate Table 4 (Panel B)
model1 <- lm(belief_end ~ treat * score_ave * par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model2 <- lm(wb_math ~ treat * score_math * par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)
model3 <- lm(wb_engl ~ treat * score_engl * par_educ + resp_literate + ave_12 + child_female + factor(grade) + resp_female,
             data = dr19_dta)

reg_table(list(belief_end = model1, wb_math = model2, wb_engl = model3), types = "HC1")
