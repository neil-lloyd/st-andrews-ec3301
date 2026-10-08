# Title: Econometrics Lab 2: Linear Regression in R
# Author: Neil Lloyd
# Date: 25 September 2026

# ============================================================================ #
# Load libraries
library("haven")
library("dplyr")
library("ggplot2")

# Clear all objects from the R environment
rm(list = ls())

# Set working directory
setwd("[INSERT YOUR WORKING DIRECTORY]")

# ============================================================================ #
# Part 1

# Open data
shs <- read_dta("shs2023.dta")

# ============================================================================ #
# Pre-amble from Lab 1

shs <- shs %>%
  rename(dwell_type = hb1, own_grp = hb509) %>%
  mutate(dwell_type = structure(dwell_type, label = "Type of dwelling"),
         own_grp = structure(own_grp, label = "Ownership of the dwelling"))

shs$MD20QUIN <- labelled(
  shs$MD20QUIN,
  labels = c("Bottom: 0-20%" = 1, "Lower: 20-40%" = 2, "Middle: 40-60%" = 3, "Upper: 60-80%" = 4, "Top: 80-100%" = 5))

# Stata's sequential -replace- means the last condition wins, so it goes first in case_when()
shs <- shs %>%
  mutate(
    amt_sum = case_when(
      shared_ownership_amt > 0 ~ 3,
      mortgage_amt > 0 ~ 2,
      rent_amt > 0 ~ 1,
      TRUE ~ NA_real_
    ),
    payment = case_when(
      amt_sum == 1 ~ as.numeric(rent_amt),
      amt_sum == 2 ~ as.numeric(mortgage_amt),
      amt_sum == 3 ~ as.numeric(shared_ownership_amt),
      TRUE ~ NA_real_
    )
  )

# ============================================================================ #
# Create variables

## Energy
shs %>% count(htcostsum_1 = as_factor(htcostsum_1))

shs %>%
  group_by(htcostsum_1 = as_factor(htcostsum_1)) %>%
  summarise(
    n = sum(!is.na(htcostamt_1)),
    mean = mean(htcostamt_1, na.rm = TRUE),
    sd = sd(htcostamt_1, na.rm = TRUE),
    .groups = "drop"
  )


shs <- shs %>%
  mutate(
    energy_cost = rowSums(across(starts_with("htcostamt_")), na.rm = TRUE),
    energy_cost = if_else(rowSums(!is.na(across(starts_with("htcostamt_")))) == 0, NA_real_, energy_cost)
  )

sum(is.na(shs$energy_cost))
sum(shs$energy_cost == 0, na.rm = TRUE)
sum(shs$energy_cost > 0, na.rm = TRUE)


shs %>%
  summarise(
    n = sum(!is.na(energy_cost)),
    mean = mean(energy_cost, na.rm = TRUE),
    sd = sd(energy_cost, na.rm = TRUE),
    min = min(energy_cost, na.rm = TRUE),
    p1 = quantile(energy_cost, 0.01, na.rm = TRUE, type = 2),
    p5 = quantile(energy_cost, 0.05, na.rm = TRUE, type = 2),
    p10 = quantile(energy_cost, 0.10, na.rm = TRUE, type = 2),
    p25 = quantile(energy_cost, 0.25, na.rm = TRUE, type = 2),
    p50 = quantile(energy_cost, 0.50, na.rm = TRUE, type = 2),
    p75 = quantile(energy_cost, 0.75, na.rm = TRUE, type = 2),
    p90 = quantile(energy_cost, 0.90, na.rm = TRUE, type = 2),
    p95 = quantile(energy_cost, 0.95, na.rm = TRUE, type = 2),
    p99 = quantile(energy_cost, 0.99, na.rm = TRUE, type = 2),
    max = max(energy_cost, na.rm = TRUE)
  )

shs <- shs %>%
  mutate(energy_cost = energy_cost / 12,
         energy_cost = structure(energy_cost, label = "Household energy costs (monthly £s)"))

## Housing
shs <- shs %>%
  rename(housing_cost = payment) %>%
  mutate(housing_cost = structure(housing_cost, label = "Household housing costs (monthly £s)"))

# Equivalent of -compare housing_cost hcost_amt-
shs %>%
  filter(!is.na(housing_cost) & !is.na(hcost_amt)) %>%
  mutate(diff = housing_cost - hcost_amt,
         comparison = case_when(
           diff < 0 ~ "housing_cost < hcost_amt",
           diff == 0 ~ "housing_cost = hcost_amt",
           diff > 0 ~ "housing_cost > hcost_amt"
         )) %>%
  group_by(comparison) %>%
  summarise(
    n = n(),
    min_diff = min(diff),
    mean_diff = mean(diff),
    max_diff = max(diff),
    .groups = "drop"
  )

shs %>%
  summarise(
    n_housing_missing = sum(is.na(housing_cost)),
    n_hcost_missing = sum(is.na(hcost_amt)),
    n_both_missing = sum(is.na(housing_cost) & is.na(hcost_amt))
  )

#View(select(shs, mortgage_amt, rent_amt, shared_ownership_amt, hcost_amt, housing_cost))

sum(is.na(shs$housing_cost))
sum(shs$housing_cost == 0, na.rm = TRUE)
sum(shs$housing_cost >= 0, na.rm = TRUE)
    # Note: Stata treats missing (.) as larger than any number, so -count if housing_cost>=0-
    # also counts the missing values. In R, NA >= 0 is NA, so na.rm = TRUE drops them.

shs %>%
  summarise(
    n = sum(!is.na(housing_cost)),
    mean = mean(housing_cost, na.rm = TRUE),
    sd = sd(housing_cost, na.rm = TRUE),
    min = min(housing_cost, na.rm = TRUE),
    p1 = quantile(housing_cost, 0.01, na.rm = TRUE, type = 2),
    p5 = quantile(housing_cost, 0.05, na.rm = TRUE, type = 2),
    p10 = quantile(housing_cost, 0.10, na.rm = TRUE, type = 2),
    p25 = quantile(housing_cost, 0.25, na.rm = TRUE, type = 2),
    p50 = quantile(housing_cost, 0.50, na.rm = TRUE, type = 2),
    p75 = quantile(housing_cost, 0.75, na.rm = TRUE, type = 2),
    p90 = quantile(housing_cost, 0.90, na.rm = TRUE, type = 2),
    p95 = quantile(housing_cost, 0.95, na.rm = TRUE, type = 2),
    p99 = quantile(housing_cost, 0.99, na.rm = TRUE, type = 2),
    max = max(housing_cost, na.rm = TRUE)
  )

shs %>% count(own_grp = as_factor(own_grp))

shs %>%
  group_by(own_grp = as_factor(own_grp)) %>%
  summarise(
    n = sum(!is.na(housing_cost)),
    mean = mean(housing_cost, na.rm = TRUE),
    sd = sd(housing_cost, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(shs, aes(x = housing_cost)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(
    x = "Household housing costs (monthly £s)",
    y = "Count"
  )

## Income
summary(shs$annetinc)

shs %>% count(incsum = as_factor(incsum))       # count() includes NA, like -tab, m-

shs %>%
  group_by(incsum = as_factor(incsum)) %>%
  summarise(
    n = sum(!is.na(annetinc)),
    mean = mean(annetinc, na.rm = TRUE),
    sd = sd(annetinc, na.rm = TRUE),
    .groups = "drop"
  )

shs <- shs %>%
  mutate(income = annetinc / 12,
         income = structure(income, label = "Household income (monthly £s)"))

sum(is.na(shs$income))
sum(shs$income == 0, na.rm = TRUE)
sum(shs$income >= 0, na.rm = TRUE)

shs %>%
  summarise(
    n = sum(!is.na(income)),
    mean = mean(income, na.rm = TRUE),
    sd = sd(income, na.rm = TRUE),
    min = min(income, na.rm = TRUE),
    p1 = quantile(income, 0.01, na.rm = TRUE, type = 2),
    p5 = quantile(income, 0.05, na.rm = TRUE, type = 2),
    p10 = quantile(income, 0.10, na.rm = TRUE, type = 2),
    p25 = quantile(income, 0.25, na.rm = TRUE, type = 2),
    p50 = quantile(income, 0.50, na.rm = TRUE, type = 2),
    p75 = quantile(income, 0.75, na.rm = TRUE, type = 2),
    p90 = quantile(income, 0.90, na.rm = TRUE, type = 2),
    p95 = quantile(income, 0.95, na.rm = TRUE, type = 2),
    p99 = quantile(income, 0.99, na.rm = TRUE, type = 2),
    max = max(income, na.rm = TRUE)
  )

shs <- shs %>%
  filter(!is.na(energy_cost) & !is.na(housing_cost) & !is.na(income))

# ============================================================================ #
# Simple regression

ggplot(shs, aes(x = income, y = energy_cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

reg1 <- lm(energy_cost ~ income, data = shs)
summary(reg1)

shs$energy_hat <- predict(reg1, newdata = shs)

energy <- ggplot(shs, aes(x = income, y = energy_cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 1500, 500)) +
  labs(y = "Energy costs")

housing <- ggplot(shs, aes(x = income, y = housing_cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 1500, 500)) +
  labs(y = "Housing costs")

# Equivalent of -graph combine-: stack the two outcomes and use facet_wrap()
combined <- bind_rows(
  shs %>% transmute(income, cost = as.numeric(energy_cost), type = "Energy costs"),
  shs %>% transmute(income, cost = as.numeric(housing_cost), type = "Housing costs")
)

ggplot(combined, aes(x = income, y = cost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 1500, 500)) +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Household income (monthly £s)", y = NULL)

shs <- shs %>%
  mutate(energy_share = energy_cost / (energy_cost + housing_cost))
summary(shs$energy_share)

# For ln(0) and ln(negative), R returns -Inf/NaN, so set these to NA
shs <- shs %>%
  mutate(ln_income = if_else(income > 0, log(income), NA_real_))

reg2 <- lm(energy_share ~ ln_income, data = shs)
summary(reg2)

means <- shs %>%
  mutate(area = as_factor(area)) %>%
  group_by(area) %>%
  summarise(mean_share = mean(energy_share, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = area, y = mean_share)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(x = NULL, y = "Energy share") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shs <- rename(shs, bedrooms = hc4)

reg3 <- lm(energy_share ~ ln_income + hhsize + bedrooms + factor(own_grp) + factor(dwell_type) + factor(area),
           data = shs)
summary(reg3)

reg4 <- lm(energy_share ~ ln_income + hhsize + bedrooms + relevel(factor(own_grp), ref = 2) + factor(dwell_type) + relevel(factor(area), ref = 3),
           data = shs)
summary(reg4)


# ============================================================================ #
# Part 2

# USDA
# Source: https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads

usda <- read.csv("usda2025.csv")
names(usda) <- tolower(names(usda))

usda <- usda %>%
  mutate(ln_pc_snapben17 = if_else(pc_snapben17 > 0, log(pc_snapben17), NA_real_))

cor(usda[, c("ln_pc_snapben17", "snapspth17", "pct_snap17")], use = "complete.obs")

summary(lm(ffrpth20 ~ ln_pc_snapben17, data = usda))
summary(lm(ffrpth20 ~ snapspth17, data = usda))
summary(lm(ffrpth20 ~ pct_snap17, data = usda))

cor(usda[, c("snapspth17", "grocpth16")], use = "complete.obs")

summary(lm(ffrpth20 ~ snapspth17 + povrate21 + fsrpth16 + grocpth16 + metro23 + pct_nhblack20 + pct_hisp20 + pct_65older20 + pct_18younger20,
           data = usda))

summary(lm(ffrpth20 ~ ln_pc_snapben17 + povrate21 + fsrpth16 + grocpth16 + metro23 + pct_nhblack20 + pct_hisp20 + pct_65older20 + pct_18younger20,
           data = usda))
summary(lm(ffrpth20 ~ pct_snap17 + povrate21 + fsrpth16 + grocpth16 + metro23 + pct_nhblack20 + pct_hisp20 + pct_65older20 + pct_18younger20,
           data = usda))

usda %>%
  summarise(across(c(pct_snap17, pct_wic17, pct_nslp17, pct_sfsp17),
                   list(n = ~ sum(!is.na(.x)),
                        mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE),
                        min = ~ min(.x, na.rm = TRUE),
                        max = ~ max(.x, na.rm = TRUE))))

summary(lm(ffrpth20 ~ pct_wic17 + povrate21 + fsrpth16 + grocpth16 + metro23 + pct_nhblack20 + pct_hisp20 + pct_65older20 + pct_18younger20,
           data = usda))
summary(lm(ffrpth20 ~ pct_nslp17 + povrate21 + fsrpth16 + grocpth16 + metro23 + pct_nhblack20 + pct_hisp20 + pct_65older20 + pct_18younger20,
           data = usda))
summary(lm(ffrpth20 ~ pct_sfsp17 + povrate21 + fsrpth16 + grocpth16 + metro23 + pct_nhblack20 + pct_hisp20 + pct_65older20 + pct_18younger20,
           data = usda))

# ============================================================================ #
