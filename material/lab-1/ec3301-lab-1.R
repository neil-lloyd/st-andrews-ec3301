# Title: Econometrics Lab 1: Working with data in R
# Author: Neil Lloyd
# Date: 31 August 2026

# ============================================================================ #
# Load libraries
library("haven")
library("labelled")
library("dplyr")
library("ggplot2")

# Clear all objects from the R environment
rm(list = ls())

# Set working directory
setwd("C:/Users/ndl1/OneDrive - University of St Andrews/Documents/EC3301/website/st-andrews-ec3301/material/lab-1")

# Open data
shs_csv <- read.csv("shs2023.csv")
shs_dta <- read_dta("shs2023.dta")

str(shs_dta)

# ============================================================================ #
# Managing data with `dplyr`

# 1. Create a new `clean' dataframe that has a few select variables
clean <- select(shs_dta, area, RTParea, MD20QUIN, hhsize, hb1, hb509, rent_amt, rent_sum, mortgage_amt, mortgage_sum, shared_ownership_sum, shared_ownership_amt)

# 2. Rename the variable `hb1` to `dwell_type` using the `rename()` function. 
clean <- rename(clean, dwell_type = hb1)

# 3. Rename `hb509` to `own_grp` in the clean file.

clean <- rename(clean, own_grp = hb509)

# 4. Label `dwell_type` "Type of dwelling"

var_label(clean$dwell_type) <- "Type of dwelling"

# 5. Label `own_grp` "Ownership of the dwelling"

var_label(clean$own_grp) <- "Ownership of the dwelling"

# ============================================================================ #
# Using pipes

clean <- shs_dta %>%
  select(area, RTParea, MD20QUIN, hhsize, hb1, hb509, rent_amt, rent_sum, mortgage_amt, mortgage_sum, shared_ownership_sum, shared_ownership_amt) %>%
  rename(dwell_type = hb1, own_grp = hb509)

  var_label(clean$dwell_type) <- "Type of dwelling"
  var_label(clean$own_grp) <- "Ownership of the dwelling"


# ============================================================================ #
# Variables

typeof(clean$MD20QUIN)
typeof(shs_csv$MD20QUIN)

# ============================================================================ #
# Categorical variables

# 1. How many types of dwelling are there in the data?

table(clean$dwell_type)

clean %>% count(dwell_type)

clean %>%
  group_by(dwell_type) %>%
  summarise(n = n(), .groups = "drop")

clean %>% 
  count(dwell_type) %>%
  mutate(prop = n / sum(n))

# 2. How households live in rented accommodation?
  
table(clean$own_grp)

# 3. Create a cross-tabulation of the variables `dwell_type` and `own_grp`. How many households rent a flat in the dataset?
  
clean %>%
  group_by(dwell_type, own_grp) %>%
  summarise(n = n(), .groups = "drop")

# 4. The variable `MD20QUIN` (Scottish Index of Multiple Deprivation (SIMD), 2020 quintiles) has information about the socio-economic status of the household. Are the observations balanced across quintiles? 

table(clean$MD20QUIN)

val_labels(clean$MD20QUIN)

val_labels(clean$MD20QUIN) <- c("Bottom: 0-20%" = 1, "Lower: 20-40%" = 2, "Middle: 40-60%" = 3, "Upper: 60-80%" = 4, "Top: 80-100%" = 5)
clean %>% 
  count(MD20QUIN) %>%
  mutate(prop = n / sum(n))


# ============================================================================ #
# Continuous variables

# 1. What is the average amount money paid as rent by households in the data?
summary(clean$rent_amt)

# 2. The number of observations in the dataframe is 10,496? Does this match the total number of observations used to compute the mean? [Hint: check the first table or add `n_valid = sum(!is.na(rent_amt))` to the `dplyr` version.] 

clean %>%
  summarise(
    n = n(),
    n_valid = sum(!is.na(rent_amt)),
    mean = mean(rent_amt, na.rm = TRUE),
    sd = sd(rent_amt, na.rm = TRUE),
    min = min(rent_amt, na.rm = TRUE),
    max = max(rent_amt, na.rm = TRUE)
  )

# 3. Create a frequency table of the categorical variable `rent_sum` to explore why there only 3,414 observations. 

table(clean$rent_sum)
val_labels(clean$rent_sum)

# 4. Create a table which also includes the $25^{th}$, $50^{th}$, and $75^{th}$ percentiles of the distribution. 

clean %>%
  summarise(
    n = n(),
    n_valid = sum(!is.na(rent_amt)),
    p25 = quantile(rent_amt, 0.25, na.rm = TRUE),
    p50 = quantile(rent_amt, 0.50, na.rm = TRUE),
    p75 = quantile(rent_amt, 0.75, na.rm = TRUE)
  )


# ============================================================================ #
# Adding conditions

clean_rent <- clean[clean$rent_sum == 1, ]
clean_rent <- subset(clean, rent_sum == 1)
summary(clean_rent$rent_amt)

clean %>%
  filter(rent_sum==1) %>%
  summarise(
    n = n(),
    mean = mean(rent_amt, na.rm = TRUE),
    sd = sd(rent_amt, na.rm = TRUE),
    min = min(rent_amt, na.rm = TRUE),
    max = max(rent_amt, na.rm = TRUE)
  )

# 1. What is the average mortgage payment of households who own their home and provide a mortgage payment? [You can ignore those with `mortgage_sum==3' "Buying with mortgage - amount given by respondent, but not used in imputation routines"]

clean %>%
  filter(mortgage_sum==1) %>%
  summarise(
    n = n(),
    mean = mean(mortgage_amt, na.rm = TRUE),
    sd = sd(mortgage_amt, na.rm = TRUE),
    min = min(mortgage_amt, na.rm = TRUE),
    max = max(mortgage_amt, na.rm = TRUE)
  )

# 2. Is the average imputed mortgage payment higher than the average reported payment?
  
clean %>%
  filter(mortgage_sum==2) %>%
  summarise(
    n = n(),
    mean = mean(mortgage_amt, na.rm = TRUE),
    sd = sd(mortgage_amt, na.rm = TRUE),
    min = min(mortgage_amt, na.rm = TRUE),
    max = max(mortgage_amt, na.rm = TRUE)
  )

#  3. Using the variable `area`, which city has higher mean and/or median rental payments: Edinburgh OR Glasgow? 
  
clean %>%
  filter(area %in% c("Edinburgh", "Glasgow")) %>%
  group_by(area) %>%
  summarise(
    n = n(),
    mean = mean(rent_amt, na.rm = TRUE),
    p50 = quantile(rent_amt, 0.50, na.rm = TRUE),
    .group = "drop"
  )

# ============================================================================ #
# Summary statistics

by(clean$rent_amt, clean$MD20QUIN, summary)
tapply(clean$rent_amt, clean$MD20QUIN, mean, na.rm = TRUE)
clean %>%
  group_by(MD20QUIN) %>%
  summarise(
    n = n(),
    mean = mean(rent_amt, na.rm = TRUE),
    sd = sd(rent_amt, na.rm = TRUE),
    min = min(rent_amt, na.rm = TRUE),
    max = max(rent_amt, na.rm = TRUE),
    .group = "drop"
  )

# 1. Using the variable `RTParea`, compute a table of average rental and mortgage payments by area. 

clean %>%
  group_by(RTParea) %>%
  summarise(
    mean_rent = mean(rent_amt, na.rm = TRUE),
    mean_mortgage = mean(mortgage_amt, na.rm = TRUE),
    .group = "drop"
  )

# ============================================================================ #
# Generate new variables

clean$amt_sum <- NA
summary(clean$amt_sum)

clean$amt_sum[clean$rent_amt>0] <- 1
clean$amt_sum[clean$mortgage_amt>0] <- 2
clean$amt_sum[clean$shared_ownership_amt>0] <- 3
clean %>%
  group_by(amt_sum) %>%
  summarise(n = n(), .groups = "drop")

clean <- clean %>%
  mutate(
    amt_sum = case_when(
      shared_ownership_amt > 0 ~ 3,
      mortgage_amt > 0 ~ 2,
      rent_amt > 0 ~ 1,
      TRUE ~ NA_real_
    )
  )
clean %>%
  group_by(amt_sum) %>%
  summarise(n = n(), .groups = "drop")

# 1. Check that those with rental, mortgage, and shared payments are mutually exclusive. 

clean %>%
  group_by(amt_sum) %>%
  summarise(
    n_rent = sum(rent_amt > 0, na.rm = TRUE),
    n_mort = sum(mortgage_amt > 0, na.rm = TRUE),
    n_shar = sum(shared_ownership_amt > 0, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Create a new variable called `payment` equal to the sum of `rent_amt`, `mortgage_amt`, and `shared_ownership_amt`. [Hint: you will not be able to do this by adding the variables together. Try the`rowSums()` function from base R.]

clean$payment <- rowSums(clean[, c("rent_amt", "mortgage_amt", "shared_ownership_amt")], na.rm = TRUE)


# 3. Compute the mean of `payment` and the number of observations for which `payment==.`. Who has a missing value of `payment`? How should these values be coded?
  
summary(clean$payment)
sum(is.na(clean$payment))


# 4. Create a new variable - `pc_payment` - equal to the total monthly payment divided by household size. Label the variable "Per capita monthly payment for housing."

clean <- clean %>%
  mutate(pc_payment = payment/hhsize)

# 5. Which `area` has the highest average per capita payment?
  
clean %>%
  group_by(area) %>%
  summarise(
    n = n(), 
    mean = mean(pc_payment, na.rm = TRUE), 
    .groups = "drop"
  )

clean %>%
  filter(payment>0) %>%
  group_by(area) %>%
  summarise(
    n = n(), 
    mean = mean(pc_payment, na.rm = TRUE), 
    .groups = "drop"
  )

# ============================================================================ #
# Basic R plots

hist(clean$rent_amt)

barplot(table(clean$RTParea),
        main = "Frequency by RTP area",
        xlab = "RTP Area",
        ylab = "Frequency",
        col = "lightblue",
        border = "white")

barplot(prop.table(table(clean$RTParea)),
        main = "Proportion by RTP area",
        xlab = "RTP Area",
        ylab = "Proportion",
        col = "lightblue",
        border = "white")

means <- tapply(clean$rent_amt, clean$RTParea, mean, na.rm = TRUE)
barplot(means,
        main = "Mean Rental Payment by RTP Area",
        xlab = "RTP Area",
        ylab = "Mean Rental Payment (Weekly)",
        col = "lightblue",
        border = "white")


# ============================================================================ #
# Using `ggplot2`
means <- clean %>%
  group_by(RTParea) %>%
  summarise(mean_rent = mean(rent_amt, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = RTParea, y = mean_rent)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(
    title = "Mean Rental Payment by RTP Area",
    x = "RTP Area",
    y = "Mean Rental Payment (Weekly)"
  )

means <- clean %>%
  mutate(RTParea = as_factor(RTParea)) %>%
  group_by(RTParea) %>%
  summarise(mean_rent = mean(rent_amt, na.rm = TRUE), .groups = "drop")

ggplot(means, aes(x = RTParea, y = mean_rent)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(
    title = "Mean Rental Payment by RTP Area",
    x = "RTP Area",
    y = "Mean Rental Payment (Weekly)"
  ) 

# 1. The labels of the above graph do not display very well. See if you can change the angle of labels so that they are displayed at a $45^\circ$ angle. 

ggplot(means, aes(x = RTParea, y = mean_rent)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(
    title = "Mean Rental Payment by RTP Area",
    x = "RTP Area",
    y = "Mean Rental Payment (Weekly)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Create a graph that displays the average rent and mortgage paid by households in each of the `RTParea`. Edit the graph so that it has an informative y-axis title, legend, and title. 

library("tidyr")
means <- clean %>%
  mutate(RTParea = as_factor(RTParea)) %>%
  group_by(RTParea) %>%
  summarise(
    mean_rent = mean(rent_amt, na.rm = TRUE),
    mean_mort = mean(mortgage_amt, na.rm = TRUE),
    .groups = "drop")

means_long <- means %>%
  pivot_longer(
    cols = c(mean_rent, mean_mort),
    names_to = "payment_type",
    values_to = "mean_value"
  ) %>%
  mutate(payment_type = recode(payment_type, mean_rent = "Rent", mean_mort = "Mortgage"))

ggplot(means_long, aes(x = RTParea, y = mean_value, fill = payment_type)) +
  geom_col(position = "dodge", color = "white") +
  labs(
    title = "Mean Rent and Mortgage Payment by RTP Area",
    x = "RTP Area",
    y = "Mean Payment (Weekly)",
    fill = "Payment Type"
  )

# 3. Create a single graph showing 5 separate histograms of rental payments for each `MD20QUIN` group. Use the `facet_wrap()` function within `ggplot()` to do this. 

ggplot(clean, aes(x = rent_amt)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  facet_wrap(~ MD20QUIN) +
  labs(
    title = "Distribution of Rental Payments by MD20 Quintile",
    x = "Rental Payment (Weekly)",
    y = "Count"
  )

# ============================================================================ #
# Exporting graphs
myplot <- ggplot(means, aes(x = RTParea, y = mean_rent)) +
  geom_col(fill = "lightblue", color = "white") +
  labs(
    title = "Mean Rental Payment by RTP Area",
    x = "RTP Area",
    y = "Mean Rental Payment (Weekly)"
  ) 

#ggsave("file_name.png",
#       width = 9, height = 6,
#       plot = myplot)

# 1. Save the last graph you created in the same folder as the data and `.do` file using the name "hist_rent_by_quintile.png"
histplot <- ggplot(clean, aes(x = rent_amt)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  facet_wrap(~ MD20QUIN) +
  labs(
    title = "Distribution of Rental Payments by MD20 Quintile",
    x = "Rental Payment (Weekly)",
    y = "Count"
  )


#ggsave("hist_rent_by_quintile.png",
#       width = 9, height = 6,
#       plot = histplot)