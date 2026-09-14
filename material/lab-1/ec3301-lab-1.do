* Title: Econometrics Lab 1: Working with data in Stata
* Author: Neil Lloyd
* Date: 31 August 2026

* ============================================================================ *
* Clear all objects in memory
clear all

* Set directory
cd "C:\Users\ndl1\OneDrive - University of St Andrews\Documents\EC3301\website\st-andrews-ec3301\material\lab-1"

* Start log
*cap log close
*log using lab-1.txt, replace text

* Open data
use "shs2023.dta"


* ============================================================================ *
* Managing data

** 1. Remove all unnecessary variables using the command:

keep area RTParea MD20QUIN hhsize hb1 hb509 rent_amt rent_sum mortgage_amt mortgage_sum shared_ownership_sum shared_ownership_amt

** 2. Rename the variable `hb1` `dwell_type` using the `rename` command

rename hb1 dwell_type

** 3. Rename `hb509` `own_grp` 

rename hb509 own_grp

** 4. Label `dwell_type` "Type of dwelling"

label var dwell_type "Type of dwelling"

** 5. Label `own_grp` "Ownership of the dwelling"

label var own_grp "Ownership of the dwelling"

* ============================================================================ *
* Categorical variables

** 1. How many types of dwelling are there in the data?
numlabel, add
tab dwell_type
	
	// The data categorizes the households into 3 dwelling types. Most dwellings are either houses or apartments, with the last group being a "catch-all" for other types. 

** 2. How many households live in rented accommodation? 
tab own_grp
	
	// 3,414 households pay rent, but 83 have a mix of rent and mortgage. Another 104 live rent free. 

** 3. Create a cross-tabulation of the variables `dwell_type` and `own_grp`. How many households rent a flat in the dataset?	
tab dwell_type own_grp

	// 1906 rent a flat, ignoring part owners and rent-free tenants 

** 4. The variable MD20QUIN (Scottish Index of Multiple Deprivation (SIMD), 2020 quintiles) has information about the socio-economic status of the household. Are the observations balanced across quintiles?	
tab MD20QUIN

	// Yes it looks fairly balanced, but not exact. This is likely due to survey weights. 

des MD20QUIN
label list MD20QUIN

label def QUINTILE 1 "Bottom: 0-20%" 2 "Lower: 20-40%" 3 "Middle: 40-60%" 4 "Upper: 60-80%" 5 "Top: 80-100%"
label val MD20QUIN QUINTILE
tab MD20QUIN

* ============================================================================ *
* Continuous vairbales

** 1. What is the average amount money paid as rent by households in the data?
sum rent_amt

	// about £477 per month (see variable label)

** 2. Did you notice that the number of observations is listed as 3,414? Does this match the total number of observations in the dataset? [You can use the `count` command to check this.] 

count

	// 3414 matches the number of people who are renters. 

** 3. Browse the dataset to investigate: `browse rent_amt`. 

br rent_amt

	// There are a lot of "." observations. This Stata's way of coding missing data. 

** 4. Create a frequency table of the categorical variable `rent_sum` to explore why there only 3,414 observations. 

tab rent_sum
tab rent_sum own_grp

	// People who rent have rent_amt values, but some of these are imputed.

** 5. Use the option `, detail` in your summary of both `rent_amt` and `mortgage_amt`. Which has the highest median?

sum rent_amt, detail
sum mortgage_amt, detail

	// mortgage payments have a much higher median. 
	
* ============================================================================ *
* Adding conditions

sum rent_amt if rent_sum==1

** 1. What is the average mortgage payment of households who own their home and provide a mortgage payment? [You can ignore those with `mortgage_sum==3' "Buying with mortgage - amount given by respondent, but not used in imputation routines"] 
tab mortgage_sum
tab mortgage_sum own_grp, m // check that these categories match ownership structure
label list mortgage_sum
sum mortgage_amt if mortgage_sum==1 | mortgage_sum==3

	// £666 per month

** 2. Is the average imputed mortgage payment higher than the average reported payment?

sum mortgage_amt if mortgage_sum==2

	// £654 - so slightly lower

** 3. Using the variable `area`, which city has higher mean and/or median rental payments: Edinburgh or Glasgow? 
	
sum rent_amt if area==1, det
sum rent_amt if area==2, det

	// Edinburgh's average rent of £696 is much higher than Glasgow's of £498

* ============================================================================ *
* Summary statistics

tab MD20QUIN, sum(rent_amt)
table MD20QUIN, stat(mean rent_amt) stat(sd rent_amt) stat(count rent_amt)
tabstat rent_amt, by(MD20QUIN) stat(mean sd count)

** 1. Using the variable `RTParea`, compute a table of average rental and mortgage payments by area.

table RTParea, stat(mean rent_amt) 
table RTParea, stat(mean mortgage_amt)

* ============================================================================ *
* Generate new variables

gen amt_sum = .
sum amt_sum
replace amt_sum = 1 if rent_amt > 0 & rent_amt != .
replace amt_sum = 2 if mortgage_amt > 0 & mortgage_amt != .
replace amt_sum = 3 if shared_ownership_amt > 0 & shared_ownership_amt != .

** 1. Check that those with rental, mortgage, and shared payments are mutually exclusive. 

tab amt_sum, sum(rent_amt)
tab amt_sum, sum(mortgage_amt)
tab amt_sum, sum(shared_ownership_amt)

tab mortgage_sum rent_sum

** 2. Create a new variable called `payment` equal to the sum of `rent_amt`, `mortgage_amt`, and `shared_ownership_amt`. [Hint: you will not be able to do this by adding the variables together.]
gen payment = rent_amt if amt_sum==1
replace payment = mortgage_amt if amt_sum==2
replace payment = shared_ownership_amt if amt_sum==3

** 3. Compute the mean of `payment` and the number of observations for which `payment==.`. Who has a missing value of `payment`? How should these values be coded?
	
sum payment
count if payment==.
	
tab own_grp if payment==.	

	// These are people who live rent free or own their home outright. It's unclear whether these values should be =0. 
	
** 4. Create a new variable - `pc_payment` - equal to the total monthly payment divided by household size. Label the variable "Per capita monthly payment for housing."
sum hhsize
gen pc_payment = payment/hhsize
label var pc_payment "Per capita monthly payment for housing."

** 5. Which `area` has the highest average per capita payment?

tab area, sum(pc_payment)
	

* ============================================================================ *
* Basic graphs

hist rent_amt 
graph bar, over(RTParea) 
graph bar (mean) rent_amt, over(RTParea)

** 1. The labels of the above graph do not display very well. Navigate back to the 'Bar chart' window. Under the 'Categories' tab, you will see an option to edit the 'Properties' of the categories. See if you can change the angle of labels so that they are displayed at a $45^\circ$ angle. 

graph bar (mean) rent_amt, over(RTParea, label(angle(forty_five)))

** 2. Create a graph that displays the average rent and mortgage paid by households in each of the `RTParea`. Edit the graph so that it has an informative y-axis title and give the graph a title. 

graph bar (mean) mortgage_amt (mean) rent_amt, over(RTParea, label(angle(forty_five))) ytitle("Monthly payment") title("Average mortgage and rental payment by RTP area") legend(order(1 "Mortgage" 2 "Rent"))

** 3. Create a single graph showing 5 separate histograms of rental payments for each `MD20QUIN` group. Use the 'By' tab in the 'Graphics>Histogram' window to do this. You can also select the option 'Add a graph of totals' to make it an even 6 histograms.	
	
hist rent_amt, by(MD20QUIN, total)	

* ============================================================================ *
* Exporting graphs

** 1. Save the last graph you created in the same folder as the data and .do file using the name `hist_rent_by_quintile.png`

*graph export hist_rent_by_quintile.png, replace
	
* ============================================================================ *
* Close log
*log close