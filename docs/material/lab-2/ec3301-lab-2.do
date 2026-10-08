* Title: Econometrics Lab 2: Linear Regression in Stata
* Author: Neil Lloyd
* Date: 25 September 2026

* ============================================================================ *
* Clear all objects in memory
clear all

* Set directory
cd "[INSERT YOUR WORKING DIRECTORY FOR THIS LAB]"

* Start log
*cap log close
*log using lab-2.txt, replace text

* Open data
use "shs2023.dta"

* ============================================================================ *
* Pre-amble from Lab 1

rename hb1 dwell_type
label var dwell_type "Type of dwelling"

rename hb509 own_grp
label var own_grp "Ownership of the dwelling"

label def QUINTILE 1 "Bottom: 0-20%" 2 "Lower: 20-40%" 3 "Middle: 40-60%" 4 "Upper: 60-80%" 5 "Top: 80-100%"
label val MD20QUIN QUINTILE

gen amt_sum = .
replace amt_sum = 1 if rent_amt > 0 & rent_amt != .
replace amt_sum = 2 if mortgage_amt > 0 & mortgage_amt != .
replace amt_sum = 3 if shared_ownership_amt > 0 & shared_ownership_amt != .

gen payment = .
replace payment = rent_amt if amt_sum==1
replace payment = mortgage_amt if amt_sum==2
replace payment = shared_ownership_amt if amt_sum==3

* ============================================================================ *
* Create variables

** Energy
tab htcostsum_1

tab htcostsum_1, sum(htcostamt_1)

*egen energy_cost = rowtotal(htcostamt_1 htcostamt_2 htcostamt_3 htcostamt_4 htcostamt_5 htcostamt_6 htcostamt_7 htcostamt_8 htcostamt_9), missing
*egen energy_cost = rowtotal(htcostamt_1-htcostamt_9), missing // when using this approach check the order of variables in the variable list first
egen energy_cost = rowtotal(htcostamt_*), missing 

count if energy_cost ==.
count if energy_cost==0
count if energy_cost>0 & !missing(energy_cost)

sum energy_cost, det

replace energy_cost = energy_cost/12

label var energy_cost "Household energy costs (monthly £s)"

** Housing
rename payment housing_cost
label var housing_cost "Household housing costs (monthly £s)"

compare housing_cost hcost_amt

*br mortgage_amt rent_amt shared_ownership_amt hcost_amt housing_cost

count if housing_cost ==.
count if housing_cost==0
count if housing_cost>=0

sum housing_cost, det

tab own_grp
tab own_grp, sum(housing_cost)

hist housing_cost

** Income
sum annetinc
tab incsum, m

tab incsum, sum(annetinc)

gen income = annetinc/12
label var income "Household income (monthly £s)"

count if income ==.
count if income==0
count if income>=0

sum income, det

keep if !missing(energy_cost) & !missing(housing_cost) & !missing(income)

* ============================================================================ *
* Simple regression

twoway (scatter energy_cost income) (lfit energy_cost income), legend(off)

reg energy_cost income
predict energy_hat

twoway (scatter energy_cost income) (lfit energy_cost income), ylabel(0(500)1500) legend(off) name(energy, replace) nodraw ytitle(Energy costs)
twoway (scatter housing_cost income) (lfit housing_cost income), ylabel(0(500)1500)  legend(off) name(housing, replace) nodraw ytitle(Housing costs)
graph combine energy housing

gen energy_share = energy_cost/(energy_cost + housing_cost)	
sum energy_share

gen ln_income = ln(income)
reg energy_share ln_income

graph bar (mean) energy_share, over(area, label(angle(forty_five))) ytitle("Energy share")

rename hc4 bedrooms
reg energy_share ln_income hhsize bedrooms i.own_grp i.dwell_type i.area

reg energy_share ln_income hhsize bedrooms ib2.own_grp i.dwell_type ib3.area


* ============================================================================ *
* USDA 
* Source: https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads

import delimited "usda2025.csv", clear
	
gen ln_pc_snapben17 = ln(pc_snapben17)

corr ln_pc_snapben17 snapspth17 pct_snap17

reg ffrpth20 ln_pc_snapben17 
reg ffrpth20 snapspth17 
reg ffrpth20 pct_snap17 

corr snapspth17 grocpth16

reg ffrpth20 snapspth17 povrate21 fsrpth16 grocpth16  metro23 pct_nhblack20 pct_hisp20 pct_65older20 pct_18younger20 

reg ffrpth20 ln_pc_snapben17 povrate21 fsrpth16 grocpth16  metro23 pct_nhblack20 pct_hisp20 pct_65older20 pct_18younger20 
reg ffrpth20 pct_snap17 povrate21 fsrpth16 grocpth16  metro23 pct_nhblack20 pct_hisp20 pct_65older20 pct_18younger20 

sum pct_snap17 pct_wic17 pct_nslp17 pct_sfsp17

reg ffrpth20 pct_wic17 povrate21 fsrpth16 grocpth16  metro23 pct_nhblack20 pct_hisp20 pct_65older20 pct_18younger20 
reg ffrpth20 pct_nslp17 povrate21 fsrpth16 grocpth16  metro23 pct_nhblack20 pct_hisp20 pct_65older20 pct_18younger20 
reg ffrpth20 pct_sfsp17 povrate21 fsrpth16 grocpth16  metro23 pct_nhblack20 pct_hisp20 pct_65older20 pct_18younger20 

* ============================================================================ *
* Close log
*log close