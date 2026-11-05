* Title: Econometrics Lab 4: Observational Studies in Stata
* Author: Neil Lloyd
* Date: 1 November 2026

* ============================================================================ *
* Clear all objects in memory
clear all

* Set directory
cd "[YOUR FILE PATH]"

* Start log
*cap log close
*log using lab-3.txt, replace text

* Open data
use "yelp_restaurants.dta"

* ============================================================================ *
* Sample selection 

** Keep only open restaurants

tab is_open, m
keep if is_open==1

** What type of variable is state, and how many values does it take on?

describe state
codebook state

	// 16 states

** Keep restaraurants in one state, for which there is a reasonable sample size. 

tab state, m
keep if state=="FL"

* ============================================================================ *
* Assignment


** What share of resturants in the sample offer delivery

tab delivery

** Which category of food has the highest propensity to offer delivery

foreach var of varlist cat_fastfood-cat_bar{
	dis "`var'"
	sum delivery if `var'==1
}

	// for florida it's fastfood


** Create a table that shows the share of restraurants registered under each category (and the number of categories) by delivery status. Include a test of significance. 

dtable cat_fastfood cat_pizza cat_burgers cat_sandwich cat_chinese cat_mexican cat_italian cat_japanese cat_breakfast cat_cafe cat_bar n_categories, by(delivery, tests)

** Create a bar chart showing the distribution of price_range by delivery
graph bar, over(price_range) by(delivery)

** Create a histogram of weekly_hours 
histogram weekly_hours, by(delivery)


* ============================================================================ *
* Correlation

** Create a table that shows the mean stars by whether a restaurant offers delivery

tab delivery, sum(stars)

** Plot
lab def DEL 0 "Delivery not offered" 1 "Delivery offered"
lab val delivery DEL
graph bar (mean) stars, over(delivery) ytitle(Stars) title(Average Yelp stars by delivery status)

** Test whether restaraurants that offer delivery have a statistically different average number of stars to those who do not. 

ttest stars, by(delivery) unequal

** Same for review_count
tab delivery, sum(review_count)
graph bar (mean) review_count, over(delivery) ytitle(Reviews) title(Average number of Yelp reviews by delivery status)

ttest review_count, by(delivery) unequal

** By cat_fastfood
lab def CAT_FF 0 "non-fastfood" 1 "fastfood"
lab val cat_fastfood CAT_FF
graph bar (mean) stars, over(delivery) by(cat_fastfood, note("")) ytitle("Stars")

** By price_range
lab def PRICE 1 "Least expensive" 2 "Less expensive" 3 "More expensive" 4 "Most expensive"
lab val price_range PRICE
graph bar (mean) review_count, over(delivery) by(price_range, row(2) note("") ) ytitle(Reviews)

* ============================================================================ *
* Regression analysis of stars and review_count

** Estimate a simple regression with HC1 robust SEs. Is there a significant relationship between 
reg stars delivery, vce(robust)
	est store model1
	
** Add controls that you think are good controls. Justify your choice

reg stars delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain, vce(robust)
	est store model2

reg stars delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain i.price_range alcohol i.noise, vce(robust)
	est store model3

etable, est(model1 model2 model3) column(estimates) keep(delivery) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))

** Fix changes in observations

qui reg stars delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain i.price_range alcohol i.noise, vce(robust)
	gen sample = e(sample)
	
qui reg stars delivery if sample==1, vce(robust)
	est store model1
	
qui reg stars delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain if sample==1, vce(robust)
	est store model2

qui reg stars delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain i.price_range alcohol i.noise if sample==1, vce(robust)
	est store model3

etable, est(model1 model2 model3) column(estimates) keep(delivery) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))
	
** For review_count	

qui reg review_count delivery if sample==1, vce(robust)
	est store model1
	
qui reg review_count delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain if sample==1, vce(robust)
	est store model2

qui reg review_count delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain i.price_range alcohol i.noise if sample==1, vce(robust)
	est store model3

etable, est(model1 model2 model3) column(estimates) keep(delivery) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))

	
** Estimate a simple regression with HC1 robust SEs. Is there a significant relationship between 
gen ln_reviews = ln(review_count)
qui reg ln_reviews delivery if sample==1, vce(robust)
	est store model1
	
qui reg ln_reviews delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain if sample==1, vce(robust)
	est store model2

qui reg ln_reviews delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain i.price_range alcohol i.noise if sample==1, vce(robust)
	est store model3

etable, est(model1 model2 model3) column(estimates) keep(delivery) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))


** RESET test
reg ln_reviews delivery cat_cafe cat_bar cat_breakfast cat_fastfood n_categories weekly_hours chain i.price_range alcohol i.noise if sample==1, vce(robust)

	estat ovtest
	
	// reject



