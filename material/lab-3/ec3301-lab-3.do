* Title: Econometrics Lab 3: RCT Replication in Stata
* Author: Neil Lloyd
* Date: 25 October 2026

* ============================================================================ *
* Clear all objects in memory
clear all

* Set directory
cd "[INSERT FILE PATH]"

* Start log
*cap log close
*log using lab-3.txt, replace text

* Open data
use "dizonross2019.dta"

** Create between-sibling score gap:

bys hhid (child): gen ave_12 = score_ave[1]-score_ave[2]

* ============================================================================ *
* Assignment

** How many schools are there?
codebook school

** How many households?
duplicates report hhid

** 
tab treat, m

	// almost eactly 50-50
	
** Replicate the first four rows of Table 1, the balance table

dtable resp_female resp_decides resp_age par_educ, by(treat, tests)

** Replicate the first four rows of Table 1, the balance table
	
dtable grade child_age child_female attend_bl, by(treat, tests)
	
* ============================================================================ *
* Graphical analysis

** Replicate panel A of Figure 3

graph bar (mean) belief_err_bl, over(treat) blabel(bar)

** Replicate Panel C

graph bar (mean) belief_err_end, over(treat) blabel(bar)

** Replicate panel B

twoway (lfit belief_bl score_ave if treat==0, lc(blue))  (lfit belief_bl score_ave if treat==1, lp(dash) lc(green)) (function y = x, range(0 100) lc(gs8)), ylabel(0(20)100) xlabel(0(20)100) legend(order(1 "Control" 2 "Treat") ring(0) pos(5))

** Replicate panel D

twoway (lfit belief_end score_ave if treat==0, lc(blue))  (lfit belief_end score_ave if treat==1, lp(dash) lc(green)) (function y = x, range(0 100) lc(gs8)), ylabel(0(20)100) xlabel(0(20)100) legend(order(1 "Control" 2 "Treat") ring(0) pos(5))

* ============================================================================ *
* ATE

** Estimate ATE
ttest belief_end, by(treat)
ttest belief_end, by(treat) unequal
	
** Estimate ATE using linear regression
reg belief_end treat 

** Estimate ATE using linear regression
reg belief_end treat, vce(robust)
reg belief_end treat, vce(hc2)



* ============================================================================ *
* Regression adjustment

** Recode wb_math and wb_engl
recode wb_math (-1 = 0) (0 = 100) (1 = 200)
recode wb_engl (-1 = 0) (0 = 100) (1 = 200)


** Replicate Table A.1, with regression adjustment; exlcuding school FEs
est clear

reg belief_end treat par_educ resp_literate ave_12 score_ave child_female i.grade resp_female, vce(robust)
	est store model1

reg wb_math treat par_educ resp_literate ave_12 score_ave child_female i.grade resp_female, vce(robust)
	est store model2
	
reg wb_engl treat par_educ resp_literate ave_12 score_ave child_female i.grade resp_female, vce(robust)
	est store model3
	
reg lnwtp_math_engl treat par_educ resp_literate score_ave ave_12 child_female i.grade resp_female, vce(robust)
	est store model4	

reg enrolled treat par_educ resp_literate ave_12 score_ave child_female i.grade resp_female, vce(robust)
	est store model5
	
reg ln_educ_exp_end treat par_educ resp_literate ave_12 score_ave child_female i.grade resp_female, vce(robust)
	est store model6
	
reg attend_admin treat par_educ resp_literate ave_12 score_ave child_female i.grade resp_female, vce(robust)
	est store model7

etable, est(model1 model2 model3 model4 model5 model6 model7) column(depvar) keep(treat) eqrecode(belief_end = xb wb_math = xb wb_engl = xb lnwtp_math_engl = xb enrolled = xb ln_educ_exp_end = xb attend_admin = xb) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))


** Estimate simple model controlling for school FEs
areg belief_end treat par_educ resp_literate  ave_12 child_female i.grade resp_female, absorb(school)

** Clustered SEs
areg belief_end treat par_educ resp_female resp_literate ave_12 score_ave child_female i.grade , absorb(school) vce(cluster hhid)


** Collect regression results and create a table

est clear
qui reg belief_end treat, vce(robust) 
	est store model1
	
qui reg belief_end treat par_educ resp_female resp_literate ave_12 score_ave child_female i.grade, vce(robust)
	est store model2

qui areg belief_end treat par_educ resp_female resp_literate ave_12 score_ave child_female i.grade, absorb(school) vce(robust)
	est store model3
	
qui areg belief_end treat par_educ resp_female resp_literate ave_12 score_ave child_female i.grade, absorb(school) vce(cluster hhid)
	est store model4

	
etable, est(model1 model2 model3 model4) keep(treat) column(estimates) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***")

* ============================================================================ *
* Interacted model

** Estimate interacted model

reg belief_end i.treat##c.score_ave par_educ resp_literate ave_12 child_female i.grade resp_female

** Replicate the results in models 1-4

est clear

reg belief_end i.treat##c.score_ave par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model1
	
reg wb_math i.treat##c.score_ave par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model2
	
reg wb_engl i.treat##c.score_ave par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model3
	
reg lnwtp_math_engl i.treat##c.score_ave par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model4	
	
etable, est(model1 model2 model3 model4) column(depvar) keep(1.treat 1.treat#c.score_ave score_ave) eqrecode(belief_end = xb wb_math = xb wb_engl = xb lnwtp_math_engl = xb) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))
	
  	
** Replicate Table 3 (Panel A)

est clear
  
reg enrolled i.treat##c.score_ave par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model1
	
reg ln_educ_exp_end i.treat##c.score_ave par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model2
	
reg attend_admin i.treat##c.score_ave par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model3
	
etable, est(model1 model2 model3) column(depvar) keep(1.treat 1.treat#c.score_ave score_ave) eqrecode(enrolled = xb ln_educ_exp_end = xb attend_admin = xb) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))

** Replicate Table 3 (Panel B)
sum score_ave, det
return list
gen above_median = score_ave>r(p50) if !missing(score_ave)

est clear
  
reg enrolled i.treat##i.above_median par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model1
	
reg ln_educ_exp_end i.treat##i.above_median par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model2
	
reg attend_admin i.treat##i.above_median par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model3
	
etable, est(model1 model2 model3) column(depvar) keep(1.treat 1.treat#1.above_median 1.above_median) eqrecode(enrolled = xb ln_educ_exp_end = xb attend_admin = xb) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4f)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))

* ============================================================================ *
* Heterogeneity

** Replicate Table 4 (Panel B)

est clear

reg belief_end i.treat##c.score_ave##c.par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model1
	
reg wb_math i.treat##c.score_math##c.par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model2
	
reg wb_engl i.treat##c.score_engl##c.par_educ resp_literate  ave_12 child_female i.grade resp_female, vce(robust)
	est store model3
etable, est(model1 model2 model3) column(depvar) eqrecode(belief_end = xb wb_math = xb wb_engl = xb) novarlabel cstat(_r_b, nformat(%5.4f)) cstat(_r_se, nformat(%5.4g)) showstars showstarsnote stars(.1 "*" .05 "**" .01 "***") mstat(N) mstat(r2, nformat(%4.3f))

** 

*log close