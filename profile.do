* Open data
use "data/lcf_2023.dta", clear

* Plot
scatter hhcon hhinc, mc(black) mfc(%50) ytitle("Household consumption (£/week)") xtitle("Household income (£/week)") title("Household consumption vs income (LCF, 2023)")
graph export "material/lecture-2/lcf-scatter.png", replace
