set seed 9871

set obs 100
scalar b0 = 2
scalar b1 = 0.5

generate x = runiform()
generate error = rnormal()
generate y = scalar(b0) + scalar(b1)*x + error

twoway (scatter y x) (function y = scalar(b0)+scalar(b1)*x, lcolor(red)), legend(off)
