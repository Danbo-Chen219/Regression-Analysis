////////////////////////////////////////////////////////////////////////////////
////////////////////////////   SET UP THE FOLDER   /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Set working directory and the other folders:

clear all


///////////////////////////        PROBLEM 1         ///////////////////////////


cd "D:\Documents\PHD\PHD Course\Econometrics\HW2"
import delimited "hprice1.csv", clear

/// 1.A
reg price sqrft bdrms

reg sqrft bdrms
predict e, resid

reg price e

/// 1.B

reg bdrms sqrft
gen delta_1 = _b[sqrft]

reg price sqrft
gen beta_1_tilde = _b[sqrft]

reg price sqrft bdrms
gen  beta_1_hat = _b[sqrft]
gen	 beta_2_hat = _b[bdrms]

gen error = beta_1_tilde - beta_1_hat - beta_2_hat*delta_1


///////////////////////////        PROBLEM 3         ///////////////////////////

clear all

import delimited "wage2.csv", clear

// A
eststo: ivregress 2sls lwage exper tenure (educ = sibs feduc meduc), first

// B
eststo: reg educ exper tenure sibs feduc meduc
predict educ_hat
eststo: reg lwage educ_hat exper tenure 

// For the Table
reg educ exper tenure sibs feduc meduc
reg lwage educ_hat exper tenure 


// C
eststo: reg educ  sibs feduc meduc
predict educ_tilde
eststo: reg lwage educ_tilde exper tenure 

// For the Table
reg educ  sibs feduc meduc
reg lwage educ_tilde exper tenure 

// D
eststo: reg lwage educ exper tenure 

esttab, se ar2 replace

