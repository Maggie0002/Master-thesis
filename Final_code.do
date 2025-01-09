*Rename
rename total_meanlight NTL
* set control variable
global $controlFI percentFC percentFO
global $controlSolow percentasset percentHS pop
*modify pop
replace pop = pop / 100
*solow 
global controlSolow pop percentasset percentHS
*foreign invited
global controlFI percentFC percentFO
*Initial Year
sort city_id year 
bysort city_id: gen NTL_2001 = NTL if year == 2001
bysort city_id: replace NTL_2001 = NTL_2001[_n-1] if missing(NTL_2001)
* Generate binary indicator that indicates treatment status by year
gen PFTZ = (year >= FirstSetYear)
label variable PFTZ "Treatment status by year (Yes = 1, No = 0)"
sort city_id  province_id year PFTZ
*Generate port-city
gen port_city = 0  
replace port_city = 1 if city_id == 310000 | city_id == 330200 | city_id == 440300 | ///
  city_id == 370200 | city_id == 440100 | city_id == 120000 | city_id == 350200 | ///
    city_id == 320585 | city_id == 371100 | city_id == 450700 | city_id == 320700 | ///
    city_id == 210800 | city_id == 210200 | city_id == 370600 | city_id == 441900 | ///
    city_id == 350100 | city_id == 320100 | city_id == 330400 | city_id == 130200 | ///
    city_id == 320600 | city_id == 350500 | city_id == 210700 | city_id == 310110 | ///
    city_id == 440500
list city_id port_city if port_city == 1
*Generate SEZ-city
gen sez_city = 0  
replace sez_city = 1 if city_id == 440300 | city_id == 440400 | city_id == 440500 | city_id == 350200 | city_id == 460100 | city_id == 460100 | city_id == 460200  | city_id == 460300  | city_id == 460400
list city_id sez_city if sez_city == 1
* Generate time to treatment indicator
gen time_to_treatment = year -FirstSetYear
label variable time_to_treatment "Time to treatment"
sort  province_id city_id year PFTZ NTL

* generate economic_belt
gen economic_belt = ""
* east
replace economic_belt = "east" if inlist(province_id, 110000, 120000, 130000, 310000, 320000, 330000, 350000, 370000, 440000, 460000)

* middle 
replace economic_belt = "middle" if inlist(province_id, 140000, 340000, 360000, 410000, 420000, 430000)

*west 
replace economic_belt = "west" if inlist(province_id, 150000, 450000, 500000, 510000, 520000, 530000, 540000, 610000, 620000, 630000, 640000, 650000)

* Northeast
replace economic_belt = "Northeast" if inlist(province_id, 210000, 220000, 230000)

list province_id economic_belt if economic_belt != ""

*Balanced dataset
xtset city_id year

* Descriptive statistics of variables.
sum NTL PFTZ pop percentasset percentHS percentFC percentFO NTL_2001
reg NTL PFTZ pop percentasset percentHS percentFC percentFO NTL_2001
vif
* Visualize groups and treated units
panelview NTL PFTZ, i(city_id) t(year) type(treat)  prepost bytiming xtitle("Year") ytitle("Number of cities") legend(position(6)) collapsehistory displayall  

*TWFE regression model 
reghdfe NTL PFTZ, absorb(city_id year) cluster(city_id)
eststo model1
reghdfe NTL PFTZ $controlSolow, absorb(city_id year) cluster(city_id)
eststo model2
reghdfe NTL PFTZ $controlSolow $controlFI, absorb(city_id year) cluster(city_id)
eststo model3

esttab model1 model2 model3 

esttab model1 model2 model3 using results_table.tex, replace ///
    b(%9.3f) se star(* 0.1 ** 0.05 *** 0.01) ///
    title("Average effect on NTL with Fixed Effects") ///
    addnotes("Covariates: Yes" "District FE: Yes" "Year FE: Yes") ///
    label varwidth(20) ///
    tex

*Bacondecompostion
xtbalance, range(2001, 2020)
ddtiming  NTL PFTZ , i(city_id) t(year)
*AIPW for aggregation
xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001), group(city_id) vce(cluster city_id)
estat aggregation
xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001), group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation

*TWFE for aggregation
xthdidregress twfe (NTL NTL_2001) (PFTZ), group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation

* AIPW estimator (stata 18): Plot treatment dynamics of each group(cohort)
xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001), group(city_id) controlgroup(notyet) vce(cluster city_id)  
estat atetplot, sci name(fig1, replace)

*parallel trends
eventbaseline, pre(3) post(7) baseline(-1) graph 

*Placebo Test
//The coefficient distribution from the placebo test.
permute PFTZ beta = _b[PFTZ] se = _se[PFTZ] df = e(df_r), ///
  reps(500) rseed(123) saving("Simulations_12.dta"): ///
  reghdfe NTL PFTZ, absorb(city_id year) vce(cluster city_id)
  
//Plot the kernel density of regression coefficients
preserve //Save memory status temporarily
use "Simulations_12.dta", clear
gen t_value = beta / se
gen p_value = 2 * ttail(df, abs(beta/se))

#delimit ;
dpplot beta, 
  xline(.5704036, lc(black*0.5) lp(dash)) 
  xline(0, lc(black) lp(solid))         
  xlabel(0(0.1)0.75)
  xtitle(`"{fontface "Times New Roman":Random sampling coefficients}"') xlabel(, format(%4.2f) labsize(small))
  ytitle(`"{fontface "Times New Roman":Kernel density}"') ylabel(, nogrid format(%4.2f) labsize(small)) 
  note("") caption("") graphregion(fcolor(white)) msymbol(smcircle_hollow) mcolor(black) ;

#delimit cr
graph export "Fig_5.png", width(1000) replace
shellout Fig_5.png //Open the png file of the placebo test
restore //Restore memory status


*Robustness check - Heterogeneous effects
*Using port_city
xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if port_city == 1, group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation

xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if port_city == 0, group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation

*Using sez_city
xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if sez_city == 1, group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation

xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if sez_city == 0, group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation

*Using economic_belt
xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if economic_belt == "east", group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation


xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if economic_belt == "middle", group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation


xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if economic_belt == "west", group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation

xthdidregress aipw (NTL NTL_2001) (PFTZ NTL_2001) if economic_belt ==  "Northeast", group(city_id) controlgroup(notyet) vce(cluster city_id)
estat aggregation



