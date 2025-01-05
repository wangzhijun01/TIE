use "F:\BaiduSyncdisk\论文\3Coal_did\CODE修改\DATA_palcebo.dta",clear

cap erase "simulations0.dta"
permute tt beta=_b[tt] se=_se[tt] df=e(df_r),reps(500) seed(777) saving(simulations0.dta,replace): reghdfe TE tt $control_id $control_prov, absorb(id year) vce(r)

use "simulations0.dta",clear
gen t_value=beta/se
gen p_value=2*ttail(df,abs(beta/se))
dpplot beta,xline(0.252,lp(dash)) xtitle("Estimator") ytitle("Density")
dpplot t_value,xtitle("T-value") ytitle("Density")
dpplot p_value,xtitle("P-value") ytitle("Density")
twoway(scatter p_value beta)(kdensity beta,yaxis(2))

use data,clear 
cap erase "simulations.dta"
permute ESG beta=_b[ESG] se=_se[ESG] df=e(df_r),reps(500) seed(777) saving(simulations.dta,replace): reghdfe TFP_LP ESG $控制变量, absorb(stkcd year) vce(cluster stkcd)

use "simulations.dta",clear
gen t_value=beta/se
gen p_value=2*ttail(df,abs(beta/se))
dpplot beta,xline(0.252,lp(dash)) xtitle("Estimator") ytitle("Density")
dpplot t_value,xtitle("T-value") ytitle("Density")
dpplot p_value,xtitle("P-value") ytitle("Density")
twoway(scatter p_value beta)(kdensity beta,yaxis(2))

graph save $Out\图2安慰剂检验.gph,replace 
graph export $Out\图2安慰剂检验.png,replace