**# 修改路径
global path "F:\BaiduSyncdisk\论文\3Coal_did\CODE修改"
cd $path 

use "F:\BaiduSyncdisk\论文\3Coal_did\CODE修改\DATA_palcebo.dta",clear
drop if year>2023

**# 公司控制变量
global control_id Size FirmAge Employee Lev FIXED  Cashflow Mfee
sum $control_id

**# 省份控制变量
//gen eco=ln(人均地区生产总值元人)
//gen gov=Fiscal_exp/GDP
//gen env=Env/GDP/10000
replace env=0 if env==.
gen fin=Finance_sum/GDP/10000

global control_prov eco gov env fin
sum $control_prov

tabstat TE tt $control_id $control_prov ,stats(n mean sd min p50 max) format(%6.2f) column(stats)

	outreg2 using xxx.doc,replace sum(log) keep(**TE** *treat** **time** **tt** **Size** **FirmAge** **Employee** **Lev** **FIXED** **Cashflow** **Mfee** **eco** **gov** **env** **fin**) 
	
	pwcorr_a TE treat time  tt  $control_id $control_prov
	local s "Table2_corr"
	logout, save("`s'") word replace: ///
        pwcorr_a TE treat time  tt  $control_id $control_prov 

reg	TE treat time  tt  $control_id $control_prov
vif

**# 基准回归
reghdfe TE treat time tt
est sto m1
reghdfe TE treat time tt $control_id 
est sto m2
reghdfe TE treat time tt $control_id $control_prov 
est sto m3
reghdfe TE treat time tt $control_id $control_prov , absorb(id) 
est sto m4 
xtreg TE treat time tt $control_id $control_prov ,re 
est sto m5
reghdfe TE treat time tt $control_id $control_prov , absorb(id year)  
est sto m6

outreg2[m1 m2 m3 m4 m5 m6] using  xxx.doc, replace bdec(3) tdec(2) keep( TE treat time tt $control_id $control_prov ) addtext(id FE, YES,year FE,YES)

esttab m1 m2 m3 m4 m5 m6

qui xtreg TE treat time tt $control_id $control_prov ,fe //固定效应估计
est store FE //储存结果
qui xtreg TE treat time tt $control_id $control_prov ,re //随机效应估计
est store RE //储存结果
hausman FE RE

**# 平行趋势检验


reghdfe TE pre* current post* $control_id $control_prov , absorb(id year) 
est store Dynamic1

***********************************
**图1：双重差分模型的平行趋势检验**
***********************************
coefplot  Dynamic1, ///
   keep(pre6 pre5 pre4 pre3 pre2  current post1 post2 post3 post4 post5 post6)  ///
   coeflabels(pre6=2011 pre5=2015 pre4 = 2013  ///
   pre3 = 2014            ///
   pre2  = 2015             ///
    current = 2017           ///
	 post1  = 2018 post2=2019 post3=2020 post4=2021 post5=2022 post6=2023)          ///
   vertical                             ///
   yline(0,lp(dash))  ///
   ytitle("Regression coefficient of de-capacity policy")                 ///
   xtitle("year") ///
   addplot(line @b @at)                 ///
   ciopts(recast(rcap))                 ///
   rescale(100)                         ///
   ylabel(, nogrid)                   ///
   scheme(tufte)
  
   graph export "figurename.png", replace
   
**# 安慰剂检验
   

**# 替换被解释变量1
drop TE1
gen TE1=专利1/(RD+1)
reghdfe TE1 treat time tt $control_id $control_prov , absorb(id year)  
est sto m1

**# 替换被解释变量2
xtset id year
gen RD1=l.RD
replace RD1=0 if RD1==.
gen RD2=l2.RD
replace RD2=0 if RD2==.
gen RD3=l3.RD
replace RD3=0 if RD3==.
gen RD4=l4.RD
replace RD4=0 if RD4==.
gen RD5=l5.RD
replace RD5=0 if RD5==.
gen 分母=RD1+0.8*RD2+0.6*RD3+0.4*RD4+0.2*RD5
gen TE2=专利1/(RD1+1)

reghdfe TE2 treat time tt $control_id $control_prov , absorb(id year)  
est sto m2


**# 样本区间
reghdfe TE treat time tt $control_id $control_prov if year>2013 , absorb(id year)  
est sto m3
reghdfe TE treat time tt $control_id $control_prov if year<2019 , absorb(id year) 
est sto m4
outreg2[m1 m2 m3 m4] using  xxx.doc, replace bdec(3) tdec(2) keep( TE treat time tt $control_id $control_prov ) addtext(id FE, YES,year FE,YES)

**# PSM-DID
set seed 0001
g tmp = runiform()
sort tmp 
psmatch2 tt $control_id $control_prov , out(TE) logit ate neighbor(1) common caliper(.05) ties 
pstest $control_id $control_prov , both graph

gen common=_support
drop if common == 0 

reghdfe TE treat time tt $control_id $control_prov , absorb(id year)  
est sto m4

**# 替换回归方法 Tobit回归
tab id, gen(id_dum)
tobit TE treat time tt $control_id $control_prov ,ll(0) ul(1)
est sto m1
tobit TE treat time tt $control_id $control_prov year_dum* id_dum*,ll(0) ul(1)
est sto m2

**# 系统GMMM
xtset id year
gen lTE=l.TE

xtabond2 TE  treat time tt , gmm(lTE  , lag(2 2) ) iv(tt ) twostep  small orthogonal
xtabond2 TE lTE treat time tt $control_id $control_prov year_dum* , gmm(lTE ) iv(treat time tt $control_prov ) twostep small orthogonal
est sto m3

outreg2[m1 m2 m3 m4] using  xxx.doc, replace bdec(3) tdec(2) keep( lTE TE treat time tt $control_id $control_prov ) addtext(id FE, YES,year FE,YES)

**# 遗漏变量 政府补贴
gen ggt=ln(政府补贴+1)
reghdfe TE ggt tt $control_id $control_prov , absorb(id year)
est sto m1
gen ggt1=政府补贴/营业收入
reghdfe TE ggt1 tt $control_id $control_prov , absorb(id year)
est sto m2
gen ggt2=(ggt!=0)
reghdfe TE ggt2 tt $control_id $control_prov , absorb(id year)
est sto m3
outreg2[m1 m2 m3] using  xxx.doc, replace bdec(3) tdec(2) keep(TE tt ggt ggt1 ggt2 $control_id $control_prov ) addtext(id FE, YES,year FE,YES)

**# 异质性 国企私企
reghdfe TE tt $control_id $control_prov if SOE==1, absorb(id year)
est sto m1
reghdfe TE tt $control_id $control_prov if SOE==0, absorb(id year)
est sto m2

**# 异质性地理位置 East West Mid
reghdfe TE  tt $control_id $control_prov if East==1, absorb(id year)
est sto m3
reghdfe TE  tt $control_id $control_prov if Mid==1, absorb(id year)
est sto m4
reghdfe TE  tt $control_id $control_prov if  West ==1, absorb(id year)
est sto m5
outreg2[m1 m2 m3 m4 m5] using  xxx.doc, replace bdec(3) tdec(2) keep( TE treat time tt $control_id $control_prov ) addtext(id FE, YES,year FE,YES)

**# 异质性 不同发展阶段
bysort id: egen medianS=median(Size)
gen 企业发展S=(Size>medianS)
reghdfe TE tt $control_id $control_prov if 企业发展S==1, absorb(id year)
est sto m1
reghdfe TE tt $control_id $control_prov if 企业发展S ==0, absorb(id year)
est sto m2

**# 异质性 融资约束
egen mediansa=median(SA)
gen issa1=(SA<mediansa)
reghdfe TE tt $control_id $control_prov if issa1==1, absorb(id year)
est sto m3
reghdfe TE tt $control_id $control_prov if  issa1 ==0, absorb(id year)
est sto m4

**# 异质性 研发投入
egen medianrd=median(RD)
gen isrd=(RD>medianrd)
reghdfe TE tt $control_id $control_prov if isrd==1, absorb(id year)
est sto m5
reghdfe TE tt $control_id $control_prov if  isrd ==0, absorb(id year)
est sto m6

outreg2[m1 m2 m3 m4 m5 m6] using  xxx.doc, replace bdec(3) tdec(2) keep( TE treat time tt $control_id $control_prov ) addtext(id FE, YES,year FE,YES)

center RD
reghdfe TE c.isrd##c.tt $control_id $control_prov , absorb(id year)


**# 机制分析 去产能 融资约束 与创新效率
gen SA1=-SA
reghdfe hhi2 tt $control_id $control_prov , absorb(id year)
reghdfe SA pre* current post* $control_id $control_prov , absorb(id year)

center SA1, prefix(x_)
egen mediansa1=median(SA1)
gen issa2=(SA1>mediansa1)
reghdfe TE c.issa2##c.tt $control_id $control_prov , absorb(id year)
est sto m1
reghdfe TE c.x_SA1##c.tt $control_id $control_prov , absorb(id year)
est sto m2

center lerner
gen hhi2=hhi*hhi
center hhi3
egen medianhhi=median(hhi)
gen ishhi1=(hhi>medianhhi)
reghdfe TE c.ishhi1##c.tt $control_id $control_prov , absorb(id year)
est sto m3
center hhi
reghdfe TE c.c_hhi##c.tt $control_id $control_prov , absorb(id year)
est sto m4

outreg2[m1 m2 m3 m4] using  xxx.doc, replace bdec(3) tdec(2) keep( TE treat time tt c.issa2##c.tt issa2 c.x_SA##c.tt x_SA c.ishhi1##c.tt ishhi1 c.c_hhi##c.tt c_hhi $control_id $control_prov ) addtext(id FE, YES,year FE,YES)

**# 知识存量
	sort id year				
	bys id: gen tp=year-2011
	replace tp=tp+1
	gen kni=专利1*exp(-0.15*tp)
	bys id: gen kn=0
	bys id: replace n=_n
	replace kn=kni if n==1 
	bys id: replace kn=kn[_n-1]+kni[_n] if n!=1	
	
	center kn
	gen knp=ln(kn+1)
	reghdfe knp tt $control_id $control_prov , absorb(id year)
	reghdfe TE c.knp##c.tt $control_id $control_prov , absorb(id year) 
// merge 1:1 id year using "F:\BaiduSyncdisk\论文\3Coal_did\代码_(Stata)\兼并重组_8.dta" 


**# 机制分析 创新投入
reghdfe rd tt $control_id $control_prov , absorb(id year) 
est sto m1
//replace rde=ln(研发人员)
reghdfe rde tt $control_id $control_prov , absorb(id year) 
est sto m2
reghdfe TE rd rde tt $control_id $control_prov , absorb(id year)
est sto m3

**# 机制分析 创新产出
gen opin=ln(营业收入+1)
reghdfe pat tt $control_id $control_prov , absorb(id year) 
est sto m4
reghdfe opin tt $control_id $control_prov , absorb(id year) 
est sto m5
reghdfe TE pat opin tt $control_id $control_prov , absorb(id year) vce(r)
est sto m6

outreg2[m1 m2 m3 m4 m5 m6 ] using  xxx.doc, replace bdec(3) tdec(2) keep( TE treat time tt rd rde pat opin $control_id $control_prov ) addtext(id FE, YES,year FE,YES)
