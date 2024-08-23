
*******Define path and set working directory********************
global path="D:\MOB_NFHS"
cd "${path}"

use IABR7BFL, clear
**********filtering data*************************************
fre midx
drop if v208==0 // dropping no births in the last five years
drop if b2<2014 // dropping cohorts before 2014
drop if b5==0 // dropping dead children
drop if v150==12|v150==17 // drop if not related/servant to HH
drop if v135==2 // dropping visitors
drop if midx!=1 // keeping only last births

gen m19_kg= m19/1000 if m19<9996
label variable m19_kg "Birth Weight in Kilograms"
label values m19_kg M19

gen height_mother=v438/10 if v438<9994

gen BMI_mother=v445/100 if v445<9998

******Prenatal Care Assistance*************************
gen prenatal_care_person=0 if m2n==1
replace prenatal_care_person=1 if m2a!=1 & (m2b==1|m2g==1|m2h==1|m2i==1|m2j==1|m2k==1)
replace prenatal_care_person=2 if m2a==1 & (m2b==1|m2g==1|m2h==1|m2i==1|m2j==1|m2k==1) // Doctor and someone else
replace prenatal_care_person=3 if m2a==1 & (m2b!=1 & m2g!=1 & m2h!=1 & m2i!=1 & m2j!=1 & m2k!=1) // only doctor

gen antenatal_visits_number=0 if m14==0
replace antenatal_visits_number=1 if m14>0 & m14<=4
replace antenatal_visits_number=2 if m14>4 & m14<98

gen supplementary_food= 1 if s566a==1
replace supplementary_food=0 if s566a==0|s565==0

gen m44i=1 if (m43==1|m44==1) // Aware of pregnancy complications
replace m44i=0 if (m43==0 & m44==0)|(m2n==1)

gen rural= 1 if v140==2 //Rural or Urban household
replace rural=0 if v140==1

gen female_head=1 if v151==2 // Household head sex
replace female_head=0 if v151==1

gen birth_order=bord

gen female_child=1 if b4==2
replace female_child=0 if b4==1

gen tetanus_injections=	m1
label values tetanus_injections M1

gen iron_tablets_syrup= m45 if m45!=8
label values iron_tablets_syrup M45

gen education_mother= v133

gen wealth_index= v190a
label values wealth_index V190A

gen age_mother= v012

gen age_household_head=	v152 if v152<98

save IABR7BFL_filtered, replace

***********Household head info from PR file******************
use IAPR7BFL, clear
keep if hv101==1
keep hv001 hv002 hv108 hv012 hv219 hv220 sh49 sh47
rename hv001 v001
rename hv002 v002
rename hv012 hv012_PR
save household_head, replace

use IABR7BFL_filtered, clear
merge m:1 v001 v002 using household_head
drop if _merge==2
save IABR7BFL_filtered.dta, replace

use IABR7BFL_filtered, clear
gen caste_head=sh49 if sh49!=4 //Caste categorical variable
replace caste_head=0 if sh49==4

gen religion_head=0 if sh47==1
replace religion_head=1 if sh47==2
replace religion_head=2 if sh47==3
replace religion_head=3 if (sh47!=1&sh47!=2&sh47!=3) //religion categorical variable

rename	m19_kg birth_weight
rename	m44i aware_pregnancy_complications
rename	hv108	education_household_head
rename hv012 household_size
save IABR7BFL_filtered, replace

********Infant cohort************************************
use IABR7BFL_filtered, clear
keep if b19<=12 // keep only infants

keep birth_weight BMI_mother height_mother prenatal_care_person supplementary_food antenatal_visits_number tetanus_injections iron_tablets_syrup aware_pregnancy_complications rural female_head education_household_head caste_head religion_head education_mother wealth_index age_mother age_household_head household_size birth_order female_child

*********dropping don't knows and outliers****
drop if education_household_head==98
drop if tetanus_injections==8
drop if caste_head==8
drop if birth_weight>=6.1 & birth_weight<9996 // dropping Outliers
save infantcohort,replace
