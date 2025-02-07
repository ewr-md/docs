library(rjson)
#import data_main.json
here::here()
source(here::here("functions.R"))
data_main <- here::here("../data/processed/main_data.json")
results <- fromJSON(file = paste(data_main))
df <- as.data.frame(results)

#create single variable for total number of study participants
numTotal <- as.numeric(1543)

#  Create usable variable names
###############################
sigfigs_or <- as.numeric(1)
sigfigs_demo <- as.numeric(0)
sigfigs_percent <- as.numeric(1)

irb_brown <- "IRB#: IRB00000482"
irb_penn <- "IRB#: 822830"
phar <- "Pulmonary Hypertension Association Registry"

sdh <- "social determinants of health"
section_nonsig <- "Association between Non-Adherence and Pulmonary Hypertension Indices"

tab_title_demograhpics <- "Participant Demographics"
tab_title_phindices <- "Association between Non-Adherence and Pulmonary Hypertension Indices"

# BEGIN demographic variable names
###################################
varNames_age <- "Age at last follow-Up"
age_median <- as.numeric(printSigs(sigfigs_or, df$misc.age_median))
age_iqrLow <- as.numeric(printSigs(sigfigs_or, df$misc.age_IQR_low))
age_iqrHigh <- as.numeric(printSigs(sigfigs_or, df$misc.age_IQR_high))
set_age <- c(age_median)
  
overall_na <- as.numeric(94)
overal_ad <- as.numeric(numTotal - overall_na)
overall_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.overall.rNA*100))
overall_rAD <- as.numeric(printSigs(sigfigs_or, 100 - overall_rNA))
overall_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.overall.ciLowNA*100))
overall_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.overall.ciHighNA*100))
overall_pVal <- check_pVal_sigfigs(df$outcomes.overall.pVal)
varNames_adherent <- c("Non-Adherent", "Adherent")
set_adherent <- c(overall_na, overal_ad)

#load obesity data
df_obesity <- read.csv(here::here("../data/interm/","data_obesity.csv"))
obesity_0 <- as.numeric(df_obesity[1,2])
obesity_1 <- as.numeric(df_obesity[2,2])
obesity_missing <- as.numeric(df_obesity[3,2])
set_obesity <- c(obesity_0, obesity_1, obesity_missing)
varNames_obesity <- c("BMI < 30", "BMI >= 30", "Missing")

race_white <- as.numeric(printSigs(sigfigs_or, df$demo.race_white))
race_black <- as.numeric(printSigs(sigfigs_or, df$demo.race_black))
race_asian <- as.numeric(printSigs(sigfigs_or, sum(c(df$demo.race_aian, df$demo.race_chinese, df$demo.race_filipino, df$demo.race_indian, df$demo.race_japanese, df$demo.race_korean, df$demo.race_vietnamese, df$demo.race_other_asian, df$demo.race_pacific))))
race_multi <- as.numeric(printSigs(sigfigs_or, df$demo.race_multi))
race_missing <- as.numeric(printSigs(sigfigs_or, df$demo.race_missing))
race_unknown <- as.numeric(printSigs(sigfigs_or, df$demo.race_unknown))
comp_race <- sum(c(race_white, race_black, race_asian, race_multi, race_missing, race_unknown))
set_race <- c(race_white, race_black, race_asian, race_multi, race_missing, race_unknown)
varNames_race <- c("White", "Black", "Asian", "Multiple", "Missing", "Unknown")

varNames_ethnicity <- c("Hispanic", "Not Hispanic or Latino", "Missing", "Unknown")
ethnicity_hispanic <- as.numeric(df$demo.race_hispanic)
ethnicity_notHispanic <- as.numeric(df$demo.ethnicity_notHispanic)
ethnicity_unknown <- as.numeric(df$demo.ethnicity_unknown)
ethnicity_missing <- as.numeric(df$demo.ethnicity_missing)
set_ethnicity <- c(ethnicity_hispanic, ethnicity_notHispanic, ethnicity_missing, ethnicity_unknown)

sexborn_male <- as.numeric(printSigs(sigfigs_or, df$demo.sexborn_male))
sexborn_female <- as.numeric(printSigs(sigfigs_or, df$demo.sexborn_female))
sexborn_missing <- as.numeric(printSigs(sigfigs_or, df$demo.sexborn_missing))
comp_sexborn <- sum(c(sexborn_male, sexborn_female, sexborn_missing))
set_sexborn <- c(sexborn_male, sexborn_female, sexborn_missing)
varNames_sexborn <- c("Male", "Female", "Missing")

income_leq25 <- as.numeric(printSigs(sigfigs_or, df$demo.income_leq_25))
income_25_50 <- as.numeric(printSigs(sigfigs_or, df$demo.income_25_50))
income_50_75 <- as.numeric(printSigs(sigfigs_or, df$demo.income_50_75))
income_75_100 <- as.numeric(printSigs(sigfigs_or, df$demo.income_75_100))
income_geq100 <-as.numeric(printSigs(sigfigs_or, df$demo.income_geq_100))
income_unknown <- as.numeric(printSigs(sigfigs_or, df$demo.income_unknown))
income_missing <- as.numeric(printSigs(sigfigs_or, df$demo.income_missing))
income_declined <- as.numeric(printSigs(sigfigs_or, df$demo.income_declined))
comp_income <- sum(c(income_leq25, income_25_50, income_50_75, income_75_100, income_geq100, income_missing, income_unknown, income_declined))
set_income <- c(income_leq25, income_25_50, income_50_75, income_75_100, income_geq100, income_missing, income_unknown, income_declined)
varNames_income <- c("<$25,000", "$25,000 - $50,000", "$50,000 - $75,000", "$75,000 - $100,000", ">$100,000", "Missing", "Unknown", "Declined to answer")

education_lessthan_hs <- as.numeric(printSigs(sigfigs_or, df$demo.edu_lessthan_hs))
education_hs <- as.numeric(printSigs(sigfigs_or, df$demo.edu_hs))
education_some_college <- as.numeric(printSigs(sigfigs_or, df$demo.edu_some_college))
education_college <- as.numeric(printSigs(sigfigs_or, df$demo.edu_college))
education_missing <- as.numeric(printSigs(sigfigs_or, df$demo.education_missing))
comp_education <- sum(c(education_lessthan_hs, education_hs, education_some_college, education_college, education_missing))
set_education <- c(education_lessthan_hs, education_hs, education_some_college, education_college, education_missing)
varNames_education <- c("Less than high school", "High school", "Some college", "College degree", "Missing")

partnered_TRUE <- as.numeric(printSigs(sigfigs_or, df$demo.mar_1))
partnered_FALSE <- as.numeric(printSigs(sigfigs_or, df$demo.mar_0))
partnered_missing <- as.numeric(printSigs(sigfigs_or, df$demo.mar_missing))
comp_partnered <- sum(c(partnered_TRUE, partnered_FALSE, partnered_missing))
set_partnered <- c(partnered_TRUE, partnered_FALSE, partnered_missing)
varNames_partnered <- c("Married or partnered", "Unpartnered", "Missing")

BELOW_POV_FALSE <- as.numeric(printSigs(sigfigs_or, df$demo.BELOW_POV_0))
BELOW_POV_TRUE <- as.numeric(printSigs(sigfigs_or, df$demo.BELOW_POV_1))
BELOW_POV_missing <- as.numeric(printSigs(sigfigs_or, df$demo.BELOW_POV_missing))
comp_POV <- sum(c(BELOW_POV_TRUE, BELOW_POV_FALSE, BELOW_POV_missing))
set_POV <- c(BELOW_POV_TRUE, BELOW_POV_FALSE, BELOW_POV_missing)
varNames_POV <- c("Below poverty line", "Above poverty line", "Missing")

medicaid_TRUE <- as.numeric(printSigs(sigfigs_or, df$demo.medicaid1_1))
medicaid_FALSE <- as.numeric(printSigs(sigfigs_or, df$demo.medicaid1_0))
medicaid_missing <- as.numeric(printSigs(sigfigs_or, df$demo.medicaid1_missing))
comp_medicaid <- sum(c(medicaid_TRUE, medicaid_FALSE, medicaid_missing))
set_medicaid <- c(medicaid_TRUE, medicaid_FALSE, medicaid_missing)
varNames_medicaid <- c("Medicaid or No Insurance", "Other insurance", "Missing")

wsph1 <- sum(c(df$demo.primaryphdx_wsph1_1,
               df$demo.primaryphdx_wsph1_2,
               df$demo.primaryphdx_wsph1_3,
               df$demo.primaryphdx_wsph1_4a,
               df$demo.primaryphdx_wsph1_4b,
               df$demo.primaryphdx_wsph1_4c,
               df$demo.primaryphdx_wsph1_4d,
               df$demo.primaryphdx_wsph1_6,
               df$demo.primaryphdx_wsph1_7
))
pah <- wsph1  
wsph4 <- as.numeric(printSigs(sigfigs_demo, df$demo.primaryphdx_wsph4))
cteph <- wsph4
wsph_missing <- as.numeric(printSigs(sigfigs_demo, df$demo.primaryphdx_missing))
comp_wsph <- sum(c(pah, cteph, wsph_missing))
set_wsph <- c(pah, cteph, wsph_missing)
varNames_wsph <- c("PAH", "CTEPH", "Missing")

nyha_1 <- sum(c(as.numeric(printSigs(sigfigs_demo, df$demo.nyha_0)), as.numeric(printSigs(sigfigs_demo, df$demo.nyha_1))))
nyha_2 <- as.numeric(printSigs(sigfigs_demo, df$demo.nyha_2))
nyha_3 <- as.numeric(printSigs(sigfigs_demo, df$demo.nyha_3))
nyha_4 <- as.numeric(printSigs(sigfigs_demo, df$demo.nyha_4))
nyha_missing <- as.numeric(printSigs(sigfigs_demo, df$demo.nyha_missing))
comp_nyha <- sum(c(nyha_1, nyha_2, nyha_3, nyha_4, nyha_missing))
set_nyha <- c(nyha_1, nyha_2, nyha_3, nyha_4, nyha_missing)
varNames_nyha <- c("Class I", "Class II", "Class III", "Class IV", "Missing")

prost_i_TRUE <- as.numeric(printSigs(sigfigs_demo, df$demo.prost_i_1))
prost_i_FALSE <- as.numeric(printSigs(sigfigs_demo, df$demo.prost_i_0)) 
prost_i_missing <- as.numeric(printSigs(sigfigs_demo, df$demo.prost_i_missing))
set_prost_i <- c(prost_i_TRUE, prost_i_FALSE, prost_i_missing)
comp_prost_i <- sum(set_prost_i)
varNames_prost_i <- c("Prescribed", "Not prescribed", "Missing")

prost_p_TRUE <- as.numeric(printSigs(sigfigs_demo, df$demo.prost_p_1))
prost_p_FALSE <- as.numeric(printSigs(sigfigs_demo, df$demo.prost_p_0))
prost_p_missing <- as.numeric(printSigs(sigfigs_demo, df$demo.prost_p_missing))
set_prost_p <- c(prost_p_TRUE, prost_p_FALSE, prost_p_missing)
comp_prost_p <- sum(set_prost_p) 
varNames_prost_p <- c("Prescribed", "Not prescribed", "Missing")

#END demographic variable naming

# START outcome variable naming
ernum_ad <- as.numeric(printSigs(sigfigs_or, df$outcomes.ernum.rAD))
ernum_na <- as.numeric(printSigs(sigfigs_or, df$outcomes.ernum.rNA))
ernum_na_ciLow <- as.numeric(printSigs(sigfigs_or, df$outcomes.ernum.ciLowNA))
ernum_na_ciHigh <- as.numeric(printSigs(sigfigs_or, df$outcomes.ernum.ciHighNA))
ernum_ad_ciLow <- as.numeric(printSigs(sigfigs_or, df$outcomes.ernum.ciLowAD))
ernum_ad_ciHigh <- as.numeric(printSigs(sigfigs_or, df$outcomes.ernum.ciHighAD))
ernum_pVal <- check_pVal_sigfigs(df$outcomes.ernum.pVal)

hospnum_ad <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospnum.rAD))
hospnum_ad_ciLow <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospnum.ciLowAD))
hospnum_ad_ciHigh <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospnum.ciHighAD))
hospnum_na <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospnum.rNA))
hospnum_na_ciLow <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospnum.ciLowNA))
hospnum_na_ciHigh <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospnum.ciHighNA))
hospnum_pVal <- check_pVal_sigfigs(df$outcomes.hospnum.pVal)

hospstaytotal_ad <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospstaytotal.rAD))
hospstaytotal_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospstaytotal.ciLowAD))
hospstaytotal_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospstaytotal.ciHighAD))
hospstaytotal_na <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospstaytotal.rNA))
hospstaytotal_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospstaytotal.ciLowNA))
hospstaytotal_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.hospstaytotal.ciHighNA))
hospstaytotal_pVal <- check_pVal_sigfigs(df$outcomes.hospstaytotal.pVal)

emph10score_ad <- as.numeric(printSigs(sigfigs_or, df$outcomes.emph10score.scoreAD))
emph10score_na <- as.numeric(printSigs(sigfigs_or, df$outcomes.emph10score.scoreNA))

emph10score_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.emph10score.ciLowNA))
emph10score_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.emph10score.ciHighNA))
emph10score_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.emph10score.ciLowAD))
emph10score_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.emph10score.ciHighAD))


emph10score_pVal <- check_pVal_sigfigs(df$outcomes.emph10score.pVal)

sfment_ad <- as.numeric(printSigs(sigfigs_or, df$outcomes.sfment.scoreAD))
sfment_na <- as.numeric(printSigs(sigfigs_or, df$outcomes.sfment.scoreNA))
sfment_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.sfment.ciLowNA))
sfment_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.sfment.ciHighNA))
sfment_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.sfment.ciLowAD))
sfment_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.sfment.ciHighAD))
sfment_pVal <- check_pVal_sigfigs(df$outcomes.sfment.pVal)

bnp_NA <- as.numeric(printSigs(sigfigs_or, df$outcomes.bnp.rNA))
bnp_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.bnp.ciLowNA))
bnp_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.bnp.ciHighNA))
bnp_AD <- as.numeric(printSigs(sigfigs_or, df$outcomes.bnp.rAD))
bnp_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.bnp.ciLowAD))
bnp_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.bnp.ciHighAD))
bnp_pVal <- check_pVal_sigfigs(df$outcomes.bnp.pVal)
# END outcome variable naming

# START predictor variable naming
sexborn_or <- as.numeric(printSigs(sigfigs_or, df$predictors.sexborn.orNA))
sexborn_or_ciHigh <- as.numeric(printSigs(sigfigs_or, df$predictors.sexborn.orHighNA))
sexborn_or_ciLow <- as.numeric(printSigs(sigfigs_or, df$predictors.sexborn.orLowNA))
sexborn_or_pVal <- check_pVal_sigfigs(df$predictors.sexborn.orPval)

education_or <- as.numeric(printSigs(sigfigs_or, df$predictors.education.or))
education_or_ciHigh <- as.numeric(printSigs(sigfigs_or, df$predictors.education.orHighNA))
education_or_ciLow <- as.numeric(printSigs(sigfigs_or, df$predictors.education.orLowNA))
education_or_pVal <- check_pVal_sigfigs(df$predictors.education.pVal)

unpartnered_or <- as.numeric(printSigs(sigfigs_or, df$predictors.married.or))
unpartnered_or_ciHigh <- as.numeric(printSigs(sigfigs_or, df$predictors.married.orHighNA))
unpartnered_or_ciLow <- as.numeric(printSigs(sigfigs_or, df$predictors.married.orLowNA))
unpartnered_or_pVal <- check_pVal_sigfigs(df$predictors.married.pVal)

BELOW_POV_or <- as.numeric(printSigs(sigfigs_or, df$predictors.BELOW_POV.or))
BELOW_POV_or_ciHigh <- as.numeric(printSigs(sigfigs_or, df$predictors.BELOW_POV.orHigh))
BELOW_POV_or_ciLow <- as.numeric(printSigs(sigfigs_or, df$predictors.BELOW_POV.orLow))
BELOW_POV_or_pVal <- check_pVal_sigfigs(df$predictors.BELOW_POV.pVal)

medicaid1_or <- as.numeric(printSigs(sigfigs_or, df$predictors.medicaid1.orNA))
medicaid1_ciHigh <- as.numeric(printSigs(sigfigs_or, df$predictors.medicaid1.orHighNA))
medicaid1_ciLow <- as.numeric(printSigs(sigfigs_or, df$predictors.medicaid1.orLowNA))
medicaid1_pVal <- check_pVal_sigfigs(df$predictors.medicaid1.orPval)

education_or <- as.numeric(printSigs(sigfigs_or, df$predictors.education.or))
education_or_ciHigh <- as.numeric(printSigs(sigfigs_or, df$predictors.education.orHighNA))
education_or_ciLow <- as.numeric(printSigs(sigfigs_or, df$predictors.education.orLowNA))
education_or_pVal <- check_pVal_sigfigs(df$predictors.education.pVal)
# END predictor variable naming

# START insignif values
ntpbnp_na <- as.numeric(printSigs(sigfigs_or, df$outcomes.ntpbnp.rNA))
ntpbnp_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ntpbnp.ciLowNA))
ntpbnp_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ntpbnp.ciHighNA))
ntpbnp_ad <- check_pVal_sigfigs(df$outcomes.ntpbnp.rAD)
ntpbnp_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ntpbnp.ciLowAD))
ntpbnp_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ntpbnp.ciHighNA))
ntpbnp_pVal <- check_pVal_sigfigs(df$outcomes.ntpbnp.pVal)

sixm_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.sixm_distance.rNA))
sixm_rAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.sixm_distance.rAD))
sixm_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.sixm_distance.ciLowNA))
sixm_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.sixm_distance.ciHighNA))
sixm_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.sixm_distance.ciLowAD))
sixm_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.sixm_distance.ciHighAD))
sixm_pVal <- check_pVal_sigfigs(df$outcomes.sixm_distance.pVal)

pasat_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.pasat.rNA))
pasat_rAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.pasat.rAD))
pasat_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.pasat.ciLowNA))
pasat_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.pasat.ciHighNA))
pasat_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.pasat.ciLowAD))
pasat_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.pasat.ciHighAD))
pasat_pVal <- check_pVal_sigfigs(df$outcomes.pasat.pVal)

ra_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ra.rNA))
ra_rAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ra.rAD))
ra_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ra.ciLowNA))
ra_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ra.ciHighNA))
ra_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ra.ciLowAD))
ra_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ra.ciHighAD))
ra_pVal <- check_pVal_sigfigs(df$outcomes.ra.pVal)

mpa_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.mpa.rNA))
mpa_rAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.mpa.rAD))
mpa_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.mpa.ciLowNA))
mpa_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.mpa.ciHighNA))
mpa_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.mpa.ciLowAD))
mpa_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.mpa.ciHighAD))
mpa_pVal <- check_pVal_sigfigs(df$outcomes.mpa.pVal)

pawp_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.pawp.rNA))
pawp_rAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.pawp.rAD))
pawp_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.pawp.ciLowNA))
pawp_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.pawp.ciHighNA))
pawp_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.pawp.ciLowAD))
pawp_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.pawp.ciHighAD))
pawp_pVal <- check_pVal_sigfigs(df$outcomes.pawp.pVal)

cardiacout_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.cardiacout.rNA))
cardiacout_rAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.cardiacout.rAD))
cardiacout_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.cardiacout.ciLowNA))
cardiacout_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.cardiacout.ciHighNA))
cardiacout_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.cardiacout.ciLowAD))
cardiacout_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.cardiacout.ciHighAD))
cardiacout_pVal <- check_pVal_sigfigs(df$outcomes.cardiacout.pVal)

ci_rNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ci.rNA))
ci_rAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ci.rAD))
ci_na_ciLowNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ci.ciLowNA))
ci_na_ciHighNA <- as.numeric(printSigs(sigfigs_or, df$outcomes.ci.ciHighNA))
ci_ad_ciLowAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ci.ciLowAD))
ci_ad_ciHighAD <- as.numeric(printSigs(sigfigs_or, df$outcomes.ci.ciHighAD))
ci_pVal <- check_pVal_sigfigs(df$outcomes.ci.pVal)


race_pVal <- check_pVal_sigfigs(df$predictors.insignif.race1_pVal)
ethnicity_pVal <- check_pVal_sigfigs(df$predictors.insignif.hispanic_pVal)
employment_pVal <- check_pVal_sigfigs(df$predictors.insignif.employ_pVal)
phType_pVal <- check_pVal_sigfigs(df$predictors.insignif.CTEPH_pVal)
nummeds_now_pVal <- check_pVal_sigfigs(df$predictors.insignif.nummeds_now_pVal)
nummeds_ever_pVal <- check_pVal_sigfigs(df$predictors.insignif.nummeds_ever_pVal)
prost_pVal <-check_pVal_sigfigs(df$predictors.insignif.prost_pVal)
prost_i_pVal <-check_pVal_sigfigs(df$predictors.insignif.prost_i_pVal)
prost_p_pVal <-check_pVal_sigfigs(df$predictors.insignif.prost_p_pVal)
type_meds_pVal <- check_pVal_sigfigs(df$predictors.insignif.type_meds_pVal)
# END insignif values

df_numMeds <- data.frame(read.csv("../data/processed/num_meds.csv"))
df_numMeds_trimmed <- df_numMeds[c(1:2), c(2:5)]
df_prostacyclin <-data.frame(read.csv("../data/processed/prostacyclins.csv"))
df_prostacyclin_trimmed <- df_prostacyclin[c(2, 4, 6), c(2, 7:9)]

df_list <- list(df_numMeds_trimmed, df_prostacyclin_trimmed)
df_byMed <- Reduce(function(x,y) merge(x,y, all = TRUE), df_list)
