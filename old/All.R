library(tidyverse)
library(ggplot2)

load("brfss2013.RData")

# hlthpln1: Have Any Health Care Coverage
# persdoc2: Multiple Health Care Professionals
# medcost: Could Not See Dr. Because Of Cost
# checkup1: Length Of Time Since Last Routine Checkup
# medicare: Do You Have Medicare?
# hlthcvrg: Health Insurance Coverage
# delaymed: Delayed Getting Medical Care
# dlyother: Delayed Getting Medical Care Other Response
# nocov121: Without Health Care Coverage Past 12 Months
# lstcovrg: Time Since Last Had Health Care Coverage
# drvisits: Doctor Visits Past 12 Months
# medscost: Could Not Get Medicine Due To Cost
# carercvd: Satisfied With Care Received
# medbills: Currently Have Medical Bills
# _hcvu651: Respondents Aged 18-64 With Health Care Coverage
health_care_access <- subset(brfss2013, select=c("hlthpln1", "persdoc2", "medcost",
                                                 "checkup1", "medicare", "hlthcvrg",  
                                                 "delaymed", "dlyother", "nocov121",
                                                 "lstcovrg", "drvisits", "medscost",
                                                 "carercvd", "medbills", "X_hcvu651"))

# bphigh4: Ever Told Blood Pressure High
# bpmeds: Currently Taking Blood Pressure Medication
# _rfhype5: High Blood Pressure Calculated Variable
hypertension <- subset(brfss2013, select=c("bphigh4", "bpmeds", "X_rfhype5"))

# bloodcho: Ever Had Blood Cholesterol Checked
# cholchk: How Long Since Cholesterol Checked
# toldhi2: Ever Told Blood Cholesterol High
# _cholchk: Cholesterol Checked Calculated Variable
# _rfchol: High Cholesterol Calculated Variable
cholesterol_awareness <- subset(brfss2013, select=c("bloodcho", "cholchk", "toldhi2",
                                                    "X_cholchk", "X_rfchol"))

# cvdinfr4: Ever Diagnosed With Heart Attack
# cvdcrhd4: Ever Diagnosed With Angina Or Coronary Heart Disease
# cvdstrk3: Ever Diagnosed With A Stroke
# asthma3: Ever Told Had Asthma
# asthnow: Still Have Asthma
# _ltasth1: Lifetime Asthma Calculated Variable
# _casthm1: Current Asthma Calculated Variable
# _asthms1: Computed Asthma Status
# chcocncr: (Ever Told) You Had Any Other Types Of Cancer?
# chccopd1: (Ever Told) You Have (Copd) Chronic Obstructive Pulmonary Disease, Emphysema Or
# havarth3: Told Have Arthritis
# addepev2: Ever Told You Had A Depressive Disorder
chronic_health_disorder <- subset(brfss2013, select=c("cvdinfr4", "cvdcrhd4", "cvdstrk3", 
                                                      "asthma3", "asthnow", "chcocncr", 
                                                      "chccopd1", "havarth3", "addepev2",
                                                      "X_ltasth1", "X_casthm1", "X_asthms1"))

# physhlth: Number Of Days Physical Health Not Good
physical_healthy_days <- subset(brfss2013, select=c("physhlth"))

# genhlth: General Health
general_health <- subset(brfss2013, select=c("genhlth"))

# smoke100: Smoked At Least 100 Cigarettes
# smokday2: Frequency Of Days Now Smoking
# stopsmk2: Stopped Smoking In Past 12 Months
# lastsmk2: Interval Since Last Smoked
# usenow3: Use Of Smokeless Tobacco Products
# X_rfhlth: Adults With Good Or Better Health
# X_smoker3: Computed Smoking Status
# X_rfsmok3 : Current Smoking Calculated Variable
tobacco_use <- subset(brfss2013, select=c("smoke100", "smokday2", "stopsmk2", 
                                          "lastsmk2", "usenow3", "X_rfhlth", 
                                          "X_smoker3", "X_rfsmok3"))
table_contingence <- table(tobacco_use$X_rfhlth, tobacco_use$X_smoker3)
prop.table(table_contingence)

ggplot(tobacco_use, aes(x=X_rfhlth, fill=X_smoker3)) + 
  geom_bar()

ggplot(subset(tobacco_use, !is.na(X_rfhlth)), aes(x = X_rfhlth, fill = X_smoker3)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))

# exerany2: Exercise In Past 30 Days
# exract11: Type Of Physical Activity
# exeroft1: How Many Times Walking, Running, Jogging, Or Swimming
# exerhmm1: Minutes Or Hours Walking, Running, Jogging, Or Swimming
# exract21: Other Type Of Physical Activity Giving Most Exercise During Past Month
# exeroft2: How Many Times Walking, Running, Jogging, Or Swimming
# exerhmm2: Minutes Or Hours Walking, Running, Jogging, Or Swimming
# strength: How Many Times Did You Do Physical Activities Or Exercises To Strengthen Your Mu
# _totinda: Leisure Time Physical Activity Calculated Variable
# metvl11_: Activity Met Value For First Activity
# metvl21_: Activity Met Value For Second Activity
# maxvo2_: Estimated Age-Gender Specific Maximum Oxygen Consumption
# fc60_: Estimated Functional Capacity
# actin11_: Estimated Activity Intensity For First Activity
# actin21_: Estimated Activity Intensity For Second Activity
# padur1_: Minutes Of First Activity
# padur2_: Minutes Of Second Activity
# pafreq1_: Physical Activity Frequency Per Week For First Activity
# pafreq2_: Physical Activity Frequency Per Week For Second Activity
# _minac11: Minutes Of Physical Activity Per Week For First Activity
# _minac21: Minutes Of Physical Activity Per Week For Second Activity
# strfreq_: Strength Activity Frequency Per Week
# pamiss1_: Missing Physical Activity Data
# pamin11_: Minutes Of Physical Activity Per Week For First Activity
# pamin21_: Minutes Of Physical Activity Per Week For Second Activity
# pa1min_: Minutes Of Total Physical Activity Per Week
# pavig11_: Minutes Of Vigorous Physical Activity Per Week For First Activity
# pavig21_: Minutes Of Vigorousphysical Activity Per Week For Second Activity
# pa1vigm_: Minutes Of Total Vigorous Physical Activity Per Week
# _pacat1: Physical Activity Categories
# _paindx1: Physical Activity Index
# _pa150r2: 150 Minute Physical Activity Calculated Variable
# _pa300r2: 300 Minute Physical Activity Calculated Variable
# _pa30021: 300 Minute Physical Activity 2-Level Calculated Variable
# _pastrng: Muscle Strengthening Recommendation
# _parec1: Aerobic And Strengthening Guideline
# _pastae1: Aerobic And Strengthening (2-Level)
exercise <- subset(brfss2013, select=c("exerany2", "exract11", "exeroft1", 
                                       "exerhmm1", "exract21", "exeroft2", 
                                       "exerhmm2", "strength", "X_totinda",
                                       "metvl11_", "metvl21_", "maxvo2_",
                                       "fc60_", "actin11_", "actin21_",
                                       "padur1_", "padur2_", "pafreq1_",
                                       "pafreq2_", "X_minac11", "X_minac21",
                                       "strfreq_", "pamiss1_", "pamin11_",
                                       "pamin21_", "pa1min_", "pavig11_",
                                       "strfreq_", "pamiss1_", "pamin11_",
                                       "pavig21_", "pa1vigm_", "X_pacat1",
                                       "X_paindx1", "X_pa150r2", "X_pa300r2",
                                       "X_pa30021", "X_pastrng", "X_parec1",
                                       "X_pastae1"))


# harehab1: Outpatient Rehab After Heart Attack Hosp Stay
# strehab1: Outpatient Rehab After Hosp Stay For Stroke
# cvdasprn: Take Aspirin Daily Or Every Other Day
# aspunsaf: Health Makes Taking Aspirin Unsafe
# rlivpain: Take Aspirin To Relieve Pain
# rduchart: Take Aspirin To Reduce Chance Of Heart Attack
# rducstrk: Take Aspirin To Reduce Chance Of Stroke
cardiovascular <- subset(brfss2013, select=c("harehab1", "strehab1", "cvdasprn", 
                                             "aspunsaf", "rlivpain", "rduchart", 
                                             "rducstrk"))










