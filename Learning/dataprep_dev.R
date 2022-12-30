pivoted_rrt <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_rrt.csv")
crrt_durations <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/crrt_durations.csv")
kdigo_stages <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/kdigo_stages.csv")
icustay_detail <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/icustay_detail.csv")
pivoted_lab <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_lab.csv")
pivoted_bg <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_bg.csv")
esrd_patients <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/esrd_patients.csv")
vasopressor_durations <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/vasopressor_durations.csv")
ventilation_durations <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/ventilation_durations.csv")
admissions <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/admissions.csv")

# or alternatively load("rawdata.RData")

library(dplyr)

# Age eligible ICU stays (i.e., age at admission >= 18yo)
age_eligible_icustays <- icustay_detail %>% filter(admission_age>=18) %>% select(icustay_id, hadm_id, subject_id)

# CKD ineligible ICU stays (i.e., esrd, or Peritoneal Dialysis mentioned in Discharge Summary)
ckd_ineligible_subject <- esrd_patients %>% rename(subject_id = SUBJECT_ID, hadm_id = HADM_ID)

# KDIGO3 eligible (Get the first KDIGO3 event per icustay)
kdigo_eligible_icustays <- kdigo_stages %>% filter(aki_stage==3) %>% mutate(charttime=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS")) %>%
        group_by(icustay_id) %>% arrange(charttime) %>%
        filter(row_number()==1) %>% rename(charttime.kdigo3 = charttime)

# KDIGO3 eligible on creatinine values only  (Get the first KDIGO3 event per icustay)
#kdigo_eligible_icustays <- kdigo_stages %>% filter(aki_stage_creat==3) %>% mutate(charttime=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS")) %>%
#        group_by(icustay_id) %>% arrange(charttime) %>%
#        filter(row_number()==1) %>% rename(charttime.kdigo3 = charttime)

# Flow chart step 3 "ICU stays" (18yo or older, no esrd, had kdigo3)
eligible_icustays_temp <- age_eligible_icustays %>% filter(!subject_id %in% ckd_ineligible_subject$subject_id, icustay_id %in% kdigo_eligible_icustays$icustay_id)
eligible_icustays1 <- merge(x=eligible_icustays_temp, y=kdigo_eligible_icustays, by="icustay_id", all.x=TRUE)

# Ventilation eligible patients (i.e. had VM prior to KIDIGO)
vm_eligible_icustay <- merge(eligible_icustays1, ventilation_durations, by="icustay_id", all.x = TRUE) %>%
         mutate(starttime=as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%OS"),
                endtime=as.POSIXct(endtime, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(vm_to_aki_hours=difftime(starttime, charttime.kdigo3, unit="hours")) %>%
        filter(!is.na(vm_to_aki_hours))

# Vasopressor eligible patients (i.e. had vaso prior to KIDIGO)
vaso_eligible_icustay <- merge(eligible_icustays1, vasopressor_durations, by="icustay_id", all.x = TRUE) %>%
        mutate(starttime=as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%OS"),
               endtime=as.POSIXct(endtime, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(vaso_to_aki_hours=difftime(starttime, charttime.kdigo3, unit="hours")) %>%
        filter(!is.na(vaso_to_aki_hours))

# Flow chart step 3 "ICU stays" (18yo or older, no esrd, had kdigo3 preceded by either vm or vaso)
eligible_icustays2 <- eligible_icustays1 %>% filter(icustay_id %in% vm_eligible_icustay$icustay_id |
                                                            icustay_id %in% vaso_eligible_icustay$icustay_id)

# Flow chart step 4 (if eligibility occurred twice for the same patients include the patient the first time they met inclusion criteria)
eligible_icustays3 <- merge(x=eligible_icustays2, y=icustay_detail, by="icustay_id", all.x = TRUE) %>%
        group_by(subject_id.x) %>% arrange(subject_id.x, intime) %>% filter(row_number()==1) %>%
        rename(subject_id=subject_id.x, hadm_id=hadm_id.x)

# 4863 eligible patients      
#

# Get the patients who had RRT prior to KDIGO3
rrt_before_aki <- merge(eligible_icustays3, pivoted_rrt, by = "icustay_id", all = TRUE) %>% 
        mutate(charttime=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS")) %>%
        rename(charttime.rrt = charttime) %>%
        filter(charttime.rrt<charttime.kdigo3, dialysis_active==1) %>%
        group_by(subject_id) %>% arrange(subject_id, charttime.rrt) %>% filter(row_number()==1)

# Exclude these patients
eligible_icustays4 <- filter(eligible_icustays3, !(icustay_id %in% rrt_before_aki$icustay_id) )
# 4662 eligible patients     

# Get first active RRT time and type for each icu stay
firstrrt <- pivoted_rrt %>% rename(charttime.rrt=charttime) %>%
        filter(dialysis_active==1) %>% group_by(icustay_id) %>% arrange(icustay_id, charttime.rrt) %>%
        filter(row_number()==1)

# Left join these first active RRT onto our included patients
dat <- merge(x=eligible_icustays4, y=firstrrt, by="icustay_id", all.x = TRUE) %>%
        mutate(dod=as.POSIXct(dod, format="%Y-%m-%d %H:%M:%OS"),
                aki_to_rrt_hours=difftime(charttime.rrt, charttime.kdigo3, unit="hours"),
                aki_to_death_days=difftime(dod, charttime.kdigo3, unit="days")) 

table(!is.na(dat$aki_to_rrt_hours))
hist(as.numeric(dat$aki_to_rrt_hours)/24, breaks = 0:70)
sum(as.numeric(dat$aki_to_rrt_hours)/24<3, na.rm=T)

# select patients who had a creatinine value measured 24 hours prior to KDIGO3
# temp <- merge(x=dat, y=pivoted_lab, by="subject_id", all.x = TRUE) %>%
#        select(-c(subject_id.y, hadm_id.y))

#dat2 <- temp %>% rename(charttime.lab = charttime) %>% mutate(charttime.lab=as.POSIXct(charttime.lab, format="%Y-%m-%d %H:%M:%OS")) %>%
#        mutate(time_to_aki_hours=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
#        arrange(subject_id, desc(time_to_aki_hours)) %>% filter(time_to_aki_hours<=0, !is.na(CREATININE)) %>%
#        group_by(subject_id) %>% filter(row_number()==1, time_to_aki_hours>=-24)

dim(dat) # 4662 * 35
summary(as.numeric(dat$aki_to_rrt_hours)) # median 38.00  
table(as.numeric(dat$aki_to_rrt_hours)<72, useNA = 'ifany') # 478 TRUEs

#### Get baseline weights
weight_durations <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/weight_durations.csv")
temp <- merge(x=dat, y=weight_durations, by="icustay_id", all.x = TRUE) %>%
        group_by(subject_id) %>% 
        mutate(weight_starttime=as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%OS"),
               weight_to_aki=difftime(weight_starttime, charttime.kdigo3, unit="hours")) %>%
        filter(weight_to_aki<=0) %>%
        arrange(subject_id, desc(weight_to_aki)) %>%
        filter(row_number()==1) %>%
        rename(weight_time = starttime) %>% ungroup() %>%
        select(c(icustay_id, weight_time, weight)) 

dat <- merge(x=dat, y=temp, by="icustay_id", all.x = TRUE)

#### Get baseline SOFAs
pivoted_sofa <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/pivoted_sofa.csv")
temp <- merge(x=dat, y=pivoted_sofa, by="icustay_id", all.x = TRUE) %>%
        group_by(subject_id) %>% 
        mutate(sofa_starttime=as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%OS"),
               sofa_to_aki=difftime(sofa_starttime, charttime.kdigo3, unit="hours")) %>%
        filter(sofa_to_aki<=0) %>%
        arrange(subject_id, desc(sofa_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        select(c(icustay_id, sofa_starttime, SOFA_24hours)) ## add other SOFA variables here if needed

dat <- merge(x=dat, y=temp, by="icustay_id", all.x = TRUE)

#### Get baseline immunosuppressant
immunosuppressant <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/immunosuppressant.csv")
colnames(immunosuppressant) <- tolower(colnames(immunosuppressant))

temp <- merge(x=dat, y=immunosuppressant, by="icustay_id", all.x = TRUE) %>%
        group_by(subject_id) %>% 
        mutate(immunosuppressant_startdate=as.POSIXct(startdate, format="%Y-%m-%d %H:%M:%OS"),
               immunosuppressant_to_aki=difftime(immunosuppressant_startdate, charttime.kdigo3, unit="hours")) %>%
        filter(immunosuppressant_to_aki<=0) %>%
        arrange(subject_id, desc(immunosuppressant_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        select(c(icustay_id, immunosuppressant_startdate, drug))

dat <- merge(x=dat, y=temp, by="icustay_id", all.x = TRUE)

#### Merge labvalues table and Blood gas table
rm(list=ls()[! ls() %in% c("dat","pivoted_lab", "pivoted_bg", "kdigo_stages", "admissions")])

lab <- bind_rows(pivoted_lab, pivoted_bg) %>% filter(hadm_id %in% dat$hadm_id)


#### Get urine output at k=1 (closest value prior to aki kdigo3)
temp <- merge(x=kdigo_stages %>% select(icustay_id, charttime, uo_rt_24hr),
              y=dat %>% select(icustay_id, charttime.kdigo3), by="icustay_id", all.x = TRUE) %>%
        filter(icustay_id %in% dat$icustay_id) %>%
        group_by(icustay_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki<=0 & !is.na(lab_to_aki) & !is.na(uo_rt_24hr)) %>%
        arrange(icustay_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(uo_k1 = uo_rt_24hr, charttime.uo_k1 = charttime.lab) %>%
        select(c(icustay_id, charttime.uo_k1, uo_k1))

dat <- merge(x=dat, y=temp, by="icustay_id", all.x = TRUE)

#### Get potassium at k=1 (closest value prior to aki kdigo 3)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki<=0 & !is.na(POTASSIUM)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(pot_k1 = POTASSIUM, charttime.pot.k1 = charttime.lab) %>%
        select(c(hadm_id, charttime.pot.k1, pot_k1))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)

#### Get pH at k=1 (closest value prior to aki kdigo 3)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki<=0 & !is.na(PH)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(ph_k1 = PH, charttime.ph.k1 = charttime.lab) %>%
        select(c(hadm_id, charttime.ph.k1, ph_k1))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)

#### Get bun at k=1 (closest value prior to aki kdigo 3)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki<=0 & !is.na(BUN)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(bun_k1 = BUN, charttime.bun.k1 = charttime.lab) %>%
        select(c(hadm_id, charttime.bun.k1, bun_k1))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)

#### Get creatinine at k=1 (closest value prior to aki kdigo 3)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki<=0 & !is.na(CREATININE)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(creat_k1 = CREATININE, charttime.creat.k1 = charttime.lab) %>%
        select(c(hadm_id, charttime.creat.k1, creat_k1))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)


###################
###### k = 2 ######
################### 

#### Get urine output at k=2 (closest value prior to aki kdigo3 + 24 hours)
temp <- merge(x=kdigo_stages %>% select(icustay_id, charttime, uo_rt_24hr),
      y=dat %>% select(icustay_id, charttime.kdigo3), by="icustay_id", all.x = TRUE) %>%
        filter(icustay_id %in% dat$icustay_id) %>%
        group_by(icustay_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki>0 & lab_to_aki<24 & !is.na(lab_to_aki) & !is.na(uo_rt_24hr)) %>%
        arrange(icustay_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(uo_k2 = uo_rt_24hr, charttime.uo_k2 = charttime.lab) %>%
        select(c(icustay_id, charttime.uo_k2, uo_k2))
        
dat <- merge(x=dat, y=temp, by="icustay_id", all.x = TRUE)

#### Get bun at k=2 (closest value prior to aki kdigo3 + 24 hours)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki>0 & lab_to_aki<24 & !is.na(lab_to_aki) & !is.na(BUN)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(bun_k2 = BUN, charttime.bun.k2 = charttime.lab) %>%
        select(c(hadm_id, charttime.bun.k2, bun_k2))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)

#### Get pH at k=2 (closest value prior to aki kdigo3 + 24 hours)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki>0 & lab_to_aki<24 & !is.na(lab_to_aki) & !is.na(PH)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(ph_k2 = PH, charttime.ph.k2 = charttime.lab) %>%
        select(c(hadm_id, charttime.ph.k2, ph_k2))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)

#### Get POTASSIUM at k=2 (closest value prior to aki kdigo3 + 24 hours)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki>0 & lab_to_aki<24 & !is.na(lab_to_aki) & !is.na(POTASSIUM)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(pot_k2 = POTASSIUM, charttime.pot.k2 = charttime.lab) %>%
        select(c(hadm_id, charttime.pot.k2, pot_k2))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)


###################
###### k = 3 ######
################### 

#### Get urine output at k=3 (closest value prior to aki kdigo3 + 48 hours with > 24 h)
temp <- merge(x=kdigo_stages %>% select(icustay_id, charttime, uo_rt_24hr),
              y=dat %>% select(icustay_id, charttime.kdigo3), by="icustay_id", all.x = TRUE) %>%
        filter(icustay_id %in% dat$icustay_id) %>%
        group_by(icustay_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki>24 & lab_to_aki<48 & !is.na(lab_to_aki) & !is.na(uo_rt_24hr)) %>%
        arrange(icustay_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(uo_k3 = uo_rt_24hr, charttime.uo_k3 = charttime.lab) %>%
        select(c(icustay_id, charttime.uo_k3, uo_k3))

dat <- merge(x=dat, y=temp, by="icustay_id", all.x = TRUE)

#### Get bun at k=3 (closest value prior to aki kdigo3 + 48 hours with > 24 h)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki>24 & lab_to_aki<48 & !is.na(lab_to_aki) & !is.na(BUN)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(bun_k3 = BUN, charttime.bun.k3 = charttime.lab) %>%
        select(c(hadm_id, charttime.bun.k3, bun_k3))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)

#### Get pH at k=3 (closest value prior to aki kdigo3 + 48 hours with > 24 h)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        filter(lab_to_aki>24 & lab_to_aki<48 & !is.na(lab_to_aki) & !is.na(PH)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(ph_k3 = PH, charttime.ph.k3 = charttime.lab) %>%
        select(c(hadm_id, charttime.ph.k3, ph_k3))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)

#### Get POTASSIUM at k=3 (closest value prior to aki kdigo3 + 48 hours with > 24 h)
temp <- merge(x=lab, y=dat %>% select(hadm_id, charttime.kdigo3), by="hadm_id", all.x = TRUE) %>%
        group_by(hadm_id) %>% 
        mutate(charttime.lab=as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%OS"),
               lab_to_aki=difftime(charttime.lab, charttime.kdigo3, unit="hours")) %>%
        arrange(hadm_id, lab_to_aki) %>% 
        filter(lab_to_aki>24 & lab_to_aki<48 & !is.na(lab_to_aki) & !is.na(POTASSIUM)) %>%
        arrange(hadm_id, desc(lab_to_aki)) %>%
        filter(row_number()==1) %>% ungroup() %>%
        rename(pot_k3 = POTASSIUM, charttime.pot.k3 = charttime.lab) %>%
        select(c(hadm_id, charttime.pot.k3, pot_k3))

dat <- merge(x=dat, y=temp, by="hadm_id", all.x = TRUE)


#######################
###### Create As ###### 
#######################

dat$a1 <- ifelse(dat$aki_to_rrt_hours < 24, 1, 0)
dat$a2 <- ifelse(dat$aki_to_rrt_hours >= 24 & dat$aki_to_rrt_hours < 48, 1, 0)
dat$a3 <- ifelse(dat$aki_to_rrt_hours >= 48 & dat$aki_to_rrt_hours < 72, 1, 0)

dat$a2 <- pmax(dat$a2, dat$a1)
dat$a3 <- pmax(dat$a3, dat$a2)

dat$a1[is.na(dat$a1)] <- 0
dat$a2[is.na(dat$a2)] <- 0
dat$a3[is.na(dat$a3)] <- 0

#################################################
###### Create Hospital free days at day 60 ###### 
#################################################
h <- 60 # change horizon here if needed

dat$horizon <- dat$charttime.kdigo3 + h *3600*24 # convert 60 days in seconds
dat$hfd <- as.numeric(difftime(dat$horizon, dat$dischtime, units = "days"))
dat$hfd <- ifelse(dat$hospital_expire_flag == 1, 0, dat$hfd)
dat$hfd <- ifelse(dat$hfd<0 | dat$hfd >h, 0, dat$hfd)

# get length of readmission in the kddio3 to horizon window
temp <- merge(x = admissions %>% rename(subject_id = SUBJECT_ID) %>% filter(subject_id %in% dat$subject_id), y=dat, by="subject_id", all.x= TRUE) %>%
        mutate(readmission = ADMITTIME < horizon & ADMITTIME > dischtime) %>% # boolean for readmission between discharge and horizon (day 60)
        filter(HADM_ID != hadm_id, readmission == TRUE) %>%
        mutate(discharge_or_horizon = ifelse(DISCHTIME>horizon, horizon, DISCHTIME) ) %>%
        mutate(ADMITTIME=as.POSIXct(ADMITTIME, format="%Y-%m-%d %H:%M:%OS"),
               discharge_or_horizon=as.POSIXct(discharge_or_horizon, format="%Y-%m-%d %H:%M:%OS")) %>%
        mutate(lo_read = as.numeric(difftime(discharge_or_horizon, ADMITTIME, units = "days")) ) %>% # length of readmission to discharge or horizon
        group_by(subject_id) %>% mutate(sum_lo_read = sum(lo_read) ) %>%
        filter(row_number()==1) %>% select(subject_id, sum_lo_read)

dat <- merge(dat, temp, by="subject_id", all.x = TRUE)
dat$sum_lo_read[is.na(dat$sum_lo_read)] <- 0
dat$hfd <- dat$hfd - dat$sum_lo_read

####################################
###### Create/Export Dataset  ###### 
####################################

# get immunosuppressant variable
dat$immunosuppressant <- !is.na(dat$drug)

# correct pseudo anonymized ages (some ages are greater than 250)
dat$admission_age <- pmin(dat$admission_age, 90)

# correct outlier weights greater than 500 kg to 100 kg (three patients)
dat$weight <- ifelse(dat$weight>500, 100, dat$weight)

# correct outliers with negative urine output (nine patients)
dat$uo_k1 <- pmax(dat$uo_k1, 0)
dat$uo_k2 <- pmax(dat$uo_k2, 0)

# remove patients who died before day 3

sel_col <- c("subject_id","hadm_id","icustay_id","charttime.kdigo3", "aki_stage_creat", "aki_stage_uo", # admin
  "admission_age", "ethnicity_grouped", "gender", "weight", # baseline characteristics
  "charttime.creat.k1", "creat_k1", "SOFA_24hours", "immunosuppressant", # baseline characteristics
  "charttime.uo_k1", "uo_k1", "charttime.bun.k1", "bun_k1", "charttime.ph.k1", "ph_k1", "charttime.pot.k1", "pot_k1", # baseline characteristics
  "charttime.uo_k2", "uo_k2", "charttime.bun.k2", "bun_k2", "charttime.ph.k2", "ph_k2", "charttime.pot.k2", "pot_k2", # characteristics at k = 2
  "charttime.uo_k3", "uo_k3", "charttime.bun.k3", "bun_k3", "charttime.ph.k3", "ph_k3", "charttime.pot.k3", "pot_k3", # characteristics at k = 3
  "a1", "a2", "a3", # RRT treatment at k=1,2,3
  "dod", "hfd") # outcomes

exp_dat <- dat[, sel_col]

# select only the patients who survived more than three days
td_surv <- dat$aki_to_death_days>3 | is.na(dat$aki_to_death_days)

exp_dat <- exp_dat[td_surv, ]

ok_na_var <- c("aki_stage_creat", "aki_stage_uo", "dod")
missing_matters <- !sel_col %in% ok_na_var

# create complete case dataset for illustration purposes
exp_dat_complete <- exp_dat[complete.cases(exp_dat[, missing_matters]), ]
#write.csv(x = exp_dat_complete, "mimic_cc.csv", row.names=FALSE)


# Multiple imputation assuming MAR
library(mice)
exp_dat$ethnicity_grouped <- as.factor(exp_dat$ethnicity_grouped)
exp_dat$gender <- as.factor(exp_dat$gender)
exp_dat$immunosuppressant <- as.factor(exp_dat$immunosuppressant)
exp_dat$a1 <- as.factor(exp_dat$a1)
exp_dat$a2 <- as.factor(exp_dat$a2)
exp_dat$a3 <- as.factor(exp_dat$a3)

imp <- mice(exp_dat, maxit=0)
predM <- imp$predictorMatrix
meth <- imp$method

no_predictor_var <- c("subject_id", "hadm_id", "icustay_id", "charttime.kdigo3",
                      "aki_stage_creat", "aki_stage_uo", "charttime.creat.k1",
                      "charttime.uo_k1", "charttime.uo_k2", "charttime.uo_k3",
                      "charttime.bun.k1", "charttime.bun.k2", "charttime.bun.k3",
                      "charttime.ph.k1", "charttime.ph.k2", "charttime.ph.k3",
                      "charttime.pot.k1", "charttime.pot.k2", "charttime.pot.k3",
                      "charttime.uo_k1", "charttime.uo_k2", "charttime.uo_k3",
                      "dod"
                      )
predM[, no_predictor_var] <- 0 # do not use no_predictor_var as predictors of other missing variables

do_not_impute_var <- c("dod", "subject_id", "hadm_id", "icustay_id", "charttime.kdigo3",
                       "aki_stage_creat", "aki_stage_uo", "charttime.creat.k1",
                       "charttime.uo_k1", "charttime.uo_k2", "charttime.uo_k3",
                       "charttime.bun.k1", "charttime.bun.k2", "charttime.bun.k3",
                       "charttime.ph.k1", "charttime.ph.k2", "charttime.ph.k3",
                       "charttime.pot.k1", "charttime.pot.k2", "charttime.pot.k3",
                       "charttime.uo_k1", "charttime.uo_k2", "charttime.uo_k3")

meth[do_not_impute_var] <- "" # do not impute the do_not_impute_var

exp_dat <- mice(exp_dat, m=100, seed = 1, predictorMatrix = predM, method = meth) # Change No. of imputations here
exp_dat_simp <- complete(exp_dat, 1)
save(exp_dat, file="imputed_mimic_dtr.RData")
#write.csv(x = exp_dat_simp, "mimic_si.csv", row.names=FALSE)

