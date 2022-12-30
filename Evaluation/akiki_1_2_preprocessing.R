library(dplyr)
library(mice)

##################################################################################################################################
##################################################################################################################################
############################################################ AKIKI 1 #############################################################
##################################################################################################################################
##################################################################################################################################

akikidbsg <- read.csv("~/Desktop/TEAM METHODS/phd/Trials Data/akiki data/SGakiki.csv", sep=";", comment.char="#")
ak_dat_1<-data.frame(
        study="AKIKI1",
        SUBJECT_ID=substring(akikidbsg$subject_id, 5, nchar(akikidbsg$subject_id)-3),
        DATERANDO=as.POSIXct(akikidbsg$inc_daterando, tryFormats = "%d%b%Y:%H:%M:%OS"),
        BRASRANDO=akikidbsg$inc_bras, 
        DATEINC=as.POSIXct(akikidbsg$inc_datecent22, tryFormats = "%d%b%Y:%H:%M:%OS"),
        DATENAISS=as.Date(format(as.Date(akikidbsg$inc_datenaiss, "%d/%m/%Y"), "19%y-%m-%d")), 
        SEXEPAT=akikidbsg$inc_sexepat,
        POIDS=akikidbsg$inc_poids,
        COMTTIMMUNOSUP=akikidbsg$adm_com_ttimmunosup,
        J0_SCOSOFASUM=akikidbsg$rando_car_sofa_sum,
        EER_SEADATE=as.POSIXct(akikidbsg$eer_date, tryFormats = "%d%b%Y:%H:%M:%OS"),
        creat_k1=akikidbsg$rando_car_creat,
        creat_k2=akikidbsg$j0_car_creatmax,
        creat_k3=akikidbsg$j1_car_creatmax,
        bun_k1=akikidbsg$rando_car_uree,
        bun_k2=akikidbsg$j0_car_ureemax,
        bun_k3=akikidbsg$j1_car_ureemax,
        pot_k1=akikidbsg$rando_car_pot,
        pot_k2=akikidbsg$j0_car_potmax,
        pot_k3=akikidbsg$j1_car_potmax,
        ph_k1=akikidbsg$rando_car_ph,
        ph_k2=akikidbsg$j0_car_phmin,
        ph_k3=akikidbsg$j1_car_phmin,
        uo_k1=akikidbsg$j0_car_diures,
        uo_k2=akikidbsg$j1_car_diures,
        uo_k3=akikidbsg$j2_car_diures,
        HOP_SORTIEDATE=as.Date(format(as.Date(akikidbsg$bil_sortiedate, "%d/%m/%Y"), "20%y-%m-%d")),
        hospital_mortality=akikidbsg$bil_sortieetat=="D̩c̩d̩",
        kdigo_creat=akikidbsg$inc_ci4a=="OUI" | is.na(akikidbsg$inc_ci4a)
)

## Create age
ak_dat_1$AGE <- with(ak_dat_1, as.numeric(difftime(DATERANDO, DATENAISS , unit="days")))/365.25

###################################################
###### Get survival data as in NEJM Figure 1 ###### 
###################################################

load("akiki1_survival.RData")
temp <- data.frame(SUBJECT_ID=substring(dat$id, 5, nchar(dat$id)-3), t_d60=dat$derniere.nouvelles.censureJ60, status_d60=dat$etat.censureJ60)

ak_dat_1 <- merge(x = ak_dat_1, temp, by="SUBJECT_ID")
ak_dat_1$time_to_death <- NA
ak_dat_1$time_to_death[temp$status_d60==1] <- temp$t_d60[temp$status_d60==1] 

#######################
###### Create As ###### 
#######################

ak_dat_1$aki_to_rrt_hours <- as.numeric(with(ak_dat_1, difftime(EER_SEADATE, DATERANDO, units = "hours")))

ak_dat_1$a1 <- ifelse(ak_dat_1$aki_to_rrt_hours < 24, 1, 0)
ak_dat_1$a2 <- ifelse(ak_dat_1$aki_to_rrt_hours >= 24 & ak_dat_1$aki_to_rrt_hours < 48, 1, 0)
ak_dat_1$a3 <- ifelse(ak_dat_1$aki_to_rrt_hours >= 48 & ak_dat_1$aki_to_rrt_hours < 72, 1, 0)

ak_dat_1$a2 <- pmax(ak_dat_1$a2, ak_dat_1$a1)
ak_dat_1$a3 <- pmax(ak_dat_1$a3, ak_dat_1$a2)

ak_dat_1$a1[is.na(ak_dat_1$a1)] <- 0
ak_dat_1$a2[is.na(ak_dat_1$a2)] <- 0
ak_dat_1$a3[is.na(ak_dat_1$a3)] <- 0  

######################################################
############### Create Terminal States ###############
######################################################

ak_dat_1$PHI_1 <- 0
ak_dat_1$PHI_2 <- 0
ak_dat_1$PHI_3 <- 0

ak_dat_1$PHI_2[!is.na(ak_dat_1$time_to_death) & ak_dat_1$time_to_death<1] <- 1
ak_dat_1$PHI_3[!is.na(ak_dat_1$time_to_death) & ak_dat_1$time_to_death<1] <- 1

ak_dat_1$PHI_3[!is.na(ak_dat_1$time_to_death) & ak_dat_1$time_to_death>=1 & ak_dat_1$time_to_death<2] <- 1


#####################################################
######## Create Hospital Free Days at Day 60 ########
##################################################### 

# Create time_to_hospi_discharge variable
ak_dat_1$time_to_hospi_discharge <- as.numeric(with(ak_dat_1, difftime(HOP_SORTIEDATE, DATERANDO, unit="days")))
# fix the patients who appear to have left the hospital before randomization
ak_dat_1$time_to_hospi_discharge[which(ak_dat_1$time_to_hospi_discharge<0)] <- 0 

ak_dat_1$HFD60 <- NA

# The patients who died after hospital discharge AND within 60 days 
# are allocated hospital free days of time_to_death-time_to_hospi_discharge 
# i.e., the time they spent alive and outside the hospital before their death
ak_dat_1$death_within_60d_outside_h <- with(ak_dat_1, (!is.na(time_to_hospi_discharge) & !is.na(time_to_death) & time_to_death > time_to_hospi_discharge) & time_to_death <= 60 )
ak_dat_1$HFD60[ak_dat_1$death_within_60d_outside_h] <- with(ak_dat_1[ak_dat_1$death_within_60d_outside_h,], time_to_death-time_to_hospi_discharge)

# The patients who died after 60 days or had not died at the end of follow up
# are allocated hospital free days of 60-time_to_hospi_discharge 
# i.e., the time they spent alive and outside the hospital before day 60
ak_dat_1$death_after_d60 <- with(ak_dat_1, (is.na(time_to_death) | time_to_death > 60))
ak_dat_1$HFD60[ak_dat_1$death_after_d60] <- with(ak_dat_1[ak_dat_1$death_after_d60,], 60 - time_to_hospi_discharge)

# The patients who left the hospital after 60 days 
# are allocated hospital free days of zero 
ak_dat_1$HFD60[ak_dat_1$time_to_hospi_discharge>60] <- 0

# The patients who left the hospital after 60 days (at an unknown date)
# are allocated hospital free days of zero 
ak_dat_1$HFD60[is.na(ak_dat_1$HFD60)] <- 0 

# The patients who died in the hospital or 
# had not left the hospital at the end of followup (i.e., had no hospital_mortality is NA)
# are allocated hospital free days of zero 

ak_dat_1$HFD60[is.na(ak_dat_1$hospital_mortality) | ak_dat_1$hospital_mortality] <- 0 

# Convert urine output in ml/kg/h
ak_dat_1[, c("uo_k1", "uo_k2", "uo_k3")] <- ak_dat_1[, c("uo_k1", "uo_k2", "uo_k3")]/ak_dat_1$POIDS/24

# Convert creatinine at baseline in ml/dL
ak_dat_1$creat_k1 <- ak_dat_1$creat_k1/88.4

# Give study name
ak_dat_1$study <- "AKIKI1"

# Exclude the patients randomized to the "early RRT strategy"
akiki_1_prep <- ak_dat_1 %>% filter(BRASRANDO != "STRATEGIE PRECOCE") %>%   
        # Select the relevant columns  
        select(SUBJECT_ID, study, kdigo_creat, BRASRANDO, AGE, SEXEPAT, POIDS, COMTTIMMUNOSUP, J0_SCOSOFASUM, 
               creat_k1, creat_k2, creat_k3, bun_k1, bun_k2, bun_k3,
               pot_k1, pot_k2, pot_k3, ph_k1, ph_k2, ph_k3, 
               uo_k1, uo_k2, uo_k3,
               a1, a2, a3, PHI_1, PHI_2, PHI_3, HFD60) %>% 
        # rename these columns
        rename(orig_id=SUBJECT_ID, gender = SEXEPAT, admission_age = AGE, weight=POIDS,
               immunosuppressant=COMTTIMMUNOSUP, hfd = HFD60, SOFA_24hours=J0_SCOSOFASUM) %>% rename_with(tolower)

akiki_1_prep$hmor <- as.numeric((ak_dat_1 %>% filter(BRASRANDO != "STRATEGIE PRECOCE") %>% select(hospital_mortality))$hospital_mortality)
akiki_1_prep$hmor[is.na(akiki_1_prep$hmor)] <- 0

##################################################################################################################################
##################################################################################################################################
############################################################ AKIKI 2 #############################################################
##################################################################################################################################
##################################################################################################################################

INC <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/INC_ANSI.csv",
                sep=";")

ADMISREA <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/ADMISREA_ANSI.csv",
                     sep=";")

DQR <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/DQR_ANSI.csv",
                sep=";")

DQJ <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/DQJ_ANSI.csv",
                sep=";")

EER_SEA <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/EER_ANSI.csv",
                    sep=";")

RDM <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/RDM_ANSI.csv",
                sep=";")

HOP <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/HOP_ANSI.csv",
                sep=";")

BFE <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/BFE_ANSI.csv",
                sep=";")

no_dateinc <- INC$IN_DATEINC=="" # Exclude 9 patients with no inclusion date ## NB: We will later exclude the 5 patients who withdrew consent (and have missing dat)
ak_dat <- data.frame(SUBJECT_ID=INC$SUBJECT_ID[!no_dateinc])

ak_dat$kdigo_creat <- (INC$IN_CI4A==1 | is.na(INC$IN_CI4A) )[!no_dateinc]
ak_dat$DATERANDO <- RDM$RDM_DATERANDO[!no_dateinc]

ak_dat$BRASRANDO <- RDM$RDM_BRASRANDO[!no_dateinc]
ak_dat$BRASRANDO[ak_dat$BRASRANDO==1 & !is.na(ak_dat$BRASRANDO)] <- "standard"
ak_dat$BRASRANDO[ak_dat$BRASRANDO==2 & !is.na(ak_dat$BRASRANDO)] <- "grande_attente"

ak_dat$DATEINC <- as.POSIXct(INC$IN_DATEINC[!no_dateinc], tryFormats = "%d%b%Y:%H:%M:%OS") 
ak_dat$DATENAISS <- as.POSIXct(with(INC[!no_dateinc,], paste0(IN_DATENAISS_Y, "-", IN_DATENAISS_M, "-01")), tryFormats = "%Y-%m-%d")
ak_dat$AGE <-with(ak_dat, as.numeric(difftime(DATEINC, DATENAISS , unit="days")))/365.25
ak_dat$SEXEPAT <- ifelse(INC$IN_SEXEPAT[!no_dateinc]==1, "M", "F")
ak_dat$POIDS <- ADMISREA$ADM_POIDS_V[!no_dateinc]
ak_dat$COMTTIMMUNOSUP <- ADMISREA$ADM_COMTTIMMUNOSUP[!no_dateinc]

ak_dat$J0_SCOSOFASUM <- DQJ$J0_SCOSOFASUM[!no_dateinc]

ak_dat$J0_CCBCREATMAX <- DQJ$J0_CCBCREATMAX[!no_dateinc]
ak_dat$J0_CCBUREEMAX <- DQJ$J0_CCBUREEMAX[!no_dateinc]
ak_dat$J0_CCBPOTMAX <- DQJ$J0_CCBPOTMAX[!no_dateinc]
ak_dat$J0_CCBPHMIN <- DQJ$J0_CCBPHMIN[!no_dateinc]
ak_dat$J0_CCBDIURES <- DQJ$J0_CCBDIURES_V[!no_dateinc]

ak_dat$J1_CCBCREATMAX <- DQJ$J1_CCBCREATMAX[!no_dateinc]
ak_dat$J1_CCBUREEMAX <- DQJ$J1_CCBUREEMAX[!no_dateinc]
ak_dat$J1_CCBPOTMAX <- DQJ$J1_CCBPOTMAX[!no_dateinc]
ak_dat$J1_CCBPHMIN <- DQJ$J1_CCBPHMIN[!no_dateinc]
ak_dat$J1_CCBDIURES <- DQJ$J1_CCBDIURES_V[!no_dateinc]

ak_dat$J2_CCBCREATMAX <- DQJ$J2_CCBCREATMAX[!no_dateinc]
ak_dat$J2_CCBUREEMAX <- DQJ$J2_CCBUREEMAX[!no_dateinc]
ak_dat$J2_CCBPOTMAX <- DQJ$J2_CCBPOTMAX[!no_dateinc]
ak_dat$J2_CCBPHMIN <- DQJ$J2_CCBPHMIN[!no_dateinc]
ak_dat$J2_CCBDIURES <- DQJ$J2_CCBDIURES_V[!no_dateinc]

#####

ak_dat$R0_SCOSOFASUM <- DQR$R0_SCOSOFASUM[!no_dateinc]
ak_dat$R0_CCBCREATMAX <- DQR$R0_CCBCREATMAX[!no_dateinc]
ak_dat$R0_CCBUREEMAX <- DQR$R0_CCBUREEMAX[!no_dateinc]
ak_dat$R0_CCBPOTMAX <- DQR$R0_CCBPOTMAX[!no_dateinc]
ak_dat$R0_CCBPHMIN <- DQR$R0_CCBPHMIN[!no_dateinc]
ak_dat$R0_CCBDIURES <- DQR$R0_CCBDIURES_V[!no_dateinc]

ak_dat$R1_CCBCREATMAX <- DQR$R1_CCBCREATMAX[!no_dateinc]
ak_dat$R1_CCBUREEMAX <- DQR$R1_CCBUREEMAX[!no_dateinc]
ak_dat$R1_CCBPOTMAX <- DQR$R1_CCBPOTMAX[!no_dateinc]
ak_dat$R1_CCBPHMIN <- DQR$R1_CCBPHMIN[!no_dateinc]
ak_dat$R1_CCBDIURES <- DQR$R1_CCBDIURES_V[!no_dateinc]

ak_dat$R2_CCBCREATMAX <- DQR$R2_CCBCREATMAX[!no_dateinc]
ak_dat$R2_CCBUREEMAX <- DQR$R2_CCBUREEMAX[!no_dateinc]
ak_dat$R2_CCBPOTMAX <- DQR$R2_CCBPOTMAX[!no_dateinc]
ak_dat$R2_CCBPHMIN <- DQR$R2_CCBPHMIN[!no_dateinc]
ak_dat$R2_CCBDIURES <- DQR$R2_CCBDIURES_V[!no_dateinc]

#####

#####

ak_dat$DATESORTIE <- as.Date(HOP$HOP_DATESORTIE[!no_dateinc], format = "%d/%m/%Y")
ak_dat$HOP_SV <- HOP$HOP_SV[!no_dateinc]
ak_dat$hospital_mortality <- NA
ak_dat$hospital_mortality[which(ak_dat$HOP_SV==2)] <- TRUE
ak_dat$hospital_mortality[which(ak_dat$HOP_SV==1)] <- FALSE

ak_dat$EER_SEADATE <- EER_SEA$EER_SEADATE[!no_dateinc]
ak_dat$EER_SEADATE[ak_dat$EER_SEADATE==""] <- NA
ak_dat$EER_SEADATE <- as.POSIXct(ak_dat$EER_SEADATE, tryFormats = "%d%b%Y:%H:%M:%OS") 

#######################
###### Create As ###### 
#######################
ak_dat$aki_to_rrt_hours <- NA
ak_dat$aki_to_rrt_hours <- as.numeric(difftime(ak_dat$EER_SEADATE, ak_dat$DATEINC, unit="hours"))

ak_dat$a1 <- ifelse(ak_dat$aki_to_rrt_hours < 24, 1, 0)
ak_dat$a2 <- ifelse(ak_dat$aki_to_rrt_hours >= 24 & ak_dat$aki_to_rrt_hours < 48, 1, 0)
ak_dat$a3 <- ifelse(ak_dat$aki_to_rrt_hours >= 48 & ak_dat$aki_to_rrt_hours < 72, 1, 0)

ak_dat$a2 <- pmax(ak_dat$a2, ak_dat$a1)
ak_dat$a3 <- pmax(ak_dat$a3, ak_dat$a2)

ak_dat$a1[is.na(ak_dat$a1)] <- 0
ak_dat$a2[is.na(ak_dat$a2)] <- 0
ak_dat$a3[is.na(ak_dat$a3)] <- 0

### Get time from inclusion to randomization
ak_dat$aki_to_rando_hours <- NA
ak_dat$aki_to_rando_hours[ak_dat$DATERANDO!=""] <- as.numeric(difftime(as.POSIXct(ak_dat$DATERANDO[ak_dat$DATERANDO!=""], tryFormats = "%d%b%Y:%H:%M:%OS"), ak_dat$DATEINC[ak_dat$DATERANDO!=""], unit="hours"))

ak_dat$aki_to_rando_hours <- pmax(ak_dat$aki_to_rando_hours, 0) # This fixes the one patient who appear to have been randomized 4 hours prior to ... inclusion

###########################
###### Formating k's ######
########################### 

# See document "xxx.png" attached 

condition_0_24 <- !is.na(ak_dat$aki_to_rando_hours) & ak_dat$aki_to_rando_hours < 24

ak_dat$creat_k1 <- as.numeric(ak_dat$J0_CCBCREATMAX)
ak_dat$creat_k2 <- as.numeric(ifelse(condition_0_24, ak_dat$R0_CCBCREATMAX, ak_dat$J1_CCBCREATMAX))
ak_dat$creat_k3 <- as.numeric(ifelse(condition_0_24, ak_dat$R1_CCBCREATMAX, ak_dat$J2_CCBCREATMAX))

ak_dat$bun_k1 <- as.numeric(ak_dat$J0_CCBUREEMAX)
ak_dat$bun_k2 <- as.numeric(ifelse(condition_0_24, ak_dat$R0_CCBUREEMAX, ak_dat$J1_CCBUREEMAX))
ak_dat$bun_k3 <- as.numeric(ifelse(condition_0_24, ak_dat$R1_CCBUREEMAX, ak_dat$J2_CCBUREEMAX))

ak_dat$pot_k1 <- as.numeric(ak_dat$J0_CCBPOTMAX)
ak_dat$pot_k2 <- as.numeric(ifelse(condition_0_24, ak_dat$R0_CCBPOTMAX, ak_dat$J1_CCBPOTMAX))
ak_dat$pot_k3 <- as.numeric(ifelse(condition_0_24, ak_dat$R1_CCBPOTMAX, ak_dat$J2_CCBPOTMAX))

ak_dat$ph_k1 <- as.numeric(ak_dat$J0_CCBPHMIN)
ak_dat$ph_k2 <- as.numeric(ifelse(condition_0_24, ak_dat$R0_CCBPHMIN, ak_dat$J1_CCBPHMIN))
ak_dat$ph_k3 <- as.numeric(ifelse(condition_0_24, ak_dat$R1_CCBPHMIN, ak_dat$J2_CCBPHMIN))

ak_dat$uo_k1 <- as.numeric(ak_dat$J0_CCBDIURES)
ak_dat$uo_k2 <- as.numeric(ifelse(condition_0_24, ak_dat$R0_CCBDIURES, ak_dat$J1_CCBDIURES))
ak_dat$uo_k3 <- as.numeric(ifelse(condition_0_24, ak_dat$R1_CCBDIURES, ak_dat$J2_CCBDIURES))

####

condition_24_48 <- !is.na(ak_dat$aki_to_rando_hours) & ak_dat$aki_to_rando_hours >= 24 & ak_dat$aki_to_rando_hours < 48

ak_dat$creat_k3[condition_24_48] <- as.numeric(ak_dat$R0_CCBCREATMAX)[condition_24_48] 
ak_dat$bun_k3[condition_24_48] <- as.numeric(ak_dat$R0_CCBUREEMAX)[condition_24_48] 
ak_dat$pot_k3[condition_24_48] <- as.numeric(ak_dat$R0_CCBPOTMAX)[condition_24_48] 
ak_dat$ph_k3[condition_24_48] <- as.numeric(ak_dat$R0_CCBPHMIN)[condition_24_48] 
ak_dat$uo_k3[condition_24_48] <- as.numeric(ak_dat$R0_CCBDIURES)[condition_24_48] 


#####################################################
######## Create Hospital Free Days at Day 60 ########
##################################################### 

suivi_J60 <- as.numeric(BFE$BFE_SUIVI1)[!no_dateinc]
suivi_R60 <- as.numeric(BFE$BFE_SUIVI2)[!no_dateinc]
suivi_60 <- ifelse(is.na(suivi_J60), suivi_R60, suivi_J60)

date_fin <- BFE$BFE_DATEFIN[!no_dateinc]

temp <- data.frame(SUBJECT_ID=BFE$SUBJECT_ID[!no_dateinc], suivi_60=suivi_60, motif_arret=BFE$BFE_MOTIFARRET[!no_dateinc], date_inc=ak_dat$DATEINC, date_sortie_hospi=ak_dat$DATESORTIE,  hospital_mortality=ak_dat$hospital_mortality, date_fin=date_fin)

temp$date_of_death <- NA
temp$date_of_death[which(temp$motif_arret==1)] <- temp$date_fin[which(temp$motif_arret==1)]
temp$date_of_death <- as.POSIXct(temp$date_of_death, format = "%d/%m/%Y") 

ak_dat <- ak_dat[which(!temp$motif_arret %in% c(3,4)),] # Exclude the 5 patients who withdrew consent (and have missing data)
temp <- temp[which(!temp$motif_arret %in% c(3,4)),]

# Create time_to death and time_to_hospi_discharge variables
temp$time_to_death <- as.numeric(with(temp, difftime(date_of_death, date_inc, unit="days")))
temp$time_to_hospi_discharge <- as.numeric(with(temp, difftime(date_sortie_hospi, date_inc, unit="days")))

temp$HFD60 <- NA

# The patients who died in the hospital or 
# had not left the hospital at the end of followup (i.e., had no hospital_mortality is NA)
# are allocated hospital free days of zero 

temp$HFD60[is.na(temp$hospital_mortality) | temp$hospital_mortality] <- 0 

# The patients who died after hospital discharge AND within 60 days 
# are allocated hospital free days of time_to_death-time_to_hospi_discharge 
# i.e., the time they spent alive and outside the hospital before their death
temp$death_within_60d_outside_h <- with(temp, (!is.na(time_to_hospi_discharge) & !is.na(time_to_death) & time_to_death > time_to_hospi_discharge) & time_to_death <= 60 )
temp$HFD60[temp$death_within_60d_outside_h] <- with(temp[temp$death_within_60d_outside_h,], time_to_death-time_to_hospi_discharge)

# The patients who died after 60 days or had not died at the end of follow up
# are allocated hospital free days of 60-time_to_hospi_discharge 
# i.e., the time they spent alive and outside the hospital before day 60
temp$death_after_d60 <- with(temp, (is.na(time_to_death) | time_to_death > 60))
temp$HFD60[temp$death_after_d60] <- with(temp[temp$death_after_d60,], 60 - time_to_hospi_discharge)

# The patients who left the hospital after 60 days 
# are allocated hospital free days of zero 
temp$HFD60[temp$time_to_hospi_discharge>60] <- 0

# The patients who left the hospital after 60 days 
# are allocated hospital free days of zero 
temp$HFD60[is.na(temp$HFD60)] <- 0 

# The patient who left the hospital alive at day 0
# is allocated hospital free days of 60 
temp$HFD60[temp$HFD60>60] <- 60 

# Get hospital mortality
ak_dat$hmor <- as.numeric(is.na(ak_dat$HOP_SV) | ak_dat$HOP_SV==2)

### Merge temp and ak_dat by SUBJECT_ID
ak_dat <- merge(x = ak_dat, y=temp[, c("SUBJECT_ID", "time_to_hospi_discharge", "time_to_death", "HFD60")], by="SUBJECT_ID")

######################################################
############### Create Terminal States ###############
######################################################

ak_dat$PHI_1 <- 0
ak_dat$PHI_2 <- 0
ak_dat$PHI_3 <- 0

ak_dat$PHI_2[!is.na(ak_dat$time_to_death) & ak_dat$time_to_death<1] <- 1
ak_dat$PHI_3[!is.na(ak_dat$time_to_death) & ak_dat$time_to_death<1] <- 1

ak_dat$PHI_3[!is.na(ak_dat$time_to_death) & ak_dat$time_to_death>=1 & ak_dat$time_to_death<2] <- 1

# Convert urine output in ml/kg/h
ak_dat[, c("uo_k1", "uo_k2", "uo_k3")] <- ak_dat[, c("uo_k1", "uo_k2", "uo_k3")]/ak_dat$POIDS/24

# Convert creatinine at baseline in ml/dL
ak_dat$creat_k1 <- ak_dat$creat_k1/88.4

# Give study name
ak_dat$study <- "AKIKI2"

##########
##########

# Exclude the patients randomized to the "more delayed RRT strategy"
akiki_2_prep <- ak_dat %>% filter(BRASRANDO != "grande_attente"| is.na(BRASRANDO)) %>%   
        # Select the relevant columns  
        select(orig_id=SUBJECT_ID, study, kdigo_creat, BRASRANDO, AGE, SEXEPAT, POIDS, COMTTIMMUNOSUP, J0_SCOSOFASUM, 
               creat_k1, creat_k2, creat_k3, bun_k1, bun_k2, bun_k3,
               pot_k1, pot_k2, pot_k3, ph_k1, ph_k2, ph_k3, 
               uo_k1, uo_k2, uo_k3,
               a1, a2, a3, PHI_1, PHI_2, PHI_3, HFD60, hmor) %>%
        # rename these columns
        rename(gender = SEXEPAT, admission_age = AGE, weight=POIDS,
               immunosuppressant=COMTTIMMUNOSUP, hfd = HFD60, SOFA_24hours=J0_SCOSOFASUM) %>% rename_with(tolower)

# Clone the patients randomized to the "standard RRT strategy"
akiki_2_prep <- rbind(akiki_2_prep, akiki_2_prep %>% filter(brasrando == "standard"))


######################################################
############ Merge & Impute the two bases ############
######################################################

val_dat_miss <- rbind(akiki_1_prep, akiki_2_prep)

# Give the each of these new patients an id
val_dat_miss <- val_dat_miss %>% mutate(subject_id=1:nrow(val_dat_miss))

val_dat_miss$study <- as.factor(val_dat_miss$study)
val_dat_miss$brasrando <- as.factor(val_dat_miss$brasrando)
val_dat_miss$immunosuppressant <- as.factor(val_dat_miss$immunosuppressant)

val_dat_miss$a1 <- as.factor(val_dat_miss$a1)
val_dat_miss$a2 <- as.factor(val_dat_miss$a2)
val_dat_miss$a3 <- as.factor(val_dat_miss$a3)

val_dat_miss$phi_1 <- as.factor(val_dat_miss$phi_1)
val_dat_miss$phi_2 <- as.factor(val_dat_miss$phi_2)
val_dat_miss$phi_3 <- as.factor(val_dat_miss$phi_3)

### convert BUN from mmol/L to mg/dL
val_dat_miss$bun_k1 <- val_dat_miss$bun_k1 / 0.3571
val_dat_miss$bun_k2 <- val_dat_miss$bun_k2 / 0.3571
val_dat_miss$bun_k3 <- val_dat_miss$bun_k3 / 0.3571
###

imp <- mice(val_dat_miss, maxit=0)
predM <- imp$predictorMatrix
meth <- imp$method

no_predictor_var <- c("brasrando")

predM[, no_predictor_var] <- 0 # do not use no_predictor_var as predictors of other missing variables

do_not_impute_var <- c("brasrando")

meth[do_not_impute_var] <- "" # do not impute the do_not_impute_var

val_dat <- mice(val_dat_miss, m=20, seed = 1, predictorMatrix = predM, method = meth) # Change No. of imputations here
val_dat_simp <- complete(val_dat, 1)

save(val_dat, file="imputed_val_dat_bunfix_hmor.RData")




#### Get hospital mortality in the validation set
hm_ak1 <- ak_dat_1 %>% filter(BRASRANDO != "STRATEGIE PRECOCE") %>% select(hospital_mortality)
akiki1_deaths <- table(hm_ak1)["TRUE"]

hm_ak2 <- ak_dat %>% filter(BRASRANDO != "grande_attente"| is.na(BRASRANDO)) 
hm_ak2 <- rbind(hm_ak2, ak_dat[ak_dat$BRASRANDO == "standard" & !is.na(ak_dat$BRASRANDO), ])
akiki2_deaths <- sum(table(hm_ak2$HOP_SV, useNA = "ifany")[c(2, 3)])

akiki1_deaths + akiki2_deaths
nrow(hm_ak1) + nrow(hm_ak2)
(akiki1_deaths + akiki2_deaths) / ( nrow(hm_ak1) + nrow(hm_ak2) )
####




