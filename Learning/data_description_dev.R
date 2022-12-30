library(dplyr)
library(Hmisc)
library(naniar)

load("/Users/francois/Desktop/github repos/MIMIC-DTR/imputed_mimic_dtr.RData")

dev_dat <- exp_dat$data %>% mutate(kdigo_creat = aki_stage_creat == 3 & !is.na(aki_stage_creat)) %>%
                        select(kdigo_creat, admission_age, gender, weight, immunosuppressant, SOFA_24hours,
                        creat_k1, bun_k1, bun_k2, bun_k3, pot_k1, pot_k2, pot_k3,
                        ph_k1, ph_k2, ph_k3, uo_k1, uo_k2, uo_k3)
        

gg_miss_upset(dev_dat, nsets = n_var_miss(dev_dat), nintersects = 15)
naplot(naclus(dev_dat), which=c('na per var'))

table_1 <- tableone::CreateTableOne(vars=colnames(dev_dat), data=dev_dat)
printed_t_1 <- print(table_1, nonnormal = colnames(dev_dat), catDigits=1, contDigits=2, noSpaces=TRUE)
printed_t_1
#write.csv(printed_t_1, file = "table1.csv")

dev.new(noRStudioGD = TRUE)
par(mfrow=c(4,5))
for(i in 1:ncol(dev_dat)){
        if(is.numeric(dev_dat[,i])){
        hist(dev_dat[,i], main = colnames(dev_dat)[i])}
        }


#### Get hospital mortality in the patients included in the development set

admissions <- read.csv("~/Desktop/TEAM METHODS/phd/Mimic/extracted_data/admissions.csv")

mortality_dev <- merge(x=exp_dat$data["hadm_id"], y=admissions %>% select(hadm_id=HADM_ID, HOSPITAL_EXPIRE_FLAG), all.x = TRUE, by="hadm_id")
deaths <- as.numeric(table(mortality_dev$HOSPITAL_EXPIRE_FLAG)['1'])
n_included <- nrow(exp_dat$data)
deaths; n_included
deaths / n_included

