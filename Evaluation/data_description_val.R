library(dplyr)
library(Hmisc)
library(naniar)

load("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/imputed_val_dat.RData")

val_data_des <- val_dat$data %>% select(kdigo_creat, admission_age, gender, weight, immunosuppressant, sofa_24hours,
                                   creat_k1, bun_k1, bun_k2, bun_k3, pot_k1, pot_k2, pot_k3,
                                   ph_k1, ph_k2, ph_k3, uo_k1, uo_k2, uo_k3, study)

gg_miss_upset(val_data_des, nsets = n_var_miss(val_data_des), nintersects = 15)
naplot(naclus(val_data_des), which=c('na per var'))

val_data_des$immunosuppressant <- as.character(val_data_des$immunosuppressant)
val_data_des$immunosuppressant[val_data_des$immunosuppressant=="NON"] <- 0
val_data_des$immunosuppressant[val_data_des$immunosuppressant=="OUI"] <- 1

val_data_des$gender <- as.character(val_data_des$gender)
val_data_des$gender[val_data_des$gender=="Masculin"] <- "M"
val_data_des$gender[val_data_des$gender=="F̩minin"] <- "F"

table_1 <- tableone::CreateTableOne(vars=colnames(val_data_des)[1:(length(colnames(val_data_des))-1)], data=val_data_des, strata = c("study"), test = FALSE )
printed_t_1 <- print(table_1, catDigits=1, contDigits=2, noSpaces=TRUE)
#write.csv(printed_t_1, file = "table1_val.csv")

table_1 <- tableone::CreateTableOne(vars=colnames(val_data_des)[1:(length(colnames(val_data_des))-1)], data=val_data_des)
printed_t_1 <- print(table_1, nonnormal = colnames(val_data_des)[1:(length(colnames(val_data_des))-1)],
                     catDigits=1, contDigits=2, noSpaces=TRUE)
#write.csv(printed_t_1, file = "table1_val_merged.csv")


## Get those patients who had stage 3 KDIGO-AKI based on urine output
INC <- read.csv("/Users/francois/Desktop/TEAM\ METHODS/phd/Trials\ Data/akiki\ 2\ data/Données\ AKIKI2/INC_ANSI.csv",
                sep=";")
mg1 <- merge(x = val_dat$data, y=INC %>% select(orig_id=SUBJECT_ID, IN_CI4C), all.x = TRUE, by="orig_id")

akikidbsg <- read.csv("~/Desktop/TEAM METHODS/phd/Trials Data/akiki data/SGakiki.csv", sep=";", comment.char="#")
ak1 <- data.frame(orig_id=substring(akikidbsg$subject_id, 5, nchar(akikidbsg$subject_id)-3),
                  IN_CI4C=as.numeric(akikidbsg$inc_ci4c=='OUI'))

mged <- merge(mg1, ak1, all.x = TRUE, by="orig_id")

mged$uo_pos <-  apply(mged[c('IN_CI4C.x', 'IN_CI4C.y')], 1, function(x) sum(x, na.rm = TRUE))

prop.table(table(mged$uo_pos))*100
