library(dplyr)
library(mice)

load("imputed_val_dat_bunfix_hmor.RData")
load("ite_preds_coef.RData")
source("functions_val.R")

###### Get ITE predictions and their standard errors ###### 

mats_k123 <- list()
for(i in 1:val_dat$m)
{
        mats_k123[[i]] <- boot_dtr(d = complete(val_dat, i) %>% rename(SOFA_24hours=sofa_24hours), output = "mats")
        print(paste0(i, "/", val_dat$m, " datasets completed"))
}

mean_list_of_matrices <- function(x)
{
        Reduce("+", x)/ length(x)
}

mean_mat_k1 <- mean_list_of_matrices(lapply(mats_k123, function(x) x[[1]]))
mean_mat_k2 <- mean_list_of_matrices(lapply(mats_k123, function(x) x[[2]]))
mean_mat_k3 <- mean_list_of_matrices(lapply(mats_k123, function(x) x[[3]]))

ite_k1_val_ori <- mean_mat_k1 %*% est_k1
ite_k2_val_ori <- mean_mat_k2 %*% est_k2
ite_k3_val_ori <- mean_mat_k3 %*% est_k3

ite_se_k1_val_ori <- sqrt(sapply(as.list(as.data.frame(t(mean_mat_k1))), function(x) as.numeric(t(x) %*% vcv_k1 %*% x) ))
ite_se_k2_val_ori <- sqrt(sapply(as.list(as.data.frame(t(mean_mat_k2))), function(x) as.numeric(t(x) %*% vcv_k2 %*% x) ))
ite_se_k3_val_ori <- sqrt(sapply(as.list(as.data.frame(t(mean_mat_k3))), function(x) as.numeric(t(x) %*% vcv_k3 %*% x) ))

actual_ttts_val_ori <- boot_dtr(d = complete(val_dat, 1) %>% rename(SOFA_24hours=sofa_24hours), output = "ttt")
actual_ids_val_ori <- boot_dtr(d = complete(val_dat, 1) %>% rename(SOFA_24hours=sofa_24hours), output = "ids")


# Because the validation set contains data of patients who died within three days
# Remove predictions (ite_k#_val) & apparent decisions (a#) 
# for the patients who died prior to decision points 2 and 3

### Decision 1 ###
# No problem here
actual_ttts_val <- list(a1=actual_ttts_val_ori$a1)
actual_ids_val <- list(ids_k1=actual_ids_val_ori$ids_k1)
ite_k1_val <- ite_k1_val_ori
ite_se_k1_val <- ite_se_k1_val_ori

### Decision 2 ###
# Get indices for the patients who died prior to decision 2
phi_2_ids <- val_dat$data$subject_id[val_dat$data$phi_2==1]

# Check if patients used for predictions included any of these dead patients
alive_amg_no_init_k2 <- !(actual_ids_val_ori$ids_k2 %in% phi_2_ids)

# Remove the patients who do not meet this condition
ite_k2_val <- ite_k2_val_ori[alive_amg_no_init_k2]
ite_se_k2_val <- ite_se_k2_val_ori[alive_amg_no_init_k2]

actual_ttts_val$a2 <- actual_ttts_val_ori$a2[alive_amg_no_init_k2]
actual_ids_val$ids_k2 <- actual_ids_val_ori$ids_k2[alive_amg_no_init_k2]

### Decision 3 ###
# Get indices for the patients who died prior to decision 3
phi_3_ids <- val_dat$data$subject_id[val_dat$data$phi_3==1]

# Check if patients used for predictions included any of these dead patients
alive_amg_no_init_k3 <- !(actual_ids_val_ori$ids_k3 %in% phi_3_ids)

# Remove the patients who do not meet this condition
ite_k3_val <- ite_k3_val_ori[alive_amg_no_init_k3]
ite_se_k3_val <- ite_se_k3_val_ori[alive_amg_no_init_k3]
actual_ttts_val$a3 <- actual_ttts_val_ori$a3[alive_amg_no_init_k3]
actual_ids_val$ids_k3 <- actual_ids_val_ori$ids_k3[alive_amg_no_init_k3]

table(ite_k1_val > 0, actual_ttts_val$a1)
table(ite_k2_val > 0, actual_ttts_val$a2)
table(ite_k3_val > 0, actual_ttts_val$a3)

#### Exporting imputed dataframes

dtr_recos <- function(imp_dat){
        mat_k1 <- model.matrix(~1 + admission_age + creat_k1 + pot_k1, data=imp_dat)
        mat_k2 <- model.matrix(~1 + SOFA_24hours + bun_k2 + I(abs(ph_k2-ph_k1)) + I(uo_k2+uo_k1), data=imp_dat)
        mat_k3 <- model.matrix(~1 + uo_k3 + I(bun_k3/bun_k1), data=imp_dat)
        
        ite_k1_val <- mat_k1 %*% est_k1
        ite_k2_val <- mat_k2 %*% est_k2
        ite_k3_val <- mat_k3 %*% est_k3 
        
        ite_se_k1_val <- sqrt(sapply(as.list(as.data.frame(t(mat_k1))), function(x) as.numeric(t(x) %*% vcv_k1 %*% x) ))
        ite_se_k2_val <- sqrt(sapply(as.list(as.data.frame(t(mat_k2))), function(x) as.numeric(t(x) %*% vcv_k2 %*% x) ))
        ite_se_k3_val <- sqrt(sapply(as.list(as.data.frame(t(mat_k3))), function(x) as.numeric(t(x) %*% vcv_k3 %*% x) ))
        
        # Recommendations from the learned policy
        
        r_1 <- as.numeric(ite_k1_val > 0 )
        r_2 <- as.numeric(ite_k2_val > 0 )
        r_3 <- as.numeric(ite_k3_val> 0 )
        
        res_df <- cbind(imp_dat, data.frame(r_1=r_1, r_2=r_2, r_3=r_3))
        
        # Recommends to continue treatment in those who initiated it and sticks with its recommendation if previously recommended initiation
        res_df$r_2[res_df$a1==1 | res_df$r_1==1] <- 1 
        res_df$r_3[res_df$a2==1 | res_df$r_1==1 | res_df$r_2==1] <- 1
        
        res_df$r_2[res_df$phi_2==1] <- NA # No recommendations for the dead
        res_df$r_3[res_df$phi_3==1] <- NA # No recommendations for the dead
        
        # Recommendations from the STRINGENT learned policy i.e., same learned 
        # policy but makes recommendation to initiate RRT only when there is evidence
        # of benefit for individual patients at the 5% alpha threshold
        
        r_s_1 <- as.numeric(ite_k1_val - qnorm(.975) * ite_se_k1_val > 0)
        r_s_2 <- as.numeric(ite_k2_val - qnorm(.975) * ite_se_k2_val > 0)
        r_s_3 <- as.numeric(ite_k3_val - qnorm(.975) * ite_se_k3_val > 0)
        
        res_df <- cbind(res_df, data.frame(r_s_1=r_s_1, r_s_2=r_s_2, r_s_3=r_s_3))
        
        res_df$r_s_2[res_df$a1==1 | res_df$r_s_1==1] <- 1 
        res_df$r_s_3[res_df$a2==1 | res_df$r_s_1==1 | res_df$r_s_2==1] <- 1
        
        res_df$r_s_2[res_df$phi_2==1] <- NA # No recommendations for the dead
        res_df$r_s_3[res_df$phi_3==1] <- NA # No recommendations for the dead
        
return(res_df)
}

fun_reco_in_imp <- function(mice_obj){
list_imp_df <- lapply(1:mice_obj$m, function(x) complete(mice_obj, x) %>% rename(SOFA_24hours=sofa_24hours)  %>% 
                              # recode factor variables into numeric for modeling in python
                              mutate(study=as.numeric(study), gender=as.numeric(startsWith(gender, "M")), immunosuppressant=as.numeric(as.character(immunosuppressant) %in% c("OUI", "1")),
                                     a1=as.numeric(as.character(a1)), a2=as.numeric(as.character(a2)), a3=as.numeric(as.character(a3)), phi_1=as.numeric(as.character(phi_1)), phi_2=as.numeric(as.character(phi_2)), phi_3=as.numeric(as.character(phi_3))) ) 
return(lapply(list_imp_df, dtr_recos))
}

imp_dfs_rec_ls <- fun_reco_in_imp(val_dat)

# Export data to make the multipanel figure in Shiny
save(list=c("actual_ttts",
            "ite_k1", "ite_se_k1",
            "ite_k2", "ite_se_k2",
            "ite_k3", "ite_se_k3",
            "actual_ttts_val",
            "ite_k1_val", "ite_se_k1_val",
            "ite_k2_val", "ite_se_k2_val",
            "ite_k3_val", "ite_se_k3_val"), file="multipanelfig_new.Rdata")

# Export dataframes in .feather for downstream use in python
library(arrow)
setwd("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/feather_files_hmor")
sapply(1:length(imp_dfs_rec_ls), 
       function(i) write_feather(imp_dfs_rec_ls[[i]], paste0("DF",i,".feather")))
setwd("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/")


