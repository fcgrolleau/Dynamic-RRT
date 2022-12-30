library(mice)
library(dplyr)
load("imputed_mimic_dtr.RData")
load("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/ite_preds_coef.RData")
imp_1 <- complete(exp_dat, 1)

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
  
  # Recommends to continue treatment in those who initiated it and sticks which its recommendation if previously recommended initiation
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
  list_imp_df <- lapply(1:mice_obj$m, function(x) complete(mice_obj, x) %>%
                          # recode factor variables into numeric for modeling in python
                          mutate(immunosuppressant=as.numeric(as.character(immunosuppressant) %in% c("OUI", "1")),
                                 a1=as.numeric(as.character(a1)), a2=as.numeric(as.character(a2)), a3=as.numeric(as.character(a3)) ) ) 
  return(lapply(list_imp_df, dtr_recos))
}

imp_dfs_rec_ls <- fun_reco_in_imp(exp_dat)

# Export dataframes in .feather for downstream use in python
library(arrow)
setwd("/Users/francois/Desktop/github repos/MIMIC-DTR/feather_files_dev")
sapply(1:length(imp_dfs_rec_ls), 
       function(i) write_feather(imp_dfs_rec_ls[[i]], paste0("DF",i,".feather")))
setwd("/Users/francois/Desktop/github repos/MIMIC-DTR/")
     