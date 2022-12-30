#Reminder:"Premature optimization is the root of all evil."
source("functions.R")
library(mice)
library(boot)

load("imputed_mimic_dtr.RData")

boot_dtr <- function(d, i=1:nrow(d), output="boo") {
        dat<-d[i,]
        dat <- as.data.frame(lapply(dat, defactorize))
        
################################################
#### Backward induction procedure stage k=3 #### 
################################################

# exclude the patients who initiated RRT before stage k=3 
# ( under regular policies, there is no treatment decision
# to make for these patients at stage k=3 )

excluded_k3 <- dat$a2 == 1
included_k3 <- !excluded_k3 # for clarity later
dat_k3 <- dat[included_k3,]

# Fit propensity score for stage k=3

ps_mod_k3 <- glm(a3 ~ bun_k3 + ph_k3 + pot_k3 + uo_k3, family = "binomial", data= dat_k3)

# Create weights as described by Wallace and Moodie, Biometrics 2015
# to achieve double robustness

w_k3 <- with(dat_k3, abs(a3 - predict(ps_mod_k3, type = "response")) )

# Weighted Least Square regression step
# Specificity model with main prognosis variables and interactions between treatment
# at stage k=3 and the tailoring variables 

#pr_mod_k3 <- lm(hfd ~ admission_age + weight + gender + SOFA_24hours + bun_k1 + bun_k3 +
#                       a3*uo_k3,
#                        weights = w_k3, data = dat_k3)

pr_mod_k3 <- lm(hfd ~ admission_age + weight + gender + SOFA_24hours + bun_k3 +
                        a3 * uo_k3 + a3 * I(bun_k3/bun_k1),
                weights = w_k3, data = dat_k3) ## keep this model

# Model fit and diagnostics are accessible via
# summary(pr_mod_k3) and plot(pr_mod_k3) respectively

# Store all coefficients for bootstrapping
all_coef_k3 <- coef(pr_mod_k3)
names(all_coef_k3) <- paste0("mod_k3_", names(all_coef_k3))

# Store the effect of tailoring variables 
psi_k3 <- coef(pr_mod_k3)[grepl("a3", names(coef(pr_mod_k3)) )]

### Create pseudo outcomes ###

# Contrast for a=1 at stage k=3
mat_k3 <- model.matrix(~1 + uo_k3 + I(bun_k3/bun_k1), data=dat_k3)
contrast_a1_k3 <- mat_k3 %*% psi_k3

# Contrast for a=0 (reference) at stage k=3
# by definition contrast for reference = 0 at all stages
contrast_a0_k3 <- 0 

# Optimal contrast at stage k=3
optimal_contrast_k3 <- pmax(contrast_a1_k3, contrast_a0_k3)

# Actual contrast at stage k=3
actual_contrast_k3 <- model.matrix(~-1 + a3 + a3:uo_k3 + a3:I(bun_k3/bun_k1), data=dat_k3) %*% psi_k3

# Regret at stage k=3
# By definition, regret = optimal contrast - actual contrast
regret_k3 <- optimal_contrast_k3 - actual_contrast_k3

# Create a copy of the observed outcome
dat$hfd_tilde_2 <- dat$hfd

# For the patients who would have benefited from another treatment at stage k=3
# add this missed benefit (aka regret) to create PSEUDOS-outcomes under optimal treatment at k=3
dat$hfd_tilde_2[included_k3] <-  dat$hfd_tilde_2[included_k3] + regret_k3

################################################
#### Backward induction procedure stage k=2 #### 
################################################

# exclude the patients who initiated RRT before stage k=2 
# ( under regular policies, there is no treatment decision
# to make for these patients at stage k=2 )

excluded_k2 <- dat$a1 == 1
included_k2 <- !excluded_k2 # for clarity later
dat_k2 <- dat[included_k2,]

# Fit propensity score for stage k=2

ps_mod_k2 <- glm(a2 ~ bun_k2 + ph_k2 + pot_k2 + uo_k2, family = "binomial", data = dat_k2)

# Create weights as described by Wallace and Moodie, Biometrics 2015
# to achieve double robustness

w_k2 <- with(dat_k2, abs(a2 - predict(ps_mod_k2, type = "response")) )

# Weighted Least Square regression step
# Specificity model with main prognosis variables and interactions between treatment
# at stage k=2 and the tailoring variables 

pr_mod_k2 <- lm(hfd_tilde_2 ~ admission_age + SOFA_24hours + weight + gender +
                        #bun_k1 +
                        #ph_k2 +
                        #uo_k2 + 
                         #pot_k2 + 
                         a2*SOFA_24hours + a2*bun_k2 + a2:I(abs(ph_k2-ph_k1)) + a2*I(uo_k2+uo_k1),
                weights = w_k2, data = dat_k2) ### A revoir +++

summary(pr_mod_k2)

# plot(gam(hfd_tilde_2 ~ s(admission_age), data = dat_k2), se=TRUE)
# plot(gam(hfd_tilde_2 ~ s(SOFA_24hours), data = dat_k2), se=TRUE)
# plot(gam(hfd_tilde_2 ~ s(pot_k2), data = dat_k2), se=TRUE)

# Model fit and diagnostics are accessible via
# summary(pr_mod_k2) and plot(pr_mod_k2) respectively

# Store all coefficients for bootstrapping
all_coef_k2 <- coef(pr_mod_k2)
names(all_coef_k2) <- paste0("mod_k2_", names(all_coef_k2))

# Store the effect of tailoring variables 
psi_k2 <- coef(pr_mod_k2)[grepl("a2", names(coef(pr_mod_k2)) )]

### Create pseudo outcomes ###

# Contrast for a=1 at stage k=2
mat_k2 <- model.matrix(~1 + SOFA_24hours + bun_k2 + I(abs(ph_k2-ph_k1)) + I(uo_k2+uo_k1), data=dat_k2)
contrast_a1_k2 <- mat_k2 %*% psi_k2

# Contrast for a=0 (reference) at stage k=2
# by definition contrast for reference = 0 at all stages
contrast_a0_k2 <- 0 

# Optimal contrast at stage k=2
optimal_contrast_k2 <- pmax(contrast_a1_k2, contrast_a0_k2)

# Actual contrast at stage k=2
actual_contrast_k2 <- model.matrix(~-1 + a2 + a2:SOFA_24hours + a2:bun_k2 + a2:I(abs(ph_k2 - ph_k1)) + a2:I(abs(uo_k2 + uo_k1)), data=dat_k2) %*% psi_k2

# Regret at stage k=2
# By definition, regret = optimal contrast - actual contrast
regret_k2 <- optimal_contrast_k2 - actual_contrast_k2

# Create a copy of the PSEUDOS-outcomes under optimal treatment at k=3
dat$hfd_tilde_1 <- dat$hfd
dat$hfd_tilde_1[included_k2] <- dat_k2$hfd_tilde_2

# For the patients who would have benefited from another treatment at stage k=2
# add this missed benefit (aka regret)
dat$hfd_tilde_1[included_k2] <-  dat$hfd_tilde_1[included_k2] + regret_k2

################################################
#### Backward induction procedure stage k=1 #### 
################################################

# no patients are exluded as non patient initiated RRT before stage k=1 
# ( all patients have a decision to make at stage k=1 )

dat_k1 <- dat

# Fit propensity score for stage k=2

ps_mod_k1 <- glm(a1 ~ bun_k1 + ph_k1 + pot_k1, family = "binomial", data = dat_k1)

# Create weights as described by Wallace and Moodie, Biometrics 2015
# to achieve double robustness

w_k1 <- with(dat_k1, abs(a1 - predict(ps_mod_k1, type = "response")) )

# Weighted Least Square regression step
# Specificity model with main prognosis variables and interactions between treatment
# at stage k=2 and the tailoring variables 

pr_mod_k1 <- lm(hfd_tilde_1 ~ admission_age + SOFA_24hours + weight + gender +
#                        bun_k1 +
#                       ph_k1 +
                        uo_k1 + 
#                       pot_k1 +
                        a1*admission_age + a1*creat_k1 + a1*pot_k1,
                weights = w_k1, data = dat_k1) ### A revoir +++

######
# Model fit and diagnostics are accessible via
# summary(pr_mod_k1) and plot(pr_mod_k1) respectively

# Store all coefficients for bootstrapping
all_coef_k1 <- coef(pr_mod_k1)
names(all_coef_k1) <- paste0("mod_k1_", names(all_coef_k1))

# Store the effect of tailoring variables 
psi_k1 <- coef(pr_mod_k1)[grepl("a1", names(coef(pr_mod_k1)) )]

# Store the matrix of tailoring variables 
mat_k1 <- model.matrix(~1 + admission_age + creat_k1 + pot_k1, data=dat_k1)


##### FINAL RETURN FROM THE FUNCTION #####
##### returns concatenated coefficients from stage 1,2,3 models #####
if(output=="boo"){
        return(c(all_coef_k1, all_coef_k2, all_coef_k3))
} else if(output=="mats"){
        return(list(mat_k1, mat_k2, mat_k3))        
} else if(output=="ttt"){
        return(list(a1=dat_k1$a1, a2=dat_k2$a2, a3=dat_k3$a3))        
} else if(output=="ids"){
        return(list(ids_k1=dat_k1$subject_id, ids_k2=dat_k2$subject_id, ids_k3=dat_k3$subject_id))
}
}

###
n_boot <- 999
res <- list()
all_coefs <- list()
all_coefs_vars <- list()

for(i in 1:exp_dat$m)
{        
imp_dat <- complete(exp_dat, i)
boot_res <- boot(imp_dat, boot_dtr, R=n_boot)
res[[i]] <- boot_res
all_coefs[[i]] <- res[[i]]$t0
all_coefs_vars[[i]] <- apply(res[[i]]$t, 2, var)
print(paste0(i, "/", exp_dat$m, " datasets completed"))
}

rubin_est <- c() ; rubin_ests <- c()
rubin_var <- c() ; rubin_vars <- c()

for(i in 1:length(all_coefs[[1]]))
{
rubin_est <- mean(sapply(all_coefs, function(x) x[[i]]))
rubin_ests <- c(rubin_ests, rubin_est)

rubin_var <- rubinr(sapply(all_coefs, function(x) x[[i]]), sapply(all_coefs_vars, function(x) x[[i]]) )
rubin_vars <- c(rubin_vars, rubin_var)
}

names(rubin_ests) <- names(res[[1]]$t0)
names(rubin_vars) <- names(res[[1]]$t0)

est_k1 <- rubin_ests[grepl("a1", names(rubin_ests))]
est_k2 <- rubin_ests[grepl("a2", names(rubin_ests))]
est_k3 <- rubin_ests[grepl("a3", names(rubin_ests))]

rubin_vars[grepl("a1", names(rubin_vars))]
rubin_vars[grepl("a2", names(rubin_vars))]
rubin_vars[grepl("a3", names(rubin_vars))]

cov_Mxs_k1 <- array(NA, dim = c(length(est_k1), length(est_k1), exp_dat$m))
cov_Mxs_k2 <- array(NA, dim = c(length(est_k2), length(est_k2), exp_dat$m))
cov_Mxs_k3 <- array(NA, dim = c(length(est_k3), length(est_k3), exp_dat$m))

for(i in 1:exp_dat$m) 
{        
cov_Mxs_k1[, , i] <- cov(res[[i]]$t[,grepl("a1", names(rubin_ests))] )
cov_Mxs_k2[, , i] <- cov(res[[i]]$t[,grepl("a2", names(rubin_ests))] )
cov_Mxs_k3[, , i] <- cov(res[[i]]$t[,grepl("a3", names(rubin_ests))] )
}

W_k1 <- apply(cov_Mxs_k1, c(1,2), mean)
W_k2 <- apply(cov_Mxs_k2, c(1,2), mean)
W_k3 <- apply(cov_Mxs_k3, c(1,2), mean)

temp_f <- function(x, a="a1", est=est_k1)
{
        tet_vec <- x[grepl(a, names(rubin_vars))] - est 
        tet_vec %*% t(tet_vec)    
}

B_k1 <- 1/(exp_dat$m - 1) * Reduce('+', lapply(all_coefs, function(x) temp_f(x, a="a1", est=est_k1) ) )
B_k2 <- 1/(exp_dat$m - 1) * Reduce('+', lapply(all_coefs, function(x) temp_f(x, a="a2", est=est_k2) ) )
B_k3 <- 1/(exp_dat$m - 1) * Reduce('+', lapply(all_coefs, function(x) temp_f(x, a="a3", est=est_k3) ) )


vcv_k1 <- W_k1 + (1 + 1/exp_dat$m) * B_k1
vcv_k2 <- W_k2 + (1 + 1/exp_dat$m) * B_k2
vcv_k3 <- W_k3 + (1 + 1/exp_dat$m) * B_k3

###### Get ITE predictions and their standard errors ###### 

mats_k123 <- list()
for(i in 1:exp_dat$m)
{
mats_k123[[i]] <- boot_dtr(d = complete(exp_dat, i), output = "mats")
print(paste0(i, "/", exp_dat$m, " datasets completed"))
}

mean_list_of_matrices <- function(x)
{
Reduce("+", x)/ length(x)
}

mean_mat_k1 <- mean_list_of_matrices(lapply(mats_k123, function(x) x[[1]]))
mean_mat_k2 <- mean_list_of_matrices(lapply(mats_k123, function(x) x[[2]]))
mean_mat_k3 <- mean_list_of_matrices(lapply(mats_k123, function(x) x[[3]]))


ite_k1 <- mean_mat_k1 %*% est_k1
ite_k2 <- mean_mat_k2 %*% est_k2
ite_k3 <- mean_mat_k3 %*% est_k3

ite_se_k1 <- sqrt(sapply(as.list(as.data.frame(t(mean_mat_k1))), function(x) as.numeric(t(x) %*% vcv_k1 %*% x) ))
ite_se_k2 <- sqrt(sapply(as.list(as.data.frame(t(mean_mat_k2))), function(x) as.numeric(t(x) %*% vcv_k2 %*% x) ))
ite_se_k3 <- sqrt(sapply(as.list(as.data.frame(t(mean_mat_k3))), function(x) as.numeric(t(x) %*% vcv_k3 %*% x) ))

actual_ttts <- boot_dtr(d = complete(exp_dat, 1), output = "ttt")
actual_ids <- boot_dtr(d = complete(exp_dat, 1), output = "ids")

table(ite_k1 > 0, actual_ttts$a1)
table(ite_k2 > 0, actual_ttts$a2)
table(ite_k3 > 0, actual_ttts$a3)

save(est_k1, est_k2, est_k3, 
       vcv_k1, vcv_k2, vcv_k3,
       ite_k1, ite_k2, ite_k3,
       ite_se_k1, ite_se_k2, ite_se_k3, 
       actual_ttts,
       file="ite_preds_coef.RData")

