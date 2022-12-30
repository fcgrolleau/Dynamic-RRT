from cmath import nan
import pandas as pd
import scipy
import numpy as np
import random
import feather
import os
import matplotlib.pyplot as plt
from tqdm import tqdm
from joblib import Parallel, delayed
import multiprocessing
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
import pickle
from speedboot import speedboot

os.chdir("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/feather_files")
directory = "/Users/francois/Desktop/github repos/MIMIC-DTR/validation/feather_files"

list_of_DFs = []

for filename in os.listdir(directory):
    DF = feather.read_dataframe(filename)
    DF = DF[~DF.orig_id.duplicated()]
    list_of_DFs.append(DF)


def tau_pi_fun(xs):
    i = 1
    for x in xs:
        if x is True:
            break
        else:
            i += 1
    return i


def bool_rec_fun(df):
    """Get policies 'stoping times' tau_\pi """
    bool_rec = df[["r_1", "r_2", "r_3"]] == 1
    df['tau_pi'] = bool_rec.apply(tau_pi_fun, axis=1)
    bool_clin = df[["a1", "a2", "a3"]] == 1
    df['tau_clin'] = bool_clin.apply(tau_pi_fun, axis=1)
    bool_rec_s = df[["r_s_1", "r_s_2", "r_s_3"]] == 1
    df['tau_pi_s'] = bool_rec_s.apply(tau_pi_fun, axis=1)
    return df


def fill_ttt_reco_in_death(df):
    df.loc[df["phi_2"]==1, "a2"] = 0
    df.loc[df["phi_3"]==1, "a3"] = 0

    df.loc[df["phi_2"]==1, "r_2"] = 0
    df.loc[df["phi_3"]==1, "r_3"] = 0
    return df


prep_imp_dfs = [bool_rec_fun(imp_i) for imp_i in list_of_DFs]
prep_imp_dfs = [fill_ttt_reco_in_death(imp_i) for imp_i in prep_imp_dfs]


def fitmodels(imp_1):
    """Fit the 4 x 3 -1 models needed in algorithm 2 ADR estimator with terminal state"""
    ### e_s

    e_1 = LogisticRegression(max_iter=2000, solver='lbfgs', penalty='none') 
    e_2 = LogisticRegression(max_iter=2000, solver='lbfgs', penalty='none') 
    e_3 = LogisticRegression(max_iter=2000, solver='lbfgs', penalty='none')

    cond_e_1 = pd.Series([True] * len(imp_1))
    cond_e_2 = (imp_1["a1"] == 0) & (imp_1["phi_1"] == 0) 
    cond_e_3 = (imp_1["a2"] == 0) & (imp_1["phi_2"] == 0) 
    
    baseline = ["admission_age", "gender", "weight", "immunosuppressant", "SOFA_24hours"]
    
    e_var_1 = ["bun_k1", "uo_k1", "pot_k1", "ph_k1"]
    e_var_2 = ["bun_k2", "uo_k2", "pot_k2", "ph_k2"]  
    e_var_3 = ["bun_k3", "uo_k3", "pot_k3", "ph_k3"]

    X_e_1 = imp_1[e_var_1]
    X_e_2 = imp_1[cond_e_2][e_var_2]
    X_e_3 = imp_1[cond_e_3][e_var_3]

    Y_e_1 = imp_1["a1"]
    Y_e_2 = imp_1[cond_e_2]["a2"]
    Y_e_3 = imp_1[cond_e_3]["a3"]

    e_1.fit(X_e_1, Y_e_1)
    e_2.fit(X_e_2, Y_e_2)
    e_3.fit(X_e_3, Y_e_3)

    preds_e_2 = e_2.predict_proba(imp_1[e_var_2])[:,1]
    preds_e_3 = e_3.predict_proba(imp_1[e_var_3])[:,1]

    ### mu_nows

    mu_now_1 = RandomForestRegressor(random_state=0)
    mu_now_2 = RandomForestRegressor(random_state=0)
    mu_now_3 = RandomForestRegressor(random_state=0)

    cond_mu_now_1 = imp_1["a1"] == 1
    cond_mu_now_2 = (imp_1["a1"] == 0) & (imp_1["a2"] == 1) & (imp_1["phi_2"] == 0) 
    cond_mu_now_3 = (imp_1["a2"] == 0) & (imp_1["a3"] == 1) & (imp_1["phi_3"] == 0) 

    mu_var_1 = ["admission_age", "gender", "weight", "immunosuppressant", "SOFA_24hours", "creat_k1", "bun_k1", "pot_k1", "ph_k1", "uo_k1"]
    mu_var_2 = mu_var_1 + ["creat_k2", "bun_k2", "pot_k2", "ph_k2", "uo_k2"]
    mu_var_3 = mu_var_2 + ["creat_k3", "bun_k3", "pot_k3", "ph_k3", "uo_k3"]

    X_mu_now_1 = imp_1[cond_mu_now_1][mu_var_1]
    X_mu_now_2 = imp_1[cond_mu_now_2][mu_var_2]
    X_mu_now_3 = imp_1[cond_mu_now_3][mu_var_3]

    Y_mu_now_1 = imp_1[cond_mu_now_1]["hfd"]
    Y_mu_now_2 = imp_1[cond_mu_now_2]["hfd"]
    Y_mu_now_3 = imp_1[cond_mu_now_3]["hfd"]

    mu_now_1.fit(X_mu_now_1, Y_mu_now_1)
    mu_now_2.fit(X_mu_now_2, Y_mu_now_2)
    mu_now_3.fit(X_mu_now_3, Y_mu_now_3)

    ### U_nexts

    U_next_1 = RandomForestRegressor(random_state=0)
    U_next_2 = RandomForestRegressor(random_state=0)
    U_next_3 = RandomForestRegressor(random_state=0)

    cond_U_next_1 = (imp_1["a1"] == 0) & (imp_1["a2"] == 1) & (imp_1["phi_2"] == 0)
    cond_U_next_2 = (imp_1["a2"] == 0) & (imp_1["a3"] == 1) & (imp_1["phi_2"] == 0) & (imp_1["phi_3"] == 0)
    cond_U_next_3 = (imp_1["a3"] == 0) & (imp_1["phi_3"] == 0)

    X_U_next_1 = imp_1[cond_U_next_1][mu_var_1]
    X_U_next_2 = imp_1[cond_U_next_2][mu_var_2]
    X_U_next_3 = imp_1[cond_U_next_3][mu_var_3]

    Y_U_next_1 = imp_1[cond_U_next_1]["hfd"] 
    Y_U_next_2 = imp_1[cond_U_next_2]["hfd"] 
    Y_U_next_3 = imp_1[cond_U_next_3]["hfd"]

    U_next_1.fit(X_U_next_1, Y_U_next_1, sample_weight=(1/preds_e_2)[cond_U_next_1])
    U_next_2.fit(X_U_next_2, Y_U_next_2, sample_weight=(1/preds_e_3)[cond_U_next_2])
    U_next_3.fit(X_U_next_3, Y_U_next_3)

    ### rhos

    rho_1 = RandomForestClassifier(random_state=0)
    rho_2 = RandomForestClassifier(random_state=0)
    # rho_3 is not needed

    cond_rho_1 = (imp_1["a1"] == 0)
    cond_rho_2 = (imp_1["a2"] == 0) & (imp_1["phi_2"] == 0)
    # rho_3 is not needed

    X_rho_1 = imp_1[cond_rho_1][mu_var_1]
    X_rho_2 = imp_1[cond_rho_2][mu_var_2]

    Y_rho_1 = imp_1[cond_rho_1]["phi_2"]
    Y_rho_2 = imp_1[cond_rho_2]["phi_3"]

    rho_1.fit(X_rho_1, Y_rho_1)
    rho_2.fit(X_rho_2, Y_rho_2)

    ## return models
    return e_1, e_2, e_3, mu_now_1, mu_now_2, mu_now_3, U_next_1, U_next_2, U_next_3, rho_1, rho_2


def crossfitted_preds(fold, e_1, e_2, e_3, mu_now_1, mu_now_2, mu_now_3, U_next_1, U_next_2, U_next_3, rho_1, rho_2):
    """returns crossfitted predictions from previously fitted models"""
    
    baseline = ["admission_age", "gender", "weight", "immunosuppressant", "SOFA_24hours"]
    
    e_var_1 = ["bun_k1", "uo_k1", "pot_k1", "ph_k1"]
    e_var_2 = ["bun_k2", "uo_k2", "pot_k2", "ph_k2"]  
    e_var_3 = ["bun_k3", "uo_k3", "pot_k3", "ph_k3"]
    
    mu_var_1 = ["admission_age", "gender", "weight", "immunosuppressant", "SOFA_24hours", "creat_k1", "bun_k1", "pot_k1", "ph_k1", "uo_k1"]
    mu_var_2 = mu_var_1 + ["creat_k2", "bun_k2", "pot_k2", "ph_k2", "uo_k2"]
    mu_var_3 = mu_var_2 + ["creat_k3", "bun_k3", "pot_k3", "ph_k3", "uo_k3"]
    
    ### e_s

    preds_e_1 = e_1.predict_proba(fold[e_var_1])[:,1]
    preds_e_2 = e_2.predict_proba(fold[e_var_2])[:,1]
    preds_e_3 = e_3.predict_proba(fold[e_var_3])[:,1]

    ### mu_nows

    preds_mu_now_1 = mu_now_1.predict(fold[mu_var_1]) 
    preds_mu_now_2 = mu_now_2.predict(fold[mu_var_2]) 
    preds_mu_now_3 = mu_now_3.predict(fold[mu_var_3]) 

    ### U_nexts

    preds_U_next_1 = U_next_1.predict(fold[mu_var_1])
    preds_U_next_2 = U_next_2.predict(fold[mu_var_2])
    preds_U_next_3 = U_next_3.predict(fold[mu_var_3])

    ### rhos

    preds_rho_1 = rho_1.predict_proba(fold[mu_var_1])[:,1]
    preds_rho_2 = rho_2.predict_proba(fold[mu_var_2])[:,1]

    ## return models
    return preds_e_1, preds_e_2, preds_e_3, preds_mu_now_1, preds_mu_now_2, preds_mu_now_3, preds_U_next_1, preds_U_next_2, preds_U_next_3, preds_rho_1, preds_rho_2


# In[8]:


def conc_cf_preds(fold_1, fold_2):
    """concatenates crossfitted predictions given two folds"""
    
    (e_1, e_2, e_3, mu_now_1, mu_now_2, mu_now_3, U_next_1, U_next_2, U_next_3, rho_1, rho_2) = fitmodels(fold_2)
    f_1_preds_e_1, f_1_preds_e_2, f_1_preds_e_3, f_1_preds_mu_now_1, f_1_preds_mu_now_2, f_1_preds_mu_now_3, f_1_preds_U_next_1, f_1_preds_U_next_2, f_1_preds_U_next_3, f_1_preds_rho_1, f_1_preds_rho_2 = crossfitted_preds(fold_1, e_1, e_2, e_3, mu_now_1, mu_now_2, mu_now_3, U_next_1, U_next_2, U_next_3, rho_1, rho_2)

    (e_1, e_2, e_3, mu_now_1, mu_now_2, mu_now_3, U_next_1, U_next_2, U_next_3, rho_1, rho_2) = fitmodels(fold_1)
    f_2_preds_e_1, f_2_preds_e_2, f_2_preds_e_3, f_2_preds_mu_now_1, f_2_preds_mu_now_2, f_2_preds_mu_now_3, f_2_preds_U_next_1, f_2_preds_U_next_2, f_2_preds_U_next_3, f_2_preds_rho_1, f_2_preds_rho_2 = crossfitted_preds(fold_2, e_1, e_2, e_3, mu_now_1, mu_now_2, mu_now_3, U_next_1, U_next_2, U_next_3, rho_1, rho_2)
    
    preds_e_1 = np.concatenate((f_1_preds_e_1, f_2_preds_e_1))
    preds_e_2 = np.concatenate((f_1_preds_e_2, f_2_preds_e_2))
    preds_e_3 = np.concatenate((f_1_preds_e_3, f_2_preds_e_3))
    
    preds_mu_now_1 = np.concatenate((f_1_preds_mu_now_1, f_2_preds_mu_now_1))
    preds_mu_now_2 = np.concatenate((f_1_preds_mu_now_2, f_2_preds_mu_now_2))
    preds_mu_now_3 = np.concatenate((f_1_preds_mu_now_3, f_2_preds_mu_now_3))
    
    preds_U_next_1 = np.concatenate((f_1_preds_U_next_1, f_2_preds_U_next_1))
    preds_U_next_2 = np.concatenate((f_1_preds_U_next_2, f_2_preds_U_next_2))
    preds_U_next_3 = np.concatenate((f_1_preds_U_next_3, f_2_preds_U_next_3))
    
    preds_rho_1 = np.concatenate((f_1_preds_rho_1, f_2_preds_rho_1))
    preds_rho_2 = np.concatenate((f_1_preds_rho_2, f_2_preds_rho_2))
    
    return preds_e_1, preds_e_2, preds_e_3, preds_mu_now_1, preds_mu_now_2, preds_mu_now_3, preds_U_next_1, preds_U_next_2, preds_U_next_3, preds_rho_1, preds_rho_2


# In[9]:


def cf_ADR(data, verbose=False):
    """computes two folds crossfitted ADR estimator given data"""
    
    # shuffle the data in-place
    data = data.sample(frac=1, random_state=0).reset_index(drop=True)

    # get index for a two fold cut
    cut_i = int(len(data)/2)

    # cut data in two folds
    fold_1 = data.iloc[0:cut_i]
    fold_2 = data.iloc[cut_i:]
    
    preds_e_1, preds_e_2, preds_e_3, preds_mu_now_1, preds_mu_now_2, preds_mu_now_3, preds_U_next_1, preds_U_next_2, preds_U_next_3, preds_rho_1, preds_rho_2 = conc_cf_preds(fold_1, fold_2)

    ### mu_nexts

    preds_mu_next_1 = (1-preds_rho_1) * preds_U_next_1
    preds_mu_next_2 = (1-preds_rho_2) * preds_U_next_2
    preds_mu_next_3 = preds_U_next_3

    ### DR scores

    dr_1 = (preds_mu_now_1 - preds_mu_next_1) + (data["a1"]==1).astype(int) * (data["hfd"] - preds_mu_now_1)/preds_e_1 - (data["a1"]==0).astype(int) * (data["a2"]==1).astype(int) * (data["hfd"] - preds_U_next_1) / ( (1-preds_e_1) * preds_e_2)
    dr_2 = (preds_mu_now_2 - preds_mu_next_2) + (data["a2"]==1).astype(int) * (data["hfd"] - preds_mu_now_2)/preds_e_2 - (data["a2"]==0).astype(int) * (data["a3"]==1).astype(int) * (data["hfd"] - preds_U_next_2) / ( (1-preds_e_2) * preds_e_3)
    dr_3 = (preds_mu_now_3 - preds_mu_next_3) + (data["a3"]==1).astype(int) * (data["hfd"] - preds_mu_now_3)/preds_e_3 - (data["a3"]==0).astype(int) * (data["hfd"] - preds_U_next_3) / (1-preds_e_3)

    ### ADR

    # learned policy
    adr_t1 = (data["phi_1"]==0).astype(int) * (1 >= data["tau_pi"]).astype(int) * 1 * dr_1 / (1)
    adr_t2 = (data["phi_2"]==0).astype(int) * (2 >= data["tau_pi"]).astype(int) * (data["a1"] == 0).astype(int) * dr_2 / (1-preds_e_1)
    adr_t3 = (data["phi_3"]==0).astype(int) * (3 >= data["tau_pi"]).astype(int) * (data["a2"] == 0).astype(int) * dr_3 / ((1-preds_e_1) * (1-preds_e_2))

    test = (adr_t1 + adr_t2 + adr_t3)
    val_learned = np.mean(test)

    # learned stringent policy
    adr_t1 = (data["phi_1"]==0).astype(int) * (1 >= data["tau_pi_s"]).astype(int) * 1 * dr_1 / (1)
    adr_t2 = (data["phi_2"]==0).astype(int) * (2 >= data["tau_pi_s"]).astype(int) * (data["a1"] == 0).astype(int) * dr_2 / (1-preds_e_1)
    adr_t3 = (data["phi_3"]==0).astype(int) * (3 >= data["tau_pi_s"]).astype(int) * (data["a2"] == 0).astype(int) * dr_3 / ((1-preds_e_1) * (1-preds_e_2))

    test = (adr_t1 + adr_t2 + adr_t3)
    val_learned_s = np.mean(test)

    # clinicians policy
    adr_t1 = (data["phi_1"]==0).astype(int) * (1 >= data["tau_clin"]).astype(int) * 1 * dr_1 / (1)
    adr_t2 = (data["phi_2"]==0).astype(int) * (2 >= data["tau_clin"]).astype(int) * (data["a1"] == 0).astype(int) * dr_2 / (1-preds_e_1)
    adr_t3 = (data["phi_3"]==0).astype(int) * (3 >= data["tau_clin"]).astype(int) * (data["a2"] == 0).astype(int) * dr_3 / ((1-preds_e_1) * (1-preds_e_2))

    test = (adr_t1 + adr_t2 + adr_t3)
    val_clin = np.mean(test)

    # treat at a1 policy
    adr_t1 = (data["phi_1"]==0).astype(int) * 1 * 1 * dr_1 / (1)
    adr_t2 = (data["phi_2"]==0).astype(int) * 1 * (data["a1"] == 0).astype(int) * dr_2 / (1-preds_e_1)
    adr_t3 = (data["phi_3"]==0).astype(int) * 1 * (data["a2"] == 0).astype(int) * dr_3 / ((1-preds_e_1) * (1-preds_e_2))

    test = (adr_t1 + adr_t2 + adr_t3)
    val_treat_all = np.mean(test)
    
    if verbose:
        print(f'val_learned = {val_learned:.3f}')
        print(f'val_learned stringent = {val_learned_s:.3f}')
        print(f'val_clin = {val_clin:.3f}')
        print(f'val_treat_all = {val_treat_all:.3f}\n')

    return (val_learned, val_learned_s, val_clin, val_treat_all,  # ref treat none policy 0:4
            val_learned-val_clin, val_learned_s-val_clin, val_treat_all-val_clin, # ref clinician policy 4:7
            val_learned-val_treat_all, val_learned_s-val_treat_all, # ref treat all policy 7:9
            val_learned_s-val_learned # ref learned policy 9
           )


# In[10]:


pol_comp = ["learned_v_none", "learned_stringent_v_none", "clin_v_none", "treat_all_v_none", 
"learned_v_clin", "learned_stringent_v_clin", "treat_all_v_clin",
"learned_v_treat_all", "learned_stringent_v_treat_all",
"learned_stringent_v_learned"]


# Bootrap the whole procedure as presented in the flowchart
def procedure_cf(dat):
    df = dat.copy()
    df = df.append(df[df["brasrando"]=='standard'])
    return np.array(cf_ADR(df))


#Bootstrap all datasets
def runboot(df):
    sb_obj = speedboot(data=df, stats_fun=procedure_cf)
    sb_obj.fit(R=999, bar=True, par=True, seed=123)
    sb_obj.per = sb_obj.per_ci()
    sb_obj.emp = sb_obj.emp_ci()
    
    #sb_obj.jackknife(bar=True, par=True)
    #sb_obj.bca = sb_obj.bca_ci()
    return sb_obj

sb_cf_alldf = [runboot(prep_imp_dfs[i]) for i in range(len(prep_imp_dfs))]


os.chdir("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/python/Terminal state fix")
with open('sb_cf_alldf.pickle', 'wb') as handle:
    pickle.dump(sb_cf_alldf, handle, protocol=pickle.HIGHEST_PROTOCOL)


final_strat = np.array(['Learned strategy', 'Never treat within 72 hours', 'Treat all within 24 hours'])

est_alldf = np.mean(np.array([sb_cf_alldf[i].ests for i in range(len(sb_cf_alldf))]), axis=0)
final_pe = np.array([est_alldf[4], -est_alldf[2], est_alldf[6]])
final_pe


emp_alldf = np.mean(np.array([sb_cf_alldf[i].emp for i in range(len(sb_cf_alldf))]), axis=0)
final_cis = np.vstack([emp_alldf[4], emp_alldf[2]-2*est_alldf[2], emp_alldf[6]])
final_cis


np.save('est_alldf.npy', est_alldf)
np.save('per_alldf.npy', per_alldf)
np.save('emp_alldf.npy', emp_alldf)