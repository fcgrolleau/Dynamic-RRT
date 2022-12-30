#!/usr/bin/env python
# coding: utf-8

# In[18]:


## Importing .feather files into a list of DFs in Python
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

os.chdir("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/feather_files_hmor")
directory = "/Users/francois/Desktop/github repos/MIMIC-DTR/validation/feather_files_hmor"

list_of_DFs = []

for filename in os.listdir(directory):
    DF = feather.read_dataframe(filename)
    DF = DF[~DF.orig_id.duplicated()]
    list_of_DFs.append(DF)


# In[19]:


def tau_pi_fun(xs):
    i = 1
    for x in xs:
        if x is True:
            break
        else:
            i += 1
    return i


# In[20]:


def bool_rec_fun(df):
    """Get policies 'stoping times' tau_\pi """
    bool_rec = df[["r_1", "r_2", "r_3"]] == 1
    df['tau_pi'] = bool_rec.apply(tau_pi_fun, axis=1)
    bool_clin = df[["a1", "a2", "a3"]] == 1
    df['tau_clin'] = bool_clin.apply(tau_pi_fun, axis=1)
    bool_rec_s = df[["r_s_1", "r_s_2", "r_s_3"]] == 1
    df['tau_pi_s'] = bool_rec_s.apply(tau_pi_fun, axis=1)
    return df


# In[21]:


def add_treatall(df):
    new_df = df.copy()
    new_df['all_1'] = 1
    new_df['all_2'] = 1
    new_df['all_3'] = 1
    return new_df


# In[22]:


prep_imp_dfs = [bool_rec_fun(imp_i) for imp_i in list_of_DFs]
prep_imp_dfs = [add_treatall(imp_i) for imp_i in prep_imp_dfs]
imp_1 = prep_imp_dfs[0]


# In[23]:


def n_rrt_fun(imp_1, r_vec):
    imp_1.loc[imp_1["phi_2"]==1, "a2"] = 0
    imp_1.loc[imp_1["phi_3"]==1, "a3"] = 0

    imp_1.loc[imp_1["phi_2"]==1, "r_2"] = 0
    imp_1.loc[imp_1["phi_3"]==1, "r_3"] = 0
    
    # Create reward for a trajectory
    imp_1['rwd'] = imp_1['hmor']#imp_1[r_vec].apply(lambda x: any(x==1), axis=1)
    imp_1['rwd_obs'] = imp_1[["a1", "a2", "a3"]].apply(lambda x: any(x==1), axis=1)

    e_1 = LogisticRegression(max_iter=2000, solver='lbfgs', penalty='none')#C=.1)
    e_2 = LogisticRegression(max_iter=2000, solver='lbfgs', penalty='none')#C=.1)
    e_3 = LogisticRegression(max_iter=2000, solver='lbfgs', penalty='none')#C=10)

    cond_e_1 = pd.Series([True] * len(imp_1))
    cond_e_2 = (imp_1["a1"] == 0) & (imp_1["phi_1"] == 0) 
    cond_e_3 = (imp_1["a2"] == 0) & (imp_1["phi_3"] == 0) 

    baseline = ["admission_age", "gender", "weight", "immunosuppressant", "SOFA_24hours"]

    e_var_1 = ["bun_k1", "uo_k1", "pot_k1", "ph_k1"]
    e_var_2 = ["bun_k2", "uo_k2", "pot_k2", "ph_k2"]  
    e_var_3 = ["bun_k3", "uo_k3", "pot_k3", "ph_k3"]

    #e_var_1 = ["bun_k1", "pot_k1", "ph_k1"]#["bun_k1", "uo_k1", "pot_k1", "ph_k1"]
    #e_var_2 = ["bun_k2", "uo_k2", "pot_k2", "ph_k2"]  
    #e_var_3 = ["bun_k3", "uo_k3", "pot_k3", "ph_k3"]

    e_var_1 = baseline + ["bun_k1", "uo_k1", "pot_k1", "ph_k1"]
    e_var_2 = baseline + ["bun_k2", "uo_k2", "pot_k2", "ph_k2"]  
    e_var_3 = baseline + ["bun_k3", "uo_k3", "pot_k3", "ph_k3"]

    X_e_1 = imp_1[e_var_1]
    X_e_2 = imp_1[cond_e_2][e_var_2]
    X_e_3 = imp_1[cond_e_3][e_var_3]

    Y_e_1 = imp_1["a1"]
    Y_e_2 = imp_1[cond_e_2]["a2"]
    Y_e_3 = imp_1[cond_e_3]["a3"]

    e_1.fit(X_e_1, Y_e_1)
    e_2.fit(X_e_2, Y_e_2)
    e_3.fit(X_e_3, Y_e_3)

    preds_e_1 = e_1.predict_proba(imp_1[e_var_1])[:,1]
    preds_e_2 = e_2.predict_proba(imp_1[e_var_2])[:,1]
    preds_e_3 = e_3.predict_proba(imp_1[e_var_3])[:,1]

    lkl_1 = (preds_e_1 ** imp_1["a1"]) * ((1-preds_e_1) ** (1-imp_1["a1"]))
    lkl_2 = (preds_e_2 ** imp_1["a2"]) * ((1-preds_e_2) ** (1-imp_1["a2"])) * (imp_1["phi_2"]==False) * (imp_1["a1"]==0) + (1-1*(imp_1["phi_2"]==False) * 1*(imp_1["a1"]==0))
    lkl_3 = (preds_e_3 ** imp_1["a3"]) * ((1-preds_e_3) ** (1-imp_1["a3"])) * (imp_1["phi_3"]==False) * (imp_1["a2"]==0) + (1-1*(imp_1["phi_3"]==False) * 1*(imp_1["a2"]==0))
    
    conc_1 = 1*(imp_1[r_vec[0]]==imp_1["a1"])
    conc_2 = 1*(imp_1[r_vec[1]]==imp_1["a2"])
    conc_3 = 1*(imp_1[r_vec[2]]==imp_1["a3"])
    conc = conc_1 * conc_2 * conc_3

    mean_RRT_pi = np.sum( imp_1['rwd'] * ( conc / (lkl_1 * lkl_2 * lkl_3))) / np.sum( conc / (lkl_1 * lkl_2 * lkl_3))
    mean_RRT_obs = np.mean( imp_1['hmor'])
    
    return np.array([mean_RRT_pi, mean_RRT_obs]) 
                           


# In[24]:


n_rrt_fun(prep_imp_dfs[2], r_vec=['r_1', 'r_2', 'r_3'])


# In[25]:


n_rrt_fun(prep_imp_dfs[2], r_vec=['all_1', 'all_2', 'all_3'])


# In[26]:


# Bootrap the whole procedure as presented in the flowchart
def procedure_n_rrt(dat):
    df = dat.copy()
    df = df.append(df[df["brasrando"]=='standard'])
    res =  np.hstack([n_rrt_fun(dat, r_vec=['r_1', 'r_2', 'r_3']),
                     n_rrt_fun(dat, r_vec=['r_s_1', 'r_s_2', 'r_s_3']),
                     n_rrt_fun(dat, r_vec=['all_1', 'all_2', 'all_3'])]
                    )
    return res


# In[27]:


procedure_n_rrt(prep_imp_dfs[0])


# In[28]:


#Bootstrap all datasets
def runboot(df):
    sb_obj = speedboot(data=df, stats_fun=procedure_n_rrt)
    sb_obj.fit(R=99, bar=True, par=True, seed=123)
    sb_obj.per = sb_obj.per_ci()
    sb_obj.emp = sb_obj.emp_ci()
    
    #sb_obj.jackknife(bar=True, par=True)
    #sb_obj.bca = sb_obj.bca_ci()
    return sb_obj

sb_cf_alldf = [runboot(prep_imp_dfs[i]) for i in range(len(prep_imp_dfs))]


# In[29]:


est_alldf = np.mean(np.array([sb_cf_alldf[i].ests for i in range(len(sb_cf_alldf))]), axis=0)
est_alldf


# In[30]:


emp_alldf = np.mean(np.array([sb_cf_alldf[i].emp for i in range(len(sb_cf_alldf))]), axis=0)
emp_alldf

