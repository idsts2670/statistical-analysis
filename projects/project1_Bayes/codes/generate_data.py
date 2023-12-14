import numpy as np
import pandas as pd
from tqdm import tqdm
from scipy.stats import truncnorm

def get_Y(a,b,x):
    return truncnorm.rvs(a,b,loc=x,scale=1,size=1)[0]

def sigmoid(x):
    return np.exp(x) / (1+np.exp(x))

beta = np.array([1.0,-0.8,0.5,-2.0,0.5])
tau = 0.457068 - 0.359613 


import random
random.seed(1234)
np.random.seed(1234)

N_sim = 200
N = 1000
phi = 50 # dispersion parameter

eps = [(10,10,10),(1,1,1),(3,3,3),(0.1,0.1,0.1)]#,(2.0,0.5,0.5),(0.5,2.0,0.5),(0.5,0.5,2.0),(0.5,1.25,1.25),(1.25,0.5,1.25),(1.25,1.25,0.5)]
for e in eps:
    for n_sim in tqdm(range(N_sim)):
        eps_x = e[0]
        eps_y = e[1]
        eps_z = e[2]
        p = 0.5
        q = np.exp(eps_z) / (1+np.exp(eps_z))
        C = (q+p-2*q*p)*(1-q-p+2*p*q)/((2*q-1)*p*(1-p))
        r = p*q + (1-p)*(1-q)
        X0 = np.ones(N)
        X1 = np.random.uniform(0,1,N)
        X2 = np.random.beta(2,5,N)
        X3 = np.random.binomial(1,0.7,N)
        X_treat = np.vstack([X0,X1,X2,X3,np.ones(N)]).T
        X_ctrl = np.vstack([X0,X1,X2,X3,np.zeros(N)]).T
        mu_ctrl = sigmoid(X_ctrl@beta)
        mu_treat = sigmoid(X_treat@beta)
        alpha_treat = mu_treat*phi
        alpha_ctrl = mu_ctrl*phi
        beta_treat = (1-mu_treat)*phi
        beta_ctrl = (1-mu_ctrl)*phi
        Y0 = np.random.beta(alpha_ctrl,beta_ctrl,size=N)
        Y1 = np.random.beta(alpha_treat,beta_treat,size=N)
        W = np.random.binomial(1,p,N)
        
        # observation
        X_obs = np.ones((N,4)) # remove the last element
        X_obs_tilde = np.ones((N,4))
        X_obs[W==0,:] = X_ctrl[W==0,0:4]
        X_obs[W==1,:] = X_treat[W==1,0:4]
        X_obs_tilde[:,1:4] = X_obs[:,1:4] + np.random.laplace(0,3/eps_x,(N,3))
        Y_obs = np.zeros(N)
        Y_obs_tilde = np.zeros(N)
        Y_obs[W==0] = Y0[W==0]
        Y_obs[W==1] = Y1[W==1]
        Y_obs_tilde = Y_obs + np.random.laplace(0,1/eps_y,N)
        W_tilde = np.array([w if np.random.binomial(1,q,1)[0] else 1-w for w in W])
        pd.DataFrame(np.concatenate([X_obs_tilde,Y_obs_tilde[:,None],W_tilde[:,None]], axis=1),columns=["X1","X2","X3","X4","Y","W"]).to_csv("input/data_{}_{}_{}_{}.csv".format(eps_x,eps_y,eps_z,n_sim), index=None)
        pd.DataFrame(np.concatenate([X_obs,Y0[:,None],Y1[:,None],W[:,None]], axis=1),columns=["X1","X2","X3","X4","Y0","Y1","W"]).to_csv("input/data_true_{}_{}_{}_{}.csv".format(eps_x,eps_y,eps_z,n_sim), index=None)