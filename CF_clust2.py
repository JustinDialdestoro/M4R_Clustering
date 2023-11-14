import numpy as np
import CF
import CF_clust1

def dis_fac(cat, n, catsizes, normfactor=2, qfactor=1/2):
    """Computes the factor to standardise categorical dummy variable data"""
    
    overall = (n+1)*n/2
    
    wit = 0
    for i in range(cat):
        wit += (catsizes[i]+1)*catsizes[i]/2
    bet = overall-wit
    
    return np.sqrt(qfactor*normfactor*overall/(2*bet))

def standardise(X):
    """Standardises a continuous variable"""
    mu = np.mean(X)
    std = np.std(X)
    return (X-mu)/std

def cluster_knn(UI, sim, k, userid, filmid, cluster):
    """Finds the k nearest neighbours who have rated a given film for a given user"""
    # ignoring the nearest neighbours (which is itself) add the index of each neighbour to list
    ind, = np.where((UI[:, filmid]>0)&(cluster==cluster[userid]))
        
    neighbours = ind[np.argsort(sim[:,userid][ind])[:-k-2:-1]]
    
    return [neighbours[1:]]

def cluster_cross_val(df, t, metric, krange, cluster):
    """Computes average evaluation metrics for a CF algorithm for varying 
    choices of k neighbours with specified similarity metric
    """
    RMSE_k = [[] for t in range(t)]
    MAE_k = [[] for t in range(t)]
    R2_k = [[] for t in range(t)]
    
    # generate fold indexes
    cval_f_i = CF.t_fold_index(df, t)
    # splits data into t training-test folds
    cval_f = CF.t_fold(df, cval_f_i)    
    
    # loop over each fold
    for i in range(t):
        # generate UI and similarity matrix for this fold
        UI = CF.gen_ui_matrix(cval_f[i], df)
        sim = metric(UI, True)

        RMSE, MAE, R2 = CF_clust1.cluster_vary_k(df, UI, sim, cval_f_i[i], krange, cluster)
            
        # compute evaluation metrics for each k when testing on this fold
        RMSE_k[i] += RMSE
        MAE_k[i] += MAE
        R2_k[i] += R2
    
    return np.mean(RMSE_k,axis=0), np.mean(MAE_k,axis=0), np.mean(R2_k,axis=0)