import numpy as np
import sklearn.metrics as skm
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
    ind, = np.where((UI[:, filmid]>0)&(cluster[:,0]==cluster[userid][0]))
    neighbours = ind[np.argsort(sim[:,userid][ind])[:-k-1:-1]]

    i=1
    while len(neighbours) < k:
        ind, = np.where((UI[:, filmid]>0)&(cluster[:,i]==cluster[userid][i]))
        neighbours = np.append(neighbours, ind[np.argsort(sim[:,userid][ind])[:-k-1:-1]])
        i+=1

    return [neighbours[:k]]

def cluster_pred_rating(df, predid, UI, sim, k, cluster):
    """Predicts the rating a user will give a film based on their k nearest neighbours"""
    ratings = []
    
    # find id of the user and film to be predicted
    userid = df['userID'][predid] - 1
    filmid = df['filmID'][predid] - 1
    
    # find k nearest neighbours
    neighbours = cluster_knn(UI, sim, k, userid, filmid, cluster)
    
    # compute the prediction
    num = sim[userid][tuple(neighbours)].dot(UI[:,filmid][tuple(neighbours)])
    denom = sum(abs(sim[userid][tuple(neighbours)])) + 1e-9
    ratings.append(num/denom)
    
    return ratings

def cluster_pred(df, df_ind, UI, sim, k, cluster):
    """Predicts the ratings of all user-item pairs given user indexes"""
    pred = []
    
    # predict the rating for each user
    for predid in df_ind:
        pred.append(cluster_pred_rating(df, predid, UI, sim, k, cluster))
    
    return np.array(pred)

def cluster_vary_k(df, UI, sim, test_ind, k_range, cluster):
    """Performs T-fold cross validation using CF algorithm with specified 
    k nearest neighbours and similarity metric
    """
    RMSE = []
    MAE = []
    R2 = []
    
    # loop over each value of k
    for k in k_range:
        # obtain true ratings and predicted ratings
        r_pred = cluster_pred(df, test_ind, UI, sim, k, cluster)
        r_true = df['rating'][test_ind]
        
        # compute evaluation metrics
        RMSE.append(skm.mean_squared_error(r_true,r_pred))
        MAE.append(skm.mean_absolute_error(r_true,r_pred))
        R2.append(skm.r2_score(r_true,r_pred))
    return RMSE, MAE, R2

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

        RMSE, MAE, R2 = cluster_vary_k(df, UI, sim, cval_f_i[i], krange, cluster)
            
        # compute evaluation metrics for each k when testing on this fold
        RMSE_k[i] += RMSE
        MAE_k[i] += MAE
        R2_k[i] += R2
    
    return np.mean(RMSE_k,axis=0), np.mean(MAE_k,axis=0), np.mean(R2_k,axis=0)