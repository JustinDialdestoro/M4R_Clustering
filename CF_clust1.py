import numpy as np
import sklearn.metrics as skm
import CF

def user_cluster(UI, sim):
    """Cluster users based on rating preferences"""
    
    # count number of ratings in each row
    n_ratings = np.count_nonzero(UI, axis=1)
    
    # find average ratings for each user
    u_mean = np.sum(UI, axis=1)/n_ratings
    
    # divide users into their rating preferences
    o_ind = np.where(u_mean>=4)[0]
    p_ind = np.where(u_mean<=3)[0]
    
    # find centre of each cluster
    c_o = o_ind[np.argmax(n_ratings[o_ind])]
    c_p = p_ind[np.argmax(n_ratings[p_ind])]
    
    # cluster users into rating preferences
    cluster = [np.argmax(sim[i,[c_o, -1, c_p]]) for i in range(len(UI))]
    
    return np.array(cluster)

def cluster_knn(UI, sim, k, userid, filmid, cluster):
    """Finds the k nearest neighbours who have rated a given film for a given user"""
    
    if cluster[userid]==0:
        ind, = np.where((UI[:, filmid]>0)&(cluster!=2))
    elif cluster[userid]==2:
        ind, = np.where((UI[:, filmid]>0)&(cluster!=0))
    else:
        ind, = np.where((UI[:, filmid]>0)&(cluster==1))
        
    neighbours = ind[np.argsort(sim[:,userid][ind])[:-k-2:-1]]
    
    return [neighbours[1:]]

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

def cluster_cross_val(df, t, metric, krange):
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
        
        # neutral clustering centre
        c_n = np.sum(UI, axis=0)/(np.count_nonzero(UI, axis=0)+1e-9)
        
        cluster = user_cluster(UI, metric(np.vstack([UI, c_n]),True))
        RMSE, MAE, R2 = cluster_vary_k(df, UI, sim, cval_f_i[i], krange, cluster)
            
        # compute evaluation metrics for each k when testing on this fold
        RMSE_k[i] += RMSE
        MAE_k[i] += MAE
        R2_k[i] += R2
    
    return np.mean(RMSE_k,axis=0), np.mean(MAE_k,axis=0), np.mean(R2_k,axis=0)
