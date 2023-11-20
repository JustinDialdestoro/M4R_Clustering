# import necessary packages
import numpy as np
import pandas as pd
import sklearn.metrics as skm

def t_fold_index(df, t):
    """Generates the indices for t-folds"""
    # empty list of lists to contain each fold
    fold_ind = [ [] for i in range(t)]
    
    # iterate through each unique user id to partition each users ratings evenly
    for i in np.unique(df['userID']):
        # find the indices of user i's ratings and permute them
        i_perm = np.random.permutation(df.loc[df['userID']==i].index)
        
        # number of ratings in each fold
        k = len(i_perm)//t
        
        # add the partitioned indices into each fold
        for j in range(t):
            fold_ind[j] = np.append(fold_ind[j], i_perm[j*k:(j+1)*k])
        
        # randomly assign remaining indices (from division) to a fold
        f = np.random.randint(0,10)
        fold_ind[f] = np.append(fold_ind[f], i_perm[10*k:])
    
    return fold_ind

def t_fold(df, indexes):
    """Splits data into T randomly permuted folds"""
    # number of folds
    t = len(indexes)
    
    # list to contain each training set dataframe
    folds = [df.drop(indexes[i], axis=0, inplace=False) for i in range(t)] 
    
    return folds

def gen_ui_matrix(df, df_o):
    """Generates a user-item matrix based of given data"""
    UI = np.zeros((max(np.unique(df_o['userID'])),max(np.unique(df_o['filmID']))))
    # UI[:] = np.nan
    
    # set entries in the matrix to corresponding ratings
    for row in df.itertuples():
        UI[row[1]-1,row[2]-1] = row[3]
    
    return UI

def find_knn(UI, sim, k, userid, filmid, user):
    """Finds the k nearest neighbours who have rated a given film for a given user"""
    # ignoring the nearest neighbours (which is itself) add the index of each neighbour to list
    if user:
        ind, = np.where(UI[:, filmid]>0)
        neighbours = ind[np.argsort(sim[:,userid][ind])[:-k-1:-1]]
    else:
        ind, = np.where(UI[userid,:]>0)
        neighbours = ind[np.argsort(sim[:,filmid][ind])[:-k-1:-1]]
    
    return [neighbours]

def pred_rating(df, predid, UI, sim, k, user):
    """Predicts the rating a user will give a film based on their k nearest neighbours"""
    ratings = []
    
    # find id of the user and film to be predicted
    userid = df['userID'][predid] - 1
    filmid = df['filmID'][predid] - 1
    
    # find k nearest neighbours
    neighbours = find_knn(UI, sim, k, userid, filmid, user)
    
    # compute the prediction
    if user:
        num = sim[userid][tuple(neighbours)].dot(UI[:,filmid][tuple(neighbours)])
        denom = sum(abs(sim[userid][tuple(neighbours)])) + 1e-9
    else:
        num = sim[filmid][tuple(neighbours)].dot(UI[userid,:][tuple(neighbours)])
        denom = sum(abs(sim[filmid][tuple(neighbours)])) + 1e-9
    ratings.append(num/denom)
    
    return ratings

def pred(df, df_ind, UI, sim, k, user, me):
    """Predicts the ratings of all user-item pairs given user indexes"""
    pred = []
    
    # predict the rating for each user
    for predid in df_ind:
        pred.append(pred_rating(df, predid, UI, sim, k, user)+me[df['userID'][predid]-1])
    
    return np.array(pred)

def vary_k(df, UI, sim, test_ind, k_range, user, me):
    """Performs T-fold cross validation using CF algorithm with specified 
    k nearest neighbours and similarity metric
    """
    RMSE = []
    MAE = []
    R2 = []
    
    # loop over each value of k
    for k in k_range:
        # obtain true ratings and predicted ratings
        r_pred = pred(df, test_ind, UI, sim, k, user, me)
        r_true = df['rating'][test_ind]
        
        # compute evaluation metrics
        RMSE.append(skm.mean_squared_error(r_true,r_pred))
        MAE.append(skm.mean_absolute_error(r_true,r_pred))
        R2.append(skm.r2_score(r_true,r_pred))
    return RMSE, MAE, R2

def normalising_mat(UI):
    """Normalises a user item matrix"""
    # count number of ratings in each row
    n_ratings = np.count_nonzero(UI, axis=1)

    # find average ratings for each user
    row_means = np.sum(UI, axis=1)/n_ratings

    u_mean = np.where(UI >0, row_means.reshape(-1,1), 0)
    return u_mean, row_means

def cross_val(df, t, metric, krange, user=True):
    """Computes average evaluation metrics for a CF algorithm for varying 
    choices of k neighbours with specified similarity metric
    """
    RMSE_k = [[] for t in range(t)]
    MAE_k = [[] for t in range(t)]
    R2_k = [[] for t in range(t)]
    
    # generate fold indexes
    cval_f_i = t_fold_index(df, t)
    # splits data into t training-test folds
    cval_f = t_fold(df, cval_f_i)    
    
    # loop over each fold
    for i in range(t):
        # generate UI and similarity matrix for this fold
        UI = gen_ui_matrix(cval_f[i], df)
        UI_mean, means = normalising_mat(UI)
        sim = metric(UI-UI_mean, user)
        
        # compute evaluation metrics for each k when testing on this fold
        RMSE, MAE, R2 = vary_k(df, UI-UI_mean, sim, cval_f_i[i], krange, user, means)
        RMSE_k[i] += RMSE
        MAE_k[i] += MAE
        R2_k[i] += R2
    
    return np.mean(RMSE_k,axis=0), np.mean(MAE_k,axis=0), np.mean(R2_k,axis=0)

def find_best_k(scores, krange):
    """Given evaluation scores, find the best number k of neighbours and corresponding score"""
    best_k = {}
    
    rmse_ind = np.argmin(scores[0])
    mae_ind = np.argmin(scores[1])
    r2_ind = np.argmax(scores[2])
    
    best_k['RMSE'] = [krange[rmse_ind], scores[0][rmse_ind]]
    best_k['MAE'] = [krange[mae_ind], scores[1][mae_ind]]
    best_k['R^2'] = [krange[r2_ind], scores[2][r2_ind]]

    return best_k