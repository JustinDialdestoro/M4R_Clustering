import numpy as np
import sklearn.metrics as skm
import scipy.stats as ss
from sklearn.neighbors import BallTree

def gen_cos(UI, user):
    """Generates user similarity matrix based on cosine similarity"""
    # ensure UI matrix has no NaN
    # UI = np.nan_to_num(UI)
    if user:
        return skm.pairwise.cosine_similarity(UI)
    else:
        return skm.pairwise.cosine_similarity(UI.T)
    
def gen_pcc(UI, user):
    """Generates user similarity matrix based on Pearson's Correlation Coefficient"""
    # ensure UI matrix has no NaN
    # UI = np.nan_to_num(UI)
    
    # adjust user-item matrix by subtracting mean of users
    if user:
        UI_adj = UI - np.mean(UI, axis=1).reshape(-1,1)
        num = UI_adj.dot(UI_adj.T)
    else:
        UI_adj = UI - np.mean(UI, axis=0).reshape(1,-1)
        num = UI_adj.T.dot(UI_adj)
    
    denom = np.array([np.sqrt(np.diagonal(num))]) + 1e-9
    return num/denom/denom.T

def gen_srcc(UI, user):
    """Generates user similarity matrix based on Spearman rank-order correlation coefficient"""
    # ensure UI matrix has no NaN
    # UI = np.nan_to_num(UI)
    if user:
        return ss.spearmanr(UI, axis=1, nan_policy='propagate')[0]
    else:
        return ss.spearmanr(UI, axis=0, nan_policy='propagate')[0]
    
def gen_euc(UI, user):
    """Generates user similarity matrix based on Euclidean similarity"""
    if user:
        return 1/(1+skm.pairwise.euclidean_distances(UI))
    else:
        return 1/(1+skm.pairwise.euclidean_distances(UI.T))
    
def gen_mhat(UI, user):
    """Generates user similarity matrix based on Manhattan similarity"""
    if user:
        return 1/(1+skm.pairwise.manhattan_distances(UI))
    else:
        return 1/(1+skm.pairwise.manhattan_distances(UI.T))

def ktau(vec1, vec2):
    """Computes the Kendall's tau between two vectors"""
    return ss.kendalltau(vec1, vec2)[0]

def jacc(vec1, vec2):
    """Computes the Jaccard index between two vectors"""
    union = len(np.where((vec1>0) | (vec2>0))[0])
    intersection = len(np.where((vec1>0) & (vec2>0))[0])
    return intersection/union

def cheb(vec1, vec2):
    """Computes the Chebyshev distance between two vectors"""
    intersection = np.where((vec1>0) & (vec2>0))[0]
    if intersection.size>0:
        return np.max(abs(vec1[intersection]-vec2[intersection]))
    else:
        return 5
    
def gen_cheb_tree(UI, user, df):
    """Generates user similarity matrix based on Chebyshev distance"""
    if user:
        nusers = max(np.unique(df['userID']))
        sim = np.zeros((nusers, nusers))
        tree = BallTree(UI, metric=cheb)
        for i in range(nusers):
            dist, ind = tree.query([UI[i]], nusers)
            sim[i] = np.array([i for _, i in sorted(zip(ind[0], dist[0]))])
    
    return 1/(1+sim)

def gen_ktau_tree(UI, user, df):
    """Generates user similarity matrix based on Kendall's tau"""
    if user:
        nusers = max(np.unique(df['userID']))
        sim = np.zeros((nusers, nusers))
        tree = BallTree(UI, metric=ktau, leaf_size=40)
        for i in range(nusers):
            dist, ind = tree.query([UI[i]], nusers)
            sim[i] = np.array([i for _, i in sorted(zip(ind[0], dist[0]))])
    
    return sim

def gen_jacc_tree(UI, user, df):
    """Generates user similarity matrix based on Jaccard index"""
    if user:
        nusers = max(np.unique(df['userID']))
        sim = np.zeros((nusers, nusers))
        tree = BallTree(UI, metric=jacc)
        for i in range(nusers):
            dist, ind = tree.query([UI[i]], nusers)
            sim[i] = np.array([i for _, i in sorted(zip(ind[0], dist[0]))])
    
    return sim