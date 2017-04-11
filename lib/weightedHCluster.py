# ziwei meng
# 2017-04-09
import numpy as np
import pandas as pd
#import numba
from scipy.misc import comb

def cleanData(dpath='../data/text1.csv'):
    df = pd.read_csv(dpath)
    df.journalTitle = df.journalTitle.fillna('')
    df.coauthor = df.coauthor.fillna('')
    df.journalTitle = [x.split("|") for x in df.journalTitle.tolist()]
    df.coauthor = [x.split("|") for x in df.coauthor.tolist()]
    features = ['coauthor','journalTitle']
    label = 'authorNum'
    my_df = df[features]
    my_df = my_df.assign(label=df[label].values)
    return(my_df)

def p_sml(r1,r2):
    '''
    parameters : r1,r2 are 2 array of arrays, e.g. [['a1','a2','a3'],['t1','t2']],[['a1','a4'],['t2','t5']]
                 w is an array, the weight parameter to adjust importance of each feature, shape is (n_feature,)
    output: sim is the similarity vector between r1 and r2
    '''
    k1 = np.intersect1d(r1[0],r2[0]).shape[0]
    k2 = np.intersect1d(r1[1],r2[1]).shape[0]
    sim = np.array((k1,k2))
    return sim  

def C_sml(d1,d2):
    '''
    parameters: d1, d2 are 2 clusters, each is an array of dim nc_records*n_feature
                w is the weight parameter, shape is (n_feature,)
    output: sim is the similarity vector between 2 clusters
    '''
    if len(d1.shape)==1:
        d1 = d1.reshape((1,d1.shape[0]))
    if len(d2.shape)==1:
        d2 = d2.reshape((1,d2.shape[0]))
    nc_1 = d1.shape[0]
    nc_2 = d2.shape[0]
    sim = 0
    for i in range(nc_1):
        for j in range(nc_2):
            sim += p_sml(d1[i,],d2[j,])
    return (sim/(nc_1*nc_2*1.0))          

def label2mat(t):
    '''
    input: t is a 1d-array of partition labels, shape is (n_records,), e.g. array([1,1,2,3])
    output: m is a 2d-array adjacency matrix of shape (n_records,n_records)
    '''
    n = t.shape[0]
    m = np.eye(n)
    for i in range((n-1)):
        for j in range((i+1),n):
            if (t[j]==t[i]):
                m[i,j] = 1
    m += (m.transpose() - np.eye(n))
    return(m)

def mat2label(m):
    '''
    input: m is a 2d-array adjacency matrix of shape (n_records,n_records)
    output: t is a 1d-array of partition labels, shape is (n_records,), e.g. array([1,1,2,3])
    '''
    n = m.shape[0]
    t = np.zeros(n)
    k = 1
    for i in range(n):
        if (t[i]==0):
            t[m[i,]==1] = k
            k += 1
    return(t)

def scoreTrue(t,t_star,vec=True):
    '''
    input: t,t_star are partitions of the dataframe, e.g. t = array([1,1,2,3,1])
           vec is True use label2mat, otherwise use t
    output: s is the precision of t based on ground truth t0
    '''
    if vec:
        m = label2mat(t)
        m_star = label2mat(t_star)
        n = m.shape[0]
        agree_pairs = np.sum(m==m_star)
    else:
        agree_pairs = 0
        n = t.shape[0]
        for i in range(n-1):
            for j in range((i+1),n):
                if (((t[i]==t[j])&(t_star[i]==t_star[j]))|((t[i]!=t[j])&(t_star[i]!=t_star[j]))):
                    agree_pairs += 1
    s = agree_pairs/(n*(n-1)*0.5)
    return(s)

def c_scoreTrue(i,j,t,t_star):
    '''
    input: i,j are the i-th and j-th cluster label in a t-partition
           t is the current partition, e.g. array([1,1,3,4,3])
           t_star is the 
    output: s is the precision of the new partition after merging i-th and j-th clusters
    '''
    uni_t = np.unique(t)
    new_t = np.copy(t)
    ind = ((t==uni_t[j])|(t==uni_t[i]))
    new_t[t==uni_t[j]] = uni_t[i]
    sub_before = t[ind]
    sub_after = new_t[ind]
    sub_star = t_star[ind]
    if (sum(ind)<=15):
        s = (scoreTrue(sub_after,sub_star,False)-scoreTrue(sub_before,sub_star,False))
    else:
        s = (scoreTrue(sub_after,sub_star)-scoreTrue(sub_before,sub_star))
    return s

def updateW(w,gw,gw_star,h=1):
    '''
    input: w is the weight parameter to be updated
           gw is the similarity vector for algorithm predicted pair of clusters
           gw_star is the similarity vector for true pair of clusters
           h is the threshold for weighted similarity of our wrong choice of merged clusters
    '''
    alpha = 0.001
    while((gw_star.dot(w)<gw.dot(w)+1)|(gw.dot(w)>h)):
        w += (gw_star - gw)*alpha
    return w

def predMerge(t,X,w):
    '''
    input: t is a 1d-array of partition labels, shape is (n_record,)
           X is a 2d-array of records values, not including label, shape is (n_records, n_feature)
           w is a 1d-array of importance weight, shape is (n_feature,)
           t_star is a 1d-array of true partition labels, shape is (n_record,)
    output: best_t is the new partition which maximize the weighted average similarity between each pair of clusters
    '''
    uni_t = np.unique(t)
    n_c = uni_t.shape[0]
    #best_t = t
    best_ind = [-1,-1]
    max_score = -1
    gw = np.zeros(w.shape)
    for i in range(n_c-1):
        for j in range((i+1),n_c):
            this_sim = C_sml(X[t==uni_t[i],],X[t==uni_t[j],])
            this_score = this_sim.dot(w)
            if (this_score>max_score):
                max_score = this_score
                best_ind = [i,j]
                gw = this_sim
                #new_t = (t[t==uni_t[best_ind[1]]]=uni_t[best_ind[0]])
                #if (scoreTrue(new_t,t_star)<scoreTrue(t,t_star)):
                #    pass #update w
    new_t = np.copy(t)
    new_t[t==uni_t[best_ind[1]]] = uni_t[best_ind[0]]
    #return(new_t)
    return (best_ind,new_t,gw)

def createTree(t0,X,w,R):
    '''
    input: t0 is the initial partition of shape (n_record,)
           X is the 2-d array of record features, n_record*n_feature
           w is the weight of shape (n_feature,)
           R is the number of partitions in a sequence
    output: T is a sequence of  partitions, T_merge is the predicted merge clusters index, G_W is a sequence 
    of gradience at w
    '''
    T = [t0]
    T_merge = []
    G_W = []
    for r in range(R):
        ind,t,gw = predMerge(T[r],X,w)
        T.append(t)
        T_merge.append(ind)
        G_W.append(gw)
        print(r)
    return (T,T_merge,G_W)

def trueMerge(t,X,t_star,best_ind,gw):
    '''
    input: t is the partition before merge
           X is the data 2d-array
           t_star is the ground truth partition
           best_ind is the predicted best merge indexes
           gw is the predicted gradience
    output: best_ind is updated to the best merge indexes under true score, gw_star is the gradience 
    of true score at w
    '''
    uni_t = np.unique(t)
    n_c = uni_t.shape[0]
    max_score = c_scoreTrue(best_ind[0],best_ind[1],t,t_star)
    gw_star = np.copy(gw)
    #max_score = scoreTrue(t_1,t_star)
    #best_t = np.copy(t)
    #best_ind = [-1,-1]
    #max_score = -1
    for i in range(n_c-1):
        for j in range((i+1),n_c):
            this_sim = C_sml(X[t==uni_t[i],],X[t==uni_t[j],])
            #this_t = np.copy(t)
            #this_t[t==uni_t[j]] = uni_t[i]
            this_score = c_scoreTrue(i,j,t,t_star)
            #this_score = scoreTrue(this_t,t_star)
            if (this_score>max_score):
                max_score = this_score
                best_ind = [i,j]
                gw_star = this_sim
                #best_t = this_t
                break
    return (best_ind,gw_star)

def train_errorDriven(my_df,features,n_run=100,R=500,th=0.00001):
    '''
    input: my_df the cleaned-up dataframe
           features is a list of features to be caculated
           n_run is the number of iterations
           R is the deepth of partition tree
           th is the converge threshold
    output: w0 is the best weight for similarity
    '''
    X = my_df.ix[:,features].values
    t_star = my_df['label'].values
    t0 = np.array([i for i in range(t_star.shape[0])])
    w0 = np.random.uniform(size=(1,len(features)))

    for i in range(n_run):
        w = np.copy(w0)
        T,T_merge,G_W = createTree(t0,X,w,R)
        for i in range(len(T)-1):
            true_merge,gw_star = trueMerge(T[i],X,t_star,T_merge[i],G_W[i])
            if (true_merge!=T_merge[i]):
                w0 = updateW(w,G_W[i],gw_star)
                break
            
        if ((w-w0)^2<th):
            break
    return w0






if __name__=="__main__":
    df = cleanData('../data/text1.csv')
    features = ['coauthor','journalTitle']
    w = train_errorDriven(df,features,100,550,0.001)
    numpy.savetxt("../output/weight.csv", a, delimiter=",")