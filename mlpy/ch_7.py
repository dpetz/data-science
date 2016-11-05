# -*- coding: utf-8 -*-
"""
Examples from 'Building Machine Learning Systems Using Python'

@Petzoldt, Sep 2014
"""


# init

if __IPYTHON__:
    # increase plot size
    pylab.rcParams['figure.figsize'] = (10.0, 8.0) 
else:
    import numpy as np
    from matplotlib import pyplot as plt

# Load Boston Housing data
from sklearn.datasets import load_boston
boston = load_boston()
boston.data.size
boston.feature_names

# Analyse avg. no of rooms vs. price
x = boston.data[:,5]
y = boston.target
plt.scatter(x, y, color='r')

# Regress via numpy's lstsq

# make x matrix with features as columns (just one this time)
x = np.array([[v,1] for v in x])

# Ignore rank and singular values.
(slope,bias),total_error,_,_ = np.linalg.lstsq(x,y)
rmse = np.sqrt(total_error / len(x))

def plot_lstsq(x,y,slope,bias,rmse):
    """ Plot regression line, confidence band and original data """
    x_min, x_max = x.min(), x.max()
    # Root Mean Squared Error is estimate of standard deviation
    plt.plot(x,y,'+', \
        (x_min,x_max), (bias + x_min*slope,bias+x_max*slope),'r-', \
        (x_min,x_max), (bias + x_min*slope+rmse*2,bias+x_max*slope+rmse*2),'r--', \
        (x_min,x_max), (bias + x_min*slope-rmse*2,bias+x_max*slope-rmse*2),'r--')
        
    plt.title("Least Square Estimate: Bias %1.3f, Slope %1.3f, Confidence Level 0.95" % (bias, slope))

plot_lstsq(x[:,0],y,slope,bias,rmse)

# Multidimensional regression

# Append offset to each row of predictors
x = np.array([np.concatenate((v,[1]) for v in boston.data])
y=boston.target
s,total_error,_,_=np.linalg.lstsq(x,y)

# Same model with scikit-learn
from sklearn.linear_model import LinearRegression
lr = LinearRegression(fit_intercept=True) # specify
lr.fit(x,y) # train
p = map(lr.predict,x) # score

# Training RMSE
e = p - y
total_error  = np.sum(e*e)
rmse_train = np.sqrt(total_error / len(p))
print('RMSE on training: {}'.format(rmse_train))

# k-fold cross validation

from sklearn.cross_validation import KFold
kf = KFold(len(x),n_folds=10)
err = 0

for train,test in kf:
    lr.fit(x[train],y[train])
    p = map(lr.predict,x[test])
    e = p - y[test]
    err += np.sum(e*e)
    
rmse_10fold = np.sqrt(err / len(x))
print('RMSE on 10fold CV: {}'.format(rmse_10fold))
