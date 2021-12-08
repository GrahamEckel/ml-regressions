import pandas as pd 
import numpy as np
import warnings; warnings.simplefilter('ignore')
from scipy.optimize import fmin_tnc

from Directories import logX
from Directories import logY

X = logX
y = logY

#assuming -1 is false and 1 is true, transforming -1 to 0
#series transformations and theta
y = logY.replace({'y' : {-1 : 0}})
y = y.T.iloc[0]
X = np.c_[np.ones((X.shape[0], 1)), X]
y = y[:, np.newaxis]
theta = np.zeros((X.shape[1], 1))

# Activation function, maps given values inside the range from 0 to 1
def sigmoid(x):
    return 1 / (1 + np.exp(-x))

# determines the weights used in the activation function
def net_input(theta, x):
    return np.dot(x, theta)

# Creates our hypothesis by computing probability after passing through activation function
def probability(theta, x):
    return sigmoid(net_input(theta, x))

# Computes the cost function for sample data
def cost_function(theta, x, y):
    m = x.shape[0]
    total_cost = -(1 / m) * np.sum(y * np.log(probability(theta, x)) + (1 - y) * np.log(1 - probability(theta, x)))
    #to inspect cost, if desired
    #print(total_cost)
    return total_cost

# runs gradient descent
def gradient(theta, x, y):
    m = x.shape[0]
    return (1 / m) * np.dot(x.T, sigmoid(net_input(theta, x)) - y)

def fit(x, y, theta):
    opt_weights = fmin_tnc(func=cost_function, x0=theta, fprime=gradient, args=(x, y.flatten()))
    return opt_weights[0]

parameters = fit(X, y, theta)

print("Our parameters are: B0 = %f, B1 = %f, B2 = %f" % (parameters[0], parameters[1], parameters[2]))

