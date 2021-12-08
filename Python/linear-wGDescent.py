import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
import warnings; warnings.simplefilter('ignore')
import seaborn as sns
from sklearn.model_selection import train_test_split
from ast import literal_eval

#Loading IMDB movies data
from Directories import meta_data
from Directories import ratings_small

#joining:
#getting an average rating by movieId, renaming cols, dropping invalid rows
ratings_small = ratings_small.drop(['userId', 'timestamp'], axis=1)
ratings_small = ratings_small.groupby("movieId").mean()
ratings_small = ratings_small.reset_index()
meta_data.rename({'id': 'movieId'}, axis=1, inplace=True)
meta_data = meta_data[pd.to_numeric(meta_data['movieId'], errors='coerce').notnull()]
meta_data = meta_data.astype({'movieId': 'int64'})
meta_data = meta_data.join(ratings_small.set_index('movieId'), on='movieId')

#cleaning:
#changing datatypes, dropping nans in numeric cols
meta_data['year'] = pd.to_datetime(meta_data['release_date'], errors='coerce').apply(lambda x: str(x).split('-')[0] if x != np.nan else np.nan)
meta_data = meta_data[pd.to_numeric(meta_data['year'], errors='coerce').notnull()]
meta_data = meta_data.astype({'budget': 'float', 'popularity': 'float', 'year': 'float'})
meta_data = meta_data.dropna(subset=['rating', 'budget', 'popularity', 'revenue', 'runtime', 'vote_average', 'vote_count'])

scatterPlot =  meta_data.plot.scatter(x='rating', y='revenue', c='darkblue')

#visualizing numerical correlation to support feature selection
numerics = meta_data[['rating', 'budget', 'popularity', 'revenue', 'runtime', 'vote_average', 'vote_count', 'year']]
matrix = np.triu(numerics.corr())
sns.heatmap(numerics.corr(), annot = True, fmt='.2g', mask=matrix, cmap = 'coolwarm')

#reloading dataset
from Directories import meta_data

#cleaning:
#changing datatypes, dropping nans in numeric cols
meta_data = meta_data[pd.to_numeric(meta_data['id'], errors='coerce').notnull()]
meta_data['year'] = pd.to_datetime(meta_data['release_date'], errors='coerce').apply(lambda x: str(x).split('-')[0] if x != np.nan else np.nan)
meta_data = meta_data[pd.to_numeric(meta_data['year'], errors='coerce').notnull()]
meta_data = meta_data.astype({'budget': 'float', 'popularity': 'float', 'year': 'float'})
meta_data = meta_data.dropna(subset=['budget', 'popularity', 'revenue', 'runtime', 'vote_average', 'vote_count', 'year'])

#re-visualizing numerical correlation
numerics = meta_data[['budget', 'popularity', 'revenue', 'runtime', 'vote_average', 'vote_count', 'year']]
matrix = np.triu(numerics.corr())
sns.heatmap(numerics.corr(), annot = True, fmt='.2g', mask=matrix, cmap = 'coolwarm')

#train/test split, we use 2/3 train, 1/3 test as per the course lecture notes
X = meta_data[['vote_count', 'budget', 'popularity']].values
Y = meta_data[['revenue']].values

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.33)

#Performing Gradient Descent
def gradientDescent(x, y, theta, alpha, m, numIt):
    for i in range(0, numIt):
        hypothesis = np.dot(x, theta)
        loss = hypothesis - y
        cost = np.sum(loss ** 2) / (2 * m)
        #to inspect cost, if you'd like
        #print("Iteration %d | Cost: %f" % (i, cost))
        gradient = np.dot(x.transpose(), loss) / m
        theta = theta - alpha * gradient
    return theta

#removing data where revenue = nan or 0 for simplicity sake
meta_data.dropna(subset=['revenue'])
meta_data = meta_data[meta_data['revenue'] != 0]

#gradient descent iterations, alpha and observations
numIt = 100000
alpha = 0.001
m = len(meta_data[['vote_average']])

#feature array, simple OLS so only intercept (x0=1), and one feature
x0 = np.ones((m,1))
x1 = meta_data[["vote_average"]].values
x1[np.isnan(x1)] = 0
x = np.concatenate((x0, x1), axis=1)

#independent array
y = meta_data[["revenue"]].values.flatten()
y[np.isnan(y)] = 0

#initial theta values of 1 for two thetas
n = 2
theta = np.ones(n)
 
#calling the function
theta = gradientDescent(x, y, theta, alpha, m, numIt)
print("Our parameters are: B0 = %f and B1 = %f" %(theta[0], theta[1]))

#visualizing our model
plt.plot(x1, y, 'o')
plt.xlabel("Vote Average")
plt.ylabel("Revenue (in billions)")
plt.title("Linear Regression using Gradient Descent")
plt.plot(x1, theta[0] + theta[1]*x1)
plt.show()
