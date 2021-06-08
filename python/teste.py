import numpy as np
import gapoly

Z = np.loadtxt("../Datasets/yacht.csv", delimiter=",")
X = Z[:, :-1]
y = Z[:,-1]

clr = gapoly.GAPoly(1000, 1000, 5, 5, 0.01)
clr.fit(X,y)

yhat = clr.predict(X)
print(np.square(y-yhat).mean())
