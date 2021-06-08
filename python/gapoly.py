from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted

import os
import tempfile
import subprocess
import numpy as np 

class GAPoly(BaseEstimator, RegressorMixin):

    def __init__(self, nGens, nPop, nTerms, maxK, pm):
        self.nGens = nGens
        self.nPop  = nPop
        self.nTerms = nTerms 
        self.maxK = maxK
        self.pm = pm

        self.tmpdir = tempfile.mkdtemp()

    def fit(self, X_train, y_train):
        X_train, y_train = check_X_y(X_train, y_train, accept_sparse=False)
        if len(y_train.shape) == 1:
            Z_train = np.hstack((X_train, y_train[:,None]))
        else:
            Z_train = np.hstack((X_train, y_train))
        fname = self.tmpdir + "/tmpdata.csv"

        np.savetxt(fname, Z_train, delimiter=",")

        cwd = os.path.dirname(os.path.realpath(__file__))
        self.expr = eval(subprocess.check_output(["./Wrapper", f"{fname}", f"{self.nGens}", f"{self.nPop}", f"{self.nTerms}", f"{self.maxK}", f"{self.pm}"]))

        self.is_fitted_ = True

    def predict(self, X_test):
        check_is_fitted(self)
        X_test = check_array(X_test, accept_sparse=False)
        return self.eval_expr(X_test)

    def eval_expr(self, x):
        return eval(self.expr)
