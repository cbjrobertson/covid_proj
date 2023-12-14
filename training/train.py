import os
import numpy as np
import random
import pandas as pd

import tensorflow as tf
import ktrain
from ktrain import text

from constants import (
  TEXT_COL, 
  LABEL_COL,
  BATCH,
  EPOCHS,
  MET,
  EXP_NAME,
  CHK,
  LEARN_RATE
  )

def reset_random_seeds(seed=42):
    os.environ['PYTHONHASHSEED']=str(seed)
    tf.random.set_seed(seed)
    np.random.seed(seed)
    random.seed(seed)
    
def multimax(lst):
    return list(np.argwhere(lst == np.amax(lst)).flatten())
    
def load_best_epoch(learner,metric):
    if metric == "val_loss":
        res = learner.history.history[metric]
        return res.index(min(res)) + 1
    elif metric == "val_accuracy":
        res = learner.history.history[metric]
        return res.index(max(res)) + 1
    elif metric == "both":
        accu = learner.history.history["val_accuracy"]
        loss = learner.history.history["val_loss"]
        dat = pd.DataFrame(dict(acc = accu,
                                lss = loss))
        best_acc = dat.loc[multimax(dat.acc.to_list()),:]
        res = best_acc.loc[best_acc.lss == best_acc.lss.min(),:].index[0]
        return res + 1

class KTrans(text.Transformer):
    model_load_type = "ktrain"
    def __init__(self,
                 model_name,
                 lr_dict=None,
                 class_weight=None
                ):
        super().__init__(model_name)
        self.model_name = model_name
        self.lr_dict = lr_dict
        self.class_weight = class_weight
        self.best_epoch = None
            
    
    def make_data(self,train,val=None):
        X_train=train.loc[:,TEXT_COL].values
        y_train=train.loc[:,LABEL_COL].values
        if val:
            X_val=val.loc[:,TEXT_COL].values
            y_val=val.loc[:,LABEL_COL].values
        
        #preprocess
        self.trn = self.preprocess_train(X_train, y_train)
        if val:
            self.val = self.preprocess_test(X_val, y_val)
        else:
            self.val = val
            
    def make_learner(self):
        model = self.get_classifier()
        return ktrain.get_learner(model, train_data=self.trn, val_data = self.val,batch_size=BATCH)
        
    def run_learning_rate(self,train,val,max_epochs):
        self.make_data(train,val)
        learner = self.make_learner()
        learner.lr_find(show_plot=True, suggest=True,max_epochs=max_epochs)
        
    def run_fit(self,train,val,save_best=True):
        self.make_data(train,val)
        self.learner = self.make_learner()
        
        save = f'{EXP_NAME}/{CHK}/{self.model_name}'
        if self.lr_dict:
            self.learner.fit_onecycle(self.lr_dict[self.model_name], EPOCHS,class_weight=self.class_weight,checkpoint_folder=save)
        else:
            self.learner.fit_onecycle(LEARN_RATE, EPOCHS,class_weight=self.class_weight,checkpoint_folder=save)
        if save_best:
            self.best_epoch = load_best_epoch(self.learner,MET)
            print(f"best epoch was: {self.best_epoch}")
            if self.best_epoch == EPOCHS-1:
                print("Best epoch was last epoch")
            else:
                self.learner.model.load_weights(f'{save}/weights-{self.best_epoch:02d}.hdf5')
        
    def save_predictor(self,save_path):
        predictor = ktrain.get_predictor(self.learner.model,self)
        predictor.save(save_path)
        
    def run_predict(self,data,load_path=None,predict_proba=True):
        data=data.loc[:,TEXT_COL].values
        #load predictor
        if load_path:
            predictor = ktrain.load_predictor(load_path)
        else:
            predictor = ktrain.get_predictor(self.learner.model, self)
        if predict_proba:
            return predictor.predict_proba(data)
        else:
            return np.array(predictor.predict(data))