#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar  8 13:44:33 2023

@author: cole
"""

import glob
import pandas as pd
import numpy as np

# =============================================================================
# constants
# =============================================================================
label_map = {"a":"accept","r":"reject","n":"ignore"}

def load_data(fn,dis=False):
    dx = pd.read_excel(fn)
    if dis:
        dx["language"]= fn.split("/")[-1].split("_")[1].lower()
    else:
        dx["language"] = fn.split("/")[-1].split("_")[0].lower()
    dx.columns = [x.lower() for x in dx.columns]
    return dx

def labeller(label_1,label_2,agreed_label):
    if label_1 == label_2:
        return label_1
    else:
        return agreed_label
    

# =============================================================================
# LOAD FIRST CODER AND CLEANS
# =============================================================================
first = pd.concat([load_data(fn) for fn in  glob.glob("../coding/*FirstCoding.xlsx")],axis=0)\
    .rename(columns={"annotator1_answer":"label_1"})

#counts number of classes per tweet_id 
first = first.merge(first.groupby(['tweet_id']).agg(num_unique = ("label_1","nunique")),
                    on="tweet_id")

#drops tweets with multiple classes and duplicate tweets
first = first.loc[first.num_unique == 1,:]\
    .drop_duplicates("tweet_id")\
    .reset_index(drop=True,inplace=False)\
    .drop("num_unique",axis=1)
    
#make sure tweet_id doesn't contain whitespace
first.tweet_id = first.tweet_id.apply(lambda x: int(str(x).strip()))

# =============================================================================
# LOAD SECOND CODER AND CLEANS
# =============================================================================
second = pd.concat([load_data(fn) for fn in  glob.glob("../SecondCoding/*secondCoder.xlsx")],axis=0)\
    .rename(columns={"decision":"label_2"})\
    .loc[:,["tweet_id","label_2"]]

#cleans up labels that were inputed oddly by hand in excel 
second.label_2 = second.label_2.apply(lambda code: code.lower().strip())
second.label_2 = second.label_2.apply(lambda lab: label_map[lab] if lab in label_map else lab)

#counts number of classes per tweet_id 
second = second.merge(second.groupby(['tweet_id']).agg(num_unique = ("label_2","nunique")),
                    on="tweet_id")

#drops tweets with multiple classes and duplicate tweets
second = second.loc[second.num_unique == 1,:]\
    .drop_duplicates("tweet_id")\
    .reset_index(drop=True,inplace=False)\
    .drop("num_unique",axis=1)

#make sure tweet_id doesn't contain whitespace
second.tweet_id = second.tweet_id.apply(lambda x: int(str(x).strip()))

# =============================================================================
# LOADS DISAGREEMENTS DECISION FILE AND CLEANS
# =============================================================================
dis = pd.concat([load_data(fn,dis=True) for fn in  glob.glob("../disagreements/*.xlsx")],axis=0)\
    .rename(columns={"finalcoding":"agreed_label"})\
    .drop(["firstcoding","secondcoding","language","text"],axis=1)

#cleans up labels that were inputed oddly by hand in excel 
dis.agreed_label = dis.agreed_label.apply(lambda code: code.lower().strip())
dis.agreed_label = dis.agreed_label.apply(lambda lab: label_map[lab] if lab in label_map else lab)

#counts number of classes per tweet_id 
dis = dis.merge(dis.groupby(['tweet_id']).agg(num_unique = ("agreed_label","nunique")),
                    on="tweet_id")

#drops tweets with multiple classes and duplicate tweets
dis = dis.loc[dis.num_unique == 1,:]\
    .drop_duplicates("tweet_id")\
    .reset_index(drop=True,inplace=False)\
    .drop("num_unique",axis=1)

#make sure tweet_id doesn't contain whitespace
dis.tweet_id = dis.tweet_id.apply(lambda x: int(str(x).strip()))

# =============================================================================
# MERGE DATA
# =============================================================================
dy = pd.merge(first,second,on="tweet_id",how="outer",indicator=True)\
    .rename(columns={"_merge":"first_merge"})

#check merge 
dy.first_merge.value_counts()

#drop non shared values
dy = dy.loc[dy.first_merge == "both",:]\
    .drop("first_merge",axis=1)\
    .reset_index(inplace=False,drop=True)

df = dy.merge(dis,on="tweet_id",how="outer",indicator=True)\
    .rename(columns={"_merge":"second_merge"})

#check merge 
df.second_merge.value_counts()

#drop values which were added only in the dis file (i.e. dropped when first or second coded disagreed with themselves)
dx = df.loc[df.second_merge != "right_only",:]\
    .drop("second_merge",axis=1)\
    .reset_index(inplace=False,drop=True)

# =============================================================================
# CREATE LABEL AND SUBSET
# =============================================================================
dx["label"] = dx.apply(lambda row: row.label_2 if row.label_1 == row.label_2 else row.agreed_label,axis=1)


#annotation data
dat = dx.loc[dx.label.isin(["accept","reject"]),:]
props  = pd.crosstab(dat.language,dat.label)
props.loc["total"] = props.sum(numeric_only=True, axis=0)
props["percent_accept"] = props.apply(lambda row: f"{round(row.accept/row.reject*100,2)}%",axis=1)

# =============================================================================
# SAVE 
# =============================================================================
dx.to_excel("../working_data/all_covid_data.xlsx",index=False)
dat.to_excel("../working_data/working_covid_data.xlsx",index=False)
props.to_excel("../working_data/label_props.xlsx",index=False)