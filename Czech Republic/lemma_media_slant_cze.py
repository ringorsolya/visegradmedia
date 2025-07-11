# -*- coding: utf-8 -*-
"""
Created on Wed Apr  3 07:30:52 2024

@author: Stauber - Admin
"""

import simplemma
import pandas as pd

df = pd.read_csv(r"D:\media_slant\database\media_vocabulary_cz.csv", sep=";")

vocab = df["value"].tolist()

lemma = []

for token in vocab:
    try:
        lemma.append(simplemma.lemmatize(token, lang='cs', greedy=True))
    except:
        lemma.append(token)
        
pd.DataFrame({'value' : vocab,
             'lemma' : lemma }, 
             columns=['value','lemma'])