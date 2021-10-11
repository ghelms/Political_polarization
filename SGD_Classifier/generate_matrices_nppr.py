"""
Author: ngoet
First version: 28th August 2016
This version: 29.9.2018
"""
###############
#libraries
###############
import pandas as pd
import math
import os
import numpy as np
import re
import random
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer

import random
import csv
from sklearn.model_selection import StratifiedKFold

import scipy.sparse
from scipy.sparse import csc_matrix
import sys

output_dir = "machine_learning_implementations/matrices_nppr/"

###############
#define functions
###############
def keep_unique(elements):
    return list(dict.fromkeys(elements).keys())

###############
#functions
###############
def mean(numbers):
    return float(sum(numbers)) / max(len(numbers), 1)

###############
#import file
###############
speech_file = "raw_data/ukhcdeb_nppr.csv"


###############
#set up file that we write out to
###############
from_session = int(sys.argv[1]) #specify from which session to start

df=pd.read_csv(speech_file, sep=',', encoding = "ISO-8859-1")

sessions = pd.DataFrame(df,columns=['session_indicator'])
sessions = sessions['session_indicator'].unique().tolist()

custom_list = ["a","an","and","are","as","at","be","by","for","from",
				"has","he","in","is","it","its","of","on","that","the",
				"to","was","were","will","with"] #as per https://nlp.stanford.edu/IR-book/html/htmledition/dropping-common-terms-stop-words-1.html

# vectorizer = TfidfVectorizer(ngram_range=(1,2),token_pattern=r'\b\w+\b', min_df=1,stop_words = custom_list) #don't remove stopwords!
vectorizer = CountVectorizer(ngram_range=(1,2),token_pattern=r'\b\w+\b', min_df=1,stop_words = custom_list) #don't remove stopwords!

for x in sessions:

	if x >= from_session:
		train_speeches_matrices = []
		test_speeches_matrices = []
		train_speeches_data = []
		test_speeches_data = []
		all_test_targets = []
		all_train_targets = []
		
		iter_csv = pd.read_csv(speech_file, iterator=True, chunksize=100000)
		session_data = pd.concat([chunk[chunk['session_indicator'] == x] for chunk in iter_csv])

		#generate MP_ids
		session_data.index = range(len(session_data))

		year = session_data.iloc[0]['year']
		session = session_data.iloc[0]['session_ref']
		print(session)
		print(x)

		
		session_data['counts'] = [sum(session_data['party'] == session_data['party'][i]) for i in range(len(session_data))]
		
		#account for at least 100
		session_data = session_data.loc[session_data['counts'] >= 100]

		#exclude certain parties
		mask = session_data['party'].isin(['independent','unknown',"INDEPENDENT","UNKNOWN","IND"])
		
		session_data = session_data[~mask]

		#exclude the speaker
		mask = session_data['speaker'].isin(['m speaker','t speaker',"deputyspeaker","d speaker","madam deputy speaker"])

		session_data = session_data[~mask]

		mask = session_data['matched_name'].isin(['m speaker','t speaker',"deputyspeaker","d speaker","madam deputy speaker"])

		session_data = session_data[~mask]
		###############
		#training data
		###############
		session_data.index = range(len(session_data))
		accuracy_values = []

		if len(session_data.index) >= 100 and len(session_data['party'].unique()) >= 2:

			sparse_matrix = vectorizer.fit_transform(session_data['speech_content'].values)
			sparse_matrix = scipy.sparse.csc_matrix(sparse_matrix)
			scipy.sparse.save_npz((output_dir + "sparse_matrix_"+str(session)+".npz"), sparse_matrix)
			sparse_matrix = []

			skf = StratifiedKFold(n_splits=20,shuffle=True,random_state = 1234)
			
			for i, (train_index, test_index) in enumerate(skf.split(session_data,session_data['party'].values)):
				
				test = session_data.loc[test_index]

				train = session_data.loc[train_index]

				train["train_index"] = train_index
				test["test_index"] = test_index

				###########################
				#get all data and write out
				###########################		
				output_data_train = pd.DataFrame({
					"obs_ids": train['obs_ids'].values,
					"train_targets": train['party'].values,
					"train_index": train["train_index"].values
					})

				output_data_test = pd.DataFrame({
					"obs_ids": test['obs_ids'].values,
					"test_index": test['test_index'].values,
					"test_targets": test['party'].values				
					})

				output_data_train.to_csv((output_dir + "train_matrices_contextual_data_"+str(session)+"_"+str(i)+".csv"), sep=',')				
				print("saved file: " + ("train_matrices_contextual_data_"+str(session)+"_"+str(i)+".csv"))

				output_data_test.to_csv((output_dir + "test_matrices_contextual_data_"+str(session)+"_"+str(i)+".csv"), sep=',')				
				print("saved file: " + ("test_matrices_contextual_data_"+str(session)+"_"+str(i)+".csv"))

				#save space within loop
				test 				= []
				output_data_test 	= []

				train 				= []
				output_data_train 	= []
				

			#save data outside loop
			session_data = []

				
		
