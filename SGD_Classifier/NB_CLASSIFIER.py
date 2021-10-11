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
from io import BytesIO
import re
import random
from sklearn.feature_extraction.text import TfidfVectorizer

from sklearn.metrics import accuracy_score
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.pipeline import Pipeline
from sklearn.naive_bayes import MultinomialNB
import csv

import io

import scipy.sparse

#performance of classifier
from sklearn import metrics

import sys 

###############
#define functions
###############
def keep_unique(elements):
    return list(dict.fromkeys(elements).keys())

###############
#select implementation
###############
implementation = str(sys.argv[1])

print("\nYou have chosen to run implementation "+str(implementation))

data_to_be_used = int(sys.argv[2])

if data_to_be_used == 1:
	DIR = "machine_learning_implementations/matrices_ppr"

	#set output location
	if implementation == "1a":
		output_dir = "machine_learning_implementations/1a_estimates_ps_NB/"

	if implementation == "1b":
		output_dir = "machine_learning_implementations/1b_estimates_ps_NB/"


elif data_to_be_used == 2:
	DIR = "machine_learning_implementations/matrices_nppr"

	#set output location
	if implementation == "1a":
		output_dir = "machine_learning_implementations/1a_estimates_nps_NB/"

	if implementation == "1b":
		output_dir = "machine_learning_implementations/1b_estimates_nps_NB/"


start_from = int(sys.argv[3])

numbers = re.compile(r'(\d+)')
def numericalSort(value):
    parts = numbers.split(value)
    parts[1::2] = map(int, parts[1::2])
    return parts

###############
#functions
###############
def mean(numbers):
    return float(sum(numbers)) / max(len(numbers), 1)

tfidf_transformer = TfidfTransformer()

session_indicators = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227]
session_refs = [4.5, 4.6, 5.1, 5.2, 5.3, 5.5, 5.6, 6.1, 6.2, 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 8.1, 8.2, 8.4, 9.1, 10.1, 10.2, 10.3, 11.1, 12.1, 12.2, 12.3, 13.1, 13.2, 13.3, 13.4, 13.5, 14.1, 14.2, 14.3, 14.4, 14.5, 14.6, 14.7, 15.1, 15.2, 15.3, 15.4, 15.5, 15.6, 16.1, 16.2, 16.3, 16.4, 16.5, 16.6, 16.7, 17.1, 17.2, 17.3, 17.4, 18.1, 18.2, 18.3, 18.4, 18.5, 18.6, 18.7, 19.1, 19.2, 19.4, 20.1, 20.2, 20.3, 20.4, 20.5, 20.6, 21.1, 21.2, 21.3, 21.4, 21.5, 21.6, 21.7, 21.8, 22.1, 22.2, 22.3, 22.4, 22.5, 22.6, 22.7, 22.8, 23.1, 24.1, 24.2, 24.3, 24.4, 24.5, 24.6, 24.8, 24.9, 25.1, 25.2, 25.3, 25.4, 26.1, 26.2, 26.3, 26.4, 26.5, 26.6, 26.7, 27.1, 27.2, 27.3, 27.4, 27.5, 27.6, 28.1, 28.2, 28.3, 28.4, 29.1, 30.1, 30.2, 30.3, 30.4, 30.5, 30.6, 30.7, 30.8, 31.1, 31.2, 31.3, 31.4, 31.5, 32.1, 32.2, 33.1, 34.1, 34.2, 34.3, 34.4, 34.5, 35.1, 35.2, 36.1, 36.2, 36.3, 36.4, 37.1, 37.2, 37.3, 37.4, 37.5, 37.6, 37.7, 37.8, 37.9, 37.11, 38.1, 38.2, 38.3, 38.4, 38.5, 39.1, 39.2, 40.1, 40.2, 40.3, 40.4, 41.1, 41.2, 41.3, 41.4, 42.1, 42.2, 42.3, 42.4, 42.5, 43.1, 43.2, 44.1, 44.2, 44.3, 44.4, 45.1, 45.2, 45.3, 45.4, 46.1, 47.1, 47.2, 47.3, 47.4, 47.5, 48.1, 48.2, 48.3, 48.4, 49.1, 49.2, 49.3, 49.4, 50.1, 50.2, 50.3, 50.4, 50.5, 51.1, 51.2, 51.3, 51.4, 51.5, 52.1, 52.2, 52.3, 52.4, 53.1, 53.2, 53.3, 53.4, 54.1, 54.2, 54.3, 54.4, 54.5, 55.1, 55.2, 55.3, 55.4]

session_dat = []

c = csv.writer(open(output_dir+"nb_scores_all_parties_1810_2015"+implementation+".csv", mode='w'))

if start_from == 1:
	c.writerow(["session_indicator","session_ref","score","best_alpha"])

c = csv.writer(open(output_dir+"nb_scores_all_parties_1810_2015"+implementation+".csv", mode='a'))

custom_list = ["a","an","and","are","as","at","be","by","for","from",
				"has","he","in","is","it","its","of","on","that","the",
				"to","was","were","will","with"] #as per https://nlp.stanford.edu/IR-book/html/htmledition/dropping-common-terms-stop-words-1.html

vectorizer = TfidfVectorizer(ngram_range=(1,2),token_pattern=r'\b\w+\b', min_df=1,stop_words = custom_list) #don't remove stopwords!

for x in range(len(session_indicators)):


	all_train_targets		= []
	all_weights				= []
	obs_ids_all				= []
	all_test_targets		= []
	train_speeches_matrices	= []
	test_speeches_matrices	= []

	test_indices = []
	train_indices = []
	
	session_ref = session_refs[x]
	session_indicator = session_indicators[x]


	if int(session_indicator) >= start_from:

		#set weights
		if "a" in implementation:
			alphas = [1e-4,1e-5,1e-6,1e-7]
			

		elif "b" in implementation:
			alphas = [1e-4,1e-5,1e-6,1e-7]
			

		###############
		#training data
		###############
		accuracy_values = []
		
		sorted_dir_files = sorted(os.listdir(DIR), key=numericalSort)

		for f in range(len(sorted_dir_files)):
			
			file = sorted_dir_files[f]
			
			if file.endswith(".csv"):
				if str(session_ref) in file.split("_") and len(str(session_ref)) == len(file.split("_")[4]):
					
					if "train_matrices" in file:
						fileref = DIR + "/" + file

						
						fileref = fileref.replace("._","")

						#read in as pandas data frame
						try:
							train_matrices_file = pd.read_csv(fileref)
							
							all_train_targets.append(train_matrices_file['train_targets'].values)
							train_indices.append(train_matrices_file['train_index'].values)

		
						except:
							pass

					elif "test_matrices" in file:
				
						fileref = DIR + "/" + file

						try:
							test_matrices_file = pd.read_csv(fileref)

							obs_ids_all.append(test_matrices_file['obs_ids'].values)
			
							all_test_targets.append(test_matrices_file['test_targets'].values)

							test_indices.append(test_matrices_file['test_index'].values)
						except:
							pass

			if file.endswith(".npz"):
				if str(session_ref) in file.split("_")[2]  and len(str(session_ref)) == len(file.split("_")[2].split(".n")[0]):
				
					if "sparse_matrix" in file:
						
						fileref = DIR + "/" + file

						sparse_matrix = scipy.sparse.load_npz(fileref)

						sparce_matrix = sparse_matrix

	
		###############
		#train model
		###############
		accuracies = []
		for alpha in alphas:
			implementation_accuracies = []
			for t in range(len(test_indices)):

				train_index = train_indices[t]
				train_data = tfidf_transformer.fit_transform(sparse_matrix[train_index])
				train_targets = all_train_targets[t]

				test_index = test_indices[t]
				prediction_data = tfidf_transformer.transform(sparse_matrix[test_index])
				test_targets = all_test_targets[t]

				#set weights for grid search
				if implementation in ["1b","2b"]:
					
					gs_clf = Pipeline([('clf', MultinomialNB(fit_prior=True,alpha=alpha)),])
					
					gs_clf.fit(train_data, train_targets)
					
					predictions = gs_clf.predict(prediction_data)
					
					accuracy = accuracy_score(test_targets,predictions,normalize=True)
					
					implementation_accuracies.append(accuracy)


				else:
						
					gs_clf = Pipeline([('clf', MultinomialNB(fit_prior=False,alpha=alpha)),])
					
					gs_clf.fit(train_data, train_targets)

					predictions = gs_clf.predict(prediction_data)
					
					accuracy = accuracy_score(test_targets,predictions,normalize=True)
					
					implementation_accuracies.append(accuracy)
					
			accuracies.append(mean(implementation_accuracies))
					
		#get best alpha
		best_alpha = alphas[accuracies.index(max(accuracies))]
		
		metrics_output = []
		aucs = []

		session_data = []
		full_parties = all_train_targets + all_test_targets
		
		number_of_parties 	= len(set([item for sublist in full_parties for item in sublist]))
		party_abbreviations = set([item for sublist in full_parties for item in sublist])
		party_abbreviations = [x for x in iter(party_abbreviations)]
		party_abbreviations = sorted(party_abbreviations)
		

		for ref in range(len(all_test_targets)):
			
			obs_ids = obs_ids_all[ref]

			train_index = train_indices[ref]
			train_data = tfidf_transformer.fit_transform(sparse_matrix[train_index])
			train_targets = all_train_targets[ref]

			test_index = test_indices[ref]
			prediction_data = tfidf_transformer.transform(sparse_matrix[test_index])
			test_targets = all_test_targets[ref]

			if implementation in ["1b","2b"]:

				gs_clf = Pipeline([('clf', MultinomialNB(fit_prior=True,alpha=alpha)),])
				
				gs_clf.fit(train_data, train_targets)

			else:
					
				gs_clf = Pipeline([('clf', MultinomialNB(fit_prior=False,alpha=alpha)),])
				
				gs_clf.fit(train_data, train_targets)

				
			predictions = gs_clf.predict(prediction_data)
			
			#predicted probability of being in either class
			predicted_prob = gs_clf.predict_proba(prediction_data)

			accuracy = accuracy_score(test_targets,predictions,normalize=True)
			
			accuracy_values.append(accuracy)

			#store individual estimates		
			scores = predicted_prob
							
			individual_data = pd.DataFrame({
			"obs_ids":obs_ids
			})
			
			individual_data.index = range(len(individual_data))
			# for i in range(0,number_of_parties):
			for i in range(0,number_of_parties):
				
				try:

					individual_data[gs_clf.classes_[i]] = scores[:,i]

				except:

					individual_data[party_abbreviations[i]] = [0]*len(scores[:,i-1])

			if ref == 0:
				individual_data.to_csv((output_dir+implementation+"_"+"nb_individual_estimates_"+str(session_ref)+".csv"), sep=',')
			else:
				individual_data.to_csv((output_dir+implementation+"_"+"nb_individual_estimates_"+str(session_ref)+".csv"), sep=',', mode = 'a', header=False)

		best_alphas = [best_alpha]*len(accuracy_values)
		session_refs_save = [str(session_ref)]*len(accuracy_values)
		session_indicators_save = [str(session_indicator)]*len(accuracy_values)
		
		print("Session_indicator = "+str(session_indicator))
		print("Implementation = "+str(implementation))
		
		if data_to_be_used == 1:
			print("Procedural language removed (=1)")
		else:
			print("Includes procedural language (=2)")

		scores = accuracy_values
		entries = zip(session_indicators_save,session_refs_save,scores,best_alphas)

		for (session_indicator_save,session_ref_save,score,best_alpha) in entries:	
			c.writerow([session_indicator_save,session_ref_save,score,best_alpha])

	else:
		pass
