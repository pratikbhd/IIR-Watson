# IIR-Watson
IBM Watson Implementation as part of the Information Retrieval and Web Search Class (CSC 583)

This model allows a user to perform two functions:

1. Ask a query. 
	The result of this will be its score and its rank
2. Evaluate the dataset
	The result of this will be the P@1 precision score of the training data based on the evaluation question set

The following scoring functions are provided for the user to choose from:

1. Default scoring i.e. BM25
2. TF-IDF scoring
3. Boolean similarity scoring

Two models are available for the user to choose from:

1. A naive model
2. An improved model

## The Naive Model

In this model the following actions have been performed:

1. Limited pre-processing of the training data in terms of input conten. The following transformations have been applied:
	a. Removing all forms of punctuation marks.
	b. Lowercasing all text
	c. Discarding all text that start with:
		-
		-
		-
2. Indexing of the documents on two fields "TITLE" and "CONTENT".
3. No lemmatization performed.
4. Provision of only one (Default) scoring function.
5. Search on the basis of query only, neglecting the category.
6. No further NLP/ML approaches applied.

Accuracy: 

## The Improved Model

In this model, the naive approach has been augmented with further processing and text retrieval techniques to increase the matching score. The following features have been applied:

1. Further pre-processing of the text. More stringent removel of data that do not contribute to the title heading or context of the article, such as:
	.
	.
	.

2. Indexing the documents on three fields "TITLE", "CATEGORY" and "CONTENT"
3. Lemmatization performed on the "CATEGORY" and "CONTENT" data.
4. Provision of three scoring functions to choose from.
5. Search on the basis of combined query and content as a single query.
6. "_____________" form of NLP/ML approach used to further boost matching performance.

Accuracy:
