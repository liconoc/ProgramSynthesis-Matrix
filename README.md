#AUTOMAT[R]IX: Automatic Program Synthesis of Matrix Operations

## Program Synthesis with R

Data is usually used in statistics in a matrix form. A matrix is a rectangular
array of numbers arranged into columns and rows (much like a spreadsheet).
Matrix  algebra  is  used  in  statistics  to  express  collections  of  data  and  many
operations can be applied to those matrix in order to extract a variety of useful
information about data. However, when matrices are too big the need of tools or
languages that help the statistician to analyse the data is a key factor. But both
tools and languages require from the user some knowledge on how to use them
or program them, converting this step in a time-consuming bottle neck for many
people.

In this project, we present a system able to induce programs in R language given a
set of inputs: a data matrix and a partial matrix filled by the user representing
the solution of the operation to apply. The system is able to find the operation
or set of operations that can be apply to the input to obtain the complete output
using the characteristics of the input and output matrices to reduce the search
and find the right solution. We assume the user has knowledge in statistics or is
able to use some existing tools for statistics, but he/she has minimal knowledge
of programming languages. This system writes in R the code the user needs to
analyse the data matrix in such a way the output is automatically filled and the
code exported ready to use in R.


## Benchmark

To encourage future research all the data is published in the data folder.


## Tutorial (how to use)

To use the code of AUTOMAT[R]IX just follow the following steps:

1. Load code\algorithm.R and its functions (the file data\prior_probs.csv should be available).

2. Create or load a matrix A, for instance:

	A <- matrix(c(NA,1,NA,1,NA,2,NA,NA,NA,1,2,3), ncol=4)
	
3. Create or load a matrix B (the output matrix) with some empty values as "NA", for instance:

	B=matrix(rep(c(NA,NA,NA,NA),3), byrow=TRUE, nrow=3)
	
	B[1,1]=TRUE
	
	B[1,2]=FALSE
	
4. Call the function automatrix(text, A, B, d_max, s_max, functions) where the parameters are as follows:

	text: The string provided by the user as tip, for instance: "Get positions for NA"
	
	A: Matrix A
	
	B: Matrix B
	
	d_max: Maximum number of functions allowed in the solution
	
	s_max: Maximum number of solutions to find
	
	functions: A csv file including the functions to use as Background Knowledge. The format of the file can be done by following the one of data\functions.csv
	
	

