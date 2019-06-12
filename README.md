#Automatic Program Synthesis of Matrix Operations

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
