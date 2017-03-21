\name{newbintree,push,pop,push.bintree,push_helper,push_firstNArow,pop.bintree,pop_helper,print.bintree}
\alias{newbintree}
\alias{push}
\alias{pop}
\alias{push.bintree}
\alias{push_helper}
\alias{push_firstNArow}
\alias{pop.bintree}
\alias{pop_helper}
\alias{print.bintree}

\title{S3 Class Binary Tree Implementation}

\description{
	Binary tree class with push, pop, and print methods.
}

\usage{
	newbintree()
	push(tree,val)
	pop(tree)
	push.bintree(tree,val)
	push_helper(root,inVal,graph)
	push_firstNArow(graph)
	pop.bintree(tree)
	pop_helper(tree,row,parent)
	print.bintree(tree)
}

\arguments{
	\item{tree}{A binary tree class object to be modified or printed.}
	\item{val}{The primitive value to be pushed or popped.}
	\item{root}{The row in the matrix to begin the tree traversal.}
	\item{inVal}{Numeric or character vectors.}
	\item{graph}{The matrix to be modified.}
	\item{row}{The row to searching for the smallest element.}
	\item{parent}{The parent row.}
}

\details{
	Implements a S3 class version of a standard binary tree in R
	with print, push and pop methods.

	This implementation involves instantiating a new Environment with each
	new instance of a binary tree, along with the class attribute appended
	with the name of the structure.

	In our implementation the binary tree is stored as a matrix,
	where its elements are the values that are pushed and popped.

	A note on NA values: The value of NA in \code{newbintree},
	\code{push.bintree}, and \code{push_helper} represents when the
	matrix is empty and where to begin searching for push and pop.

	Note in \code{newbintree}, we are appending NA as the first row in
	the matrix. We keep this row as a flag to to signify when the binary
	tree is empty, or when size of the matrix = 1. Also note in
	\code{pop.bintree}, the second row in the matrix \code{tree} is the
	root of the binary tree, not row 1 (which contains all NA values).

}

\value{The following items are returned by the methods of our binary tree:
	
	\itemize{

		\item \code{newbintree}, the instantiated newbintree object

		\item \code{push_helper}, the modified matrix after the insertion

		\item \code{push_firstNArow}, the row in the matrix containing
		the first row of NA's

		\item \code{pop.bintree}, the popped value

		\item \code{pop_helper}, the popped value
		
		\item \code{newbintree}, the instantiated newbintree object

		\item \code{helper}, the matrix in which our binary tree is stored

		\item \code{push.bintree}, the new version of the matrix in which our
		binary tree is stored

		\item \code{pop.bintree}, the smallest element that was removed

	}
}

\examples{
	# Initialize and set up our binary tree
	tree <- newbintree()

	# attempt to pop empty tree, will result in warning
	pop(tree)

	# push values into the tree
	push(tree,7)
	push(tree,5)

	# pop the smallest value, which returns 5 and removes it from the tree
	pop(tree)

	# push more values into the tree
	push(tree,1)
	push(tree,30)
	push(tree,"hello")

	# print out the binary tree
	print(tree)
}

\author{
	Jeffrey Tai
	John Nguyen
	Joanne Wang
	Eric Du
}