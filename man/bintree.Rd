<<<<<<< HEAD
\name{newbintree,push,pop,push.bintree,push_helper,push_firstNArow,pop.bintree,pop_helper,print.bintree}
\alias{newbintree}
\alias{push}
\alias{pop}
\alias{push.bintree}
\alias{push_helper}
\alias{push_firstNArow}
\alias{pop.bintree}
\alias{pop_helper}
=======
\name{newbintree,push,pop,helper,push.bintree,print.bintree}
\alias{newbintree}
\alias{push}
\alias{pop}
\alias{helper}
\alias{push.bintree}
>>>>>>> 0caae9178b02d7186db168d310f616ba1c9b3d4d
\alias{print.bintree}

\title{S3 Class Binary Tree Implementation}

\description{
<<<<<<< HEAD
	Binary tree class with push, pop, and print methods.
=======
	Binary Tree Class with print, push, pop methods. T a
>>>>>>> 0caae9178b02d7186db168d310f616ba1c9b3d4d
}

\usage{
	newbintree()
<<<<<<< HEAD
	push(tree,val)
	pop(tree)
	push.bintree(tree,val)
	push_helper(root,inVal,graph)
	push_firstNArow(graph)
	pop.bintree(tree)
	pop_helper(tree,row,parent)
=======
	push(obj,val)
	pop(obj,val)
	helper(root,inVal,graph)
	push.bintree(obj,val)
>>>>>>> 0caae9178b02d7186db168d310f616ba1c9b3d4d
	print.bintree(tree)
}

\arguments{
<<<<<<< HEAD
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
=======
	\item{obj}{A newbintree class object to be modified.}
	\item{val}{The primitive value to be pushed or popped.}
	\item{root}{The row of the node.}
	\item{inVal}{The primitive value to be pushed or popped.
		Same as \code{val} but only used in helper function.}
	\item{graph}{The newbintree class object.
		Same as \code{obj} but only used in helper function to check for existing children.}
	\item{tree}{The newbintree class object to be printed.}
}

\details{
	Implements a S3 class version of a binary tpiree in R with print,
	push and pop methods.
>>>>>>> 0caae9178b02d7186db168d310f616ba1c9b3d4d

	This implementation involves instantiating a new Environment with each
	new instance of a binary tree, along with the class attribute appended
	with the name of the structure.

<<<<<<< HEAD
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

=======
	In our implementation the binary tree is stored as a matrix, where each row
	is represented as a node with the value, row number of the its left child,
	and row number of its right child as its column values, respecti,tnvely.

	Note the comments above \code{helper} that explain what how the value, left
	child row, and right child row are accessed in the helper function.

	A note on NA values: The value of NA in the binary tree represents:
	1. if \code{val} = NA, there is no value in that row (this will only be the case
	when the tree is instantiated)
	2. if \code{left} = NA, then the node in that row has no left child
	3. if \code{right} = NA, then the node in that row has no right child

	Note that in \code{push.bintree}, when the tree is empty the first value
	pushed has its left and right children sent to NA. The function \code{helper}
	is always called as long as the tree is not empty, and performs a recursive
	search to find the correct spot to insert in the binary tree.
>>>>>>> 0caae9178b02d7186db168d310f616ba1c9b3d4d
}

\value{The following items are returned by the methods of our binary tree:
	
	\itemize{
<<<<<<< HEAD

		\item \code{newbintree}, the instantiated newbintree object

		\item \code{push_helper}, the modified matrix after the insertion

		\item \code{push_firstNArow}, the row in the matrix containing
		the first row of NA's

		\item \code{pop.bintree}, the popped value

		\item \code{pop_helper}, the popped value
=======
		
		\item \code{newbintree}, the instantiated newbintree object

		\item \code{helper}, the matrix in which our binary tree is stored

		\item \code{push.bintree}, the new version of the matrix in which our
		binary tree is stored

		\item \code{pop.bintree}, the smallest element that was removed
>>>>>>> 0caae9178b02d7186db168d310f616ba1c9b3d4d

	}
}

\examples{
<<<<<<< HEAD
	# Initialize and set up our queue
	queue <- newqueue()

	# attempt to pop empty queue, will result in warning
	pop(queue)

	# push values into the queue
	push(queue,1)
	push(queue,2)
	push(queue,3)
	push(queue,4)

	# pop the element at front of the queue, returns 1
	pop(queue)

	# push more values into the queue
	push(queue,5)
	push(queue,6)
	push(queue,7)

	# print out the queue
	print(queue)
}

\author{
=======
	
	# Initialize and set up our binary tree
	tree <- newbintree()wp

	# attempt to pop empty tree, will result in warning
	pop(tree)

	# push values into the tree
	push(tree,7)
	push(tree,5)
	push(tree,6)
	push(tree,9)

	# pop the smallest value, which returns 5 and removes it from the tree
	pop(tree)

	# push more values into the tree
	push(tree,4)
	push(tree,1)
	push(tree,30)
	push(tree,8)
	push(tree,10)

	# print out the binary tree
	print(tree)
}

\authors{
>>>>>>> 0caae9178b02d7186db168d310f616ba1c9b3d4d
	Jeffrey Tai
	John Nguyen
	Joanne Wang
	Eric Du
}