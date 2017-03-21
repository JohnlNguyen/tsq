\name{newstack,push,pop,push.stack,pop.stack,print.stack}
\alias{newstack}
\alias{push}
\alias{pop}
\alias{push.stack}
\alias{pop.stack}
\alias{print.stack}

\title{S3 Class Stack Implementation}

\description{
	Stack class with push, pop, and print methods.
}

\usage{
	newstack()
	push(obj,val)
	pop(obj)
	push.stack(obj,val)
	pop.stack(obj)
	print.stack(obj)
}

\arguments{
	\item{obj}{A stack class object to be modified or printed.}
	\item{val}{The primitive value to be pushed or popped.}
}

\details{
	Implements a S3 class version of a standard stack in R with print,
	push and pop methods.

	This implementation involves instantiating a new Environment with each
	new instance of a stack, along with the class attribute appended
	with the name of the structure.

	In our implementation the stack is stored as a vector, where its elements
	are the values that are pushed and popped.

	A note on NA values: The value of NA in \code{newstack} and \code{push.stack}
	represents missing or impossible values that can not be inserted into the vector.

	Note in \code{newstack}, we are appending NA as the first element in the stack.
	We keep this element as a flag to to signify when the stack is empty, or when
	length of stack = 1. Also note in \code{pop.stack}, the last element of the
	vector is the top of the stack.

}

\value{The following items are returned by the methods of our stack:
	
	\itemize{

		\item \code{newstack}, the instantiated newstack object

		\item \code{push.stack}, the new version of the vector in which
		our stack is stored

		\item \code{pop.stack}, the element at the top of the stack that
		was removed

	}
}

\examples{
	# Initialize and set up our stack
	stack <- newstack()

	# attempt to pop empty stack, will result in warning
	pop(stack)

	# push values into the stack
	push(stack,1)
	push(stack,2)

	# pop the element at the top of the stack, returns 4
	pop(stack)

	# push more values onto the top of the stack
	push(stack,5)
	push(stack,"hello")

	# print out the stack
	print(stack)
}

\author{
	Jeffrey Tai
	John Nguyen
	Joanne Wang
	Eric Du
}