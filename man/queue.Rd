\name{newqueue,push,pop,push.queue,pop.queue,print.queue}
\alias{newqueue}
\alias{push}
\alias{pop}
\alias{push.queue}
\alias{pop.queue}
\alias{print.queue}

\title{S3 Class Queue Implementation}

\description{
	Queue class with push, pop, and print methods.
}

\usage{
	newqueue()
	push(obj,val)
	pop(obj)
	push.queue(obj,val)
	pop.queue(obj)
	print.queue(obj)
}

\arguments{
	\item{obj}{A queue class object to be modified or printed.}
	\item{val}{The primitive value to be pushed or popped.}
}

\details{
	Implements a S3 class version of a standard queue in R with print,
	push and pop methods.

	This implementation involves instantiating a new Environment with each
	new instance of a queue, along with the class attribute appended
	with the name of the structure.

	In our implementation the queue is stored as a vector, where its elements
	are the values that are enqueued and dequeued.

	A note on NA values: The value of NA in \code{newqueue} and \code{push.queue}
	represents missing or impossible values that can not be inserted into the vector.

	Note in \code{newqueue}, we are appending NA as the first element in the queue.
	We keep this element as a flag to to signify when the queue is empty, or when
	length of queue = 1. Also note in \code{pop.queue}, the element 2 in the vector
	\code{obj} is the front of the queue, not element 1 (which is NA).

}

\value{The following items are returned by the methods of our queue:
	
	\itemize{

		\item \code{newqueue}, the instantiated newqueue object

		\item \code{push.queue}, the new version of the vector in which
		our queue is stored

		\item \code{pop.queue}, the element at the front of the queue that
		was removed

	}
}

\examples{
	# Initialize and set up our queue
	queue <- newqueue()

	# attempt to pop empty queue, will result in warning
	pop(queue)

	# push values into the queue
	push(queue,1)
	push(queue,2)

	# pop the element at front of the queue, returns 1
	pop(queue)

	# push more values into the queue
	push(queue,5)
	push(queue,"hello")

	# print out the queue
	print(queue)
}

\author{
	Jeffrey Tai
	John Nguyen
	Joanne Wang
	Eric Du
}