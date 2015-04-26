## The function, makeCacheVector, creates a special "matrixr,"
## which is really a list containing a function to set the
## value of the matrix, get the value of the matrix, 
## set the value of the inverse matrix through the solve
## R command, and get the value of the inverse matrix through
## the solve R command

makeCacheVector <- function (x = matrix()) {
	m <- "There is not a value for the inverse matrix yet"
	set <- function (y) {
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setinverse <- function(solve) m <<- solve
	getinverse <- function () m
	list(set = set, get =get, setinverse = setinverse, getinverse = getinverse)
}


## The following function, cacheSolve, calculates the inverse
## matrix of the special "matrix" created with the above fun.
## It first checks to see if the inverse matrix has already
## been calculated.  If so, it gets the inverse matrix from the
## cache.  Otherwise, it calculates the inverse matrix of the
## data and sets the value of the inverse matrix in the cache
## with the setinverse function.

cacheSolve <- function (x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
}