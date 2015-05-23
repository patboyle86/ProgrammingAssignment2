## These functions calculate and store an inputted matrix and its inverse.

## makeCacheMatrix takes a matrix as input and returns a list of functions to
## overwrite the matrix (set), return the input (get), set the inverse of the
## matrix (setinv), and return the inverse (getinv). 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix unless it's already been
## calculated and stored, in which case it returns the stored value.

cacheSolve <- function(x, ...) {
  	## Return a matrix that is the inverse of 'x'
  	inv <- x$getinv()
  	if (!is.null(inv)) {
  		message("getting cached data")
  		return(inv)
  	}
  	data <- x$get()
  	inv <- solve(data)
  	x$setinv(inv)
  	inv
}
