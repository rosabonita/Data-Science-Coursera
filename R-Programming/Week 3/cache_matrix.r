## These functions first create a cached matrix and then solve said matrix for
## its inverse. It does this by not recalculating the inverse if 
## its already calculated.

## Function to create a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function computes the inverse of the special 'matrix' returned by makeCacheMatrix.
## If the inverse has already been calculate and the matrix has not changed then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
