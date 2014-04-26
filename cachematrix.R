## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL

	setmatrix <- function(m){
		x <<- m
		inverse <<-NULL
	}
	getmatrix <- function() x
	setinverse <- function(y) inverse <<- y
	getinverse <- function() inverse

	list(setmatrix = setmatrix, getmatrix = getmatrix,
		setinverse = setinverse, getinverse= getinverse)



}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		    message("getting cached data")
                return(inv)
	}
	data <- x$getmatrix()
	inv <- solve(data)

	x$setinverse(inv)
	inv



}
