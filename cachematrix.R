## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse

## The first function, makeCacheMatrix creates and returns a list which contains four functions:
## 1) First function to set the value of the matrix. It also sets its inverse to NULL
## 2) Second function to get the value of the matrix
## 3) Third function to set the value of its inverse
## 4) Fourth function to get the value of the inverse

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
	     setinverse = setinverse, getinverse= getinverse)   ## creats a returns a list

}


## The below function calculates the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated (present in the cache).
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and then sets the value of the inverse in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {

	inv <- x$getinverse()	## Getting inverse from cache
	
	if(!is.null(inv)) {
		message("getting cached data")
                return(inv)			
	}
	
	data <- x$getmatrix()
	inv <- solve(data,...)  ## Getting the inverse of the matrix into inv
	x$setinverse(inv)       ## Saving inverse in cache
	
	inv

}
