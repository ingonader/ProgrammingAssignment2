## ############################################################################
## Coursera R Programming Assignment 02
## --------------------------------------------------------------------------
## cachematrix.R
## --------------------------------------------------------------------------
## matrix-like object that caches the inverse of the matrix, once its computed
## and retrieves this cache instead of computing it again
## ############################################################################


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## function makeCacheMatrix
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## creates a matrix-like object that can cache the inverse of this matrix
## input: 
##   x ... a numeric matrix (needs to be invertible!)
## output:
##   a list of functions: 
##   set ... to set the matrix
##   get ... to retrieve the matrix
##   setinverse ... to set the inverse of the matrix (in the cache)
##   getinverse ... to retrive the inverse of the matrix (from the cache)
makeCacheMatrix <- function(x = matrix()) {
	## initialize: set the cache to NULL
	inv <- NULL
	
	## set function: write input data to variable x, which stores the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## get function: retrive the matrix x
	get <- function() x
	
	## setinverse function: set the cache inv to the argument passed to the func.
	setinverse <- function(inverse) inv <<- inverse
	
	## getinverse function: retrive the cache inv
	getinverse <- function() inv
	
	## return object: list of 4 functions
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## function cacheSolve
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## computes (and caches) the inverse of a matrix. Assumes that the matrix
## always is invertible!
## input:
##   x ... a object returned by makeCacheMatrix
## output:
##   the inverse of this matrix, either computed or retrived from cache
cacheSolve <- function(x, ...) {
	## retrive the inverse from the cacheMatrix object (either NULL or the inverse)
	inv <- x$getinv()
	
	## if something is cached, then return this value
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	## otherwise, get the data, compute the inverse, set the cache
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	
	## return value: inverse of the matrix
	inv
}

## ============================================================================
## Main part: Test of functions
## ============================================================================

## create a random matrix
set.seed(42)
initmatrix <- matrix(runif(25), ncol=5, byrow=TRUE)
initmatrix

## compute the inverse of that matrix
initmatrix.inv <- solve(initmatrix)

## create a cacheMatrix object from the original matrix
mcm <- makeCacheMatrix(initmatrix)

## look at that object:
str(mcm)

## compute the inverse of the cacheMatrix object:
mcm.inv <- cacheSolve(mcm)
mcm$getinv()

## compute the inverse of the cacheMatrix object a second time:
mcm.inv <- cacheSolve(mcm)
mcm$getinv()

## compare the two results:
mcm$getinv() == initmatrix.inv



