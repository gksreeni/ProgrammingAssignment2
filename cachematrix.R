## Given below are a pair of functions that cache the inverse of a matrix.
## 

## The first function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## This function takes an argument x of type matrix and returns a list of four list items (or functions wrapped in a list)
##

makeCacheMatrix <- function(x = matrix()) {
	I<-NULL
	set <- function(y) {
		x <<- y
		I <<- NULL
	}
							## The function "set" sets the matrix 

	get <- function() x				## The function "get" returns the matrix 


	setInverse<-function(solve) I <<- solve		## This function sets the inverse matrix
	getInverse <- function() I			## This function returns the Inverse of matrix
	
	list(set = set, get = get,			
             setInverse = setInverse,
             getInverse = getInverse)
}


## The second function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

	I <- x$getInverse() 			##query the x matrix's cache 
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }					## if there is a cache, then returns the message "getting cached data" and the value in cache without computing I.
	
	data <- x$get()				## if there is no cache, then get the matrix in the variable data.
        I <- solve(data, ...)			## compute the inverse of the matrix in data
        x$setInverse(I)				## save the result to the x matrix's cache
        I					## return the result 

}


