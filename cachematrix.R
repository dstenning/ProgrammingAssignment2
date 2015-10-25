## The following pair of functions are designed to cache the inverse of a matrix.  This avoids a potentially costly computation.
## Note: use of these functions assumes that the supplied matrix is invertible.


# The following function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.  
# The special "matrix object is a list containing a function to: 
#
#		(1) set the values of the matrix, 
#		(2) get the values of the matrix, 
#		(3) set the value of the inverse of the matrix, and 
#		(4) get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse	) 
}


# The following function calculates the inverse of the special "matrix" created with 'makeCacheMatrix'.  
# If the inverse has already been calculated then the function gets the inverse from the cache and avoids the computation.  
# Otherwise, the inverse of the matrix is calculated and the set in cache via the 'setInverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
        			message("getting cached data")
        			return(inv)
        }
        data_matrix <- x$get()
        inv  <- solve(data_matrix)
        x$setInverse(inv)
        inv
}
