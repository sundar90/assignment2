## This function returns a special "matrix" object in the 
## form of a list which can be used by the cachesolve function to
## calculate the nverse of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
							}
  
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set=set,get=get,
			setinv=setinv,getinv=getinv)
}


## This function calculates the inverse of an invertible matrix if
## it is already not calculated. If it is already calculated
## retreives the answer from the cache in order to save computation costs

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if (!is.null(inv)) {
				message("retreiving cached data")
				return(inv)
						   }
		data <- x$get()
		inv <- solve(data,...)
		x$setinv(inv)
		inv
}
