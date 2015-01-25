## Create a wrapper around `x' that's capable of caching its inverse, in order
## to save computation time if `x' is particularly large.
makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL
	set <- function(y) {
		x <<- y
		## We got a new value so we need to reset the cache.
		cachedInverse <<- NULL
	}

	get <- function() {
		x
	}

	setinv <- function(inverse) {
		cachedInverse <<- inverse
	}

	getinv <- function() {
		cachedInverse
	}

	list(get = get, set = set,
		getinv = getinv, setinv = setinv)
}


## This function returns the inverse of `x', using its cached value if available.
cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
        }

        matrix <- x$get()
        ## `solve' is used to compute the inverse of the matrix
        inverse <- solve(matrix, ...)

        ## We computed the inverse, now make sure we also cache it.
        x$setinv(inverse)
        inverse
}
