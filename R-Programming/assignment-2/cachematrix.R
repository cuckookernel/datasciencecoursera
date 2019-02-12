## Put comments here that give an overall description of what your
## functions do
#  makeCacheMatrix : Create a CacheMatrix object (that has a slot for its inverse)
#  cacheSolve : return the inverse of the matrix (use cache if available)

## Write a short comment describing this function
# Makes an object that wraps a matrix together with it's cached inverse
# The object provides methods to set the value of the matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(mean) inv <<- mean
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# return the inverse of the matrix. Look it up in the cache first.
# If already available just return it or else compute it and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

