## This two functions are used to create a special object that stores a matrix
## and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    In <- NULL
    set <- function(y) {
        x <<- y
        In <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) In <<- solve
    getsolve <- function() In
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by the
## function above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    In <- x$getsolve()
    if(!is.null(In)) {
        message("getting cached data")
        return(In)
    }
    data <- x$get()
    In <- solve(data, ...)
    x$setsolve(In)
    In
}