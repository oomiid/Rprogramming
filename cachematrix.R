## Put comments here that give an overall description of what your
## functions do
############################################
# The following set of functions suppose to cache and calculate the inverse of a given matrix:

## Write a short comment describing this function
#############################################
# This function is to cache the inverse:

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return (list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function
#############################################
# The matrix created by makeCachematrix is taken by this function as an input and the outpul will be the matrix's inverse
# In other words, the inverse is already there, but this function is retrieving it.


cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv (inverse)
    return (inverse)
}
