## Functions provide tools for calculating an inverse of a given matrix
## and caching it

## Accepts a matrix as input, returns a vector containing list of functions that
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## create an empty spaceholder
    set <- function(y) { ## store given matrix in a different environment
        x <<- y
        i <<- NULL
    }
    get <- function() x ## return given matrix 
    set_inverse <- function(inverse) i <<- inverse ## store an inverse in a different environment
    get_inverse <- function() i ## return an inverse
    list(set = set, get = get, ## return a list announcing available functions
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Function accepts matrix as input, attempts to get the inverse of it from cache
## If it isn't cached, calculates the inverse, stores it in cache and returns it.

cacheSolve <- function(x, ...) {
    i <- x$get_inverse() # try to get the inverse from cache
    if(!is.null(i)) { # if successful 
        message("getting an inverse from cache")
        return(i) # return it and exit
    }
    data <- x$get() ## use matrix provided in input
    i <- solve(data, ...) ## inverse the matrix
    x$set_inverse(i) ## store inverse in the cache
    i ## Return a matrix that is the inverse of 'x'
}
