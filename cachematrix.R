# A pair of functions that create a matrix, and cache its inverse.

# Creates a special object that stores a matrix and its inverse.
# Setters and getters function are provided for data storage and retrieval.
makeCacheMatrix <- function(x = matrix()) {
    # return an error if input is not a matrix
    if(!is.matrix(x)) {
        stop("Input is not a matrix")
    }
    
    z <- NULL
    
    # matrix setter and getter functions
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    
    # inverse matrix setter and getter functions
    set.inverse <-function(inv) {
        z <<- inv
    }
    get.inverse <- function() z
    
    list(set = set, get = get, 
         set.inverse = set.inverse, get.inverse = get.inverse)
}

## Solves and caches the inverse of a special matrix object
cacheSolve <- function(x) {
    # return inverse matrix from cache if available
    z <- x$get.inverse()
    if(!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    
    # inverse matrix is not cached: solve, cache, and return it
    data <- x$get()
    z <- solve(data)
    x$set.inverse(z)
    z
}
