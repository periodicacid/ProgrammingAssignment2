## Functions to create a matrix object with a cacheable
## inverse and to compute the inverse if a cached value
## cannot be retrieved.

## Creates a matrix and returns a list with getter and
## setter functions for both the matrix and its inverse,
## which is cached after the first computation.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y){
        x <<- y
        i <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the matrix x from the cache
## if it has been previously computed, otherwise computes,
## returns and stores the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(!is.null(x$getInverse())){
        message("Returning cached matrix inverse...")
        return(x$getInverse())
    }
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    return(inv)
}
