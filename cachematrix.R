## These functions are utilities to speed up the calculation of a matrix inverse
## by caching the result and returning the cached value upon subsequent requests

## makeCacheMatrix creates a simple matrix data structure whith helper methods
## $set - set the original matrix
## $get - get the original matrix
## $setinverse - set the inverse of the matrix
## $getinverse - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(newInverse) inverse <<- newInverse
    
    getinverse <- function() inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve finds the inverse of a matrix x (passed in through the helper object above)
## and caches that value. It will return the cached value upon subsequent calls.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    matrixData <- x$get()
    inverse <- solve(matrixData)
    x$setinverse(inverse)
    inverse
}
