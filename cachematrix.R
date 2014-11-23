## Assignment: Caching the Inverse of a Matrix
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    setM <- function(y) {
        x <<- y
        m <<- NULL
    }
    getM <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(setM = setM, getM = getM,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting inverse")
        return(m)
    }
    data <- x$getM()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
