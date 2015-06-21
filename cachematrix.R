## Defines a cache matrix object, where the 
## inverse of the object is computed "faster" than with normal matrices

## Creates the cache matrix object

makeCacheMatrix <- function(x = matrix()) {
    ## Retruns an object (list actually) based on the matrix given as input
    ## x: Input matrix
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cache inverse of a matrix

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    inverse <- x$getinverse() 
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse  # Returns the inverse
}
