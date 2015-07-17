## Programming Assignment2
## Caching the Inverse of a Matrix

## Cache the inverse of the matrix so that when we need it again, 
## it can be looked up in the cache rather than recomputed


## makeCacheMatrix creates a special "matrix" object, 
## which is really a list containing functions
## (set, get, setInverse, getInverse)

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse) {
        cached_inverse <<- inverse
    }
    getInverse <- function() cached_inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of
## the special "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    ## if the inverse has already been calculated
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## otherwise
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
