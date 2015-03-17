## The pair of functions cache the inverse of a matrix that is invertible

## This function creates a matrix object that can cache its 
## inverse by getting and setting the values of the matrix and
## getting and setting the values of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) n <<- solve
    getinverse <- function() n
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by
## 'makeCacheMatrix'. If the inverse has already been calculated
## and the matrix has not changed, then 'cacheSolve' retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    n <- x$getinverse()
    if(!is.null(n)) {
        message("getting cached data")
        return(n)
    }
    data <- x$get()
    n <- solve(data, ...)
    x$setinverse(n)
    n
        ## Return a matrix that is the inverse of 'x'
}
