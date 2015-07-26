## The following functions aim to cache the computation of the 
## inverse of a matrix


## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse. The special matrix is really a list of functions to : 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(m) inv <<- m
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" created 
## with makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    cachedInverse <- x$getInverse()
    if (!is.null(cachedInverse)) {
        message("Getting the cached inverse")
        return(cachedInverse)
    }
    
    cachedMatrix <- x$get()
    cachedInverse <- solve(cachedMatrix)
    x$setInverse(cachedInverse)
    cachedInverse
}