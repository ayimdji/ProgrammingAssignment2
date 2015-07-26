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
    
    ## 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## 2. get the value of the matrix
    get <- function() x
    
    ## 3. set the value of the inverse
    setInverse <- function(m) inv <<- m
    
    ## 4. get the value of the inverse
    getInverse <- function() inv
    
    ## return the list of the preceeding functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the inverse of the matrix from the cache if it is already in
    cachedInverse <- x$getInverse()
    if (!is.null(cachedInverse)) {
        message("Getting the cached inverse")
        return(cachedInverse)
    }
    
    ## Here the inverse is not in the cache then
    ## get the matrix, compute the inverse and cache it
    cachedMatrix <- x$get()
    cachedInverse <- solve(cachedMatrix)
    x$setInverse(cachedInverse)
    cachedInverse
}