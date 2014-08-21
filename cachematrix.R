## ######################################################################
## cachematrix.R
## 
## Functions to solve the inverse of a matrix and cache the result
##
## Example usage:
##
## > cachemat <- makeCacheMatrix(matrix(1, 1, 1))
## > cacheSolve(cachemat)
##      [,1]
## [1,]    1
## > cacheSolve(cachemat)
## getting cached data
##      [,1]
## [1,]    1
## 
## ######################################################################


## Make a cacheable matrix. Get/set a matrix and cache its
## inverse. Setting the matrix will clear the cache.
## Returns a list object. 
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cache <<- inverse
        getinverse <- function() cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Compute the inverse of a matrix. Takes a cacheable matrix (see
## function makeCacheMatrix() above), and either returns a
## precomputed/cached value or compute the inverse, cache and return
## this.
cacheSolve <- function(x, ...) {
        cache <- x$getinverse()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setinverse(cache)
        cache
}



