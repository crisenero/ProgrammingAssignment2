## Assignment 2: Lexical Scoping
## Functions to cache/retrieve the inverse of a matrix
## Demonstrate knowledge in lexical scoping of objects

## Getters and setters to enable caching
## Assumption: 'x' was pre-validated to be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## set 'i' to NULL whenever mackeCacheMatrix is instantiated
        i <- NULL
        
        ## allow assignment of new matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## allow to read matrix from outside of makeCacheMatrix
        get <- function() x
        
        ## cache passed inverse matrix
        setInverse <- function(inverse) i <<- inverse
        
        ## return cached inverse matrix
        getInverse <- function() i
        list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## Assumption:
## 'x' is an instance of makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## get cached inverse matrix
        i <- x$getInverse()
        
        ## if exists in cache, return cached data
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## if data doesn't exist in cache perform the following
        ## 1. get matrix
        ## 2. solve the inverse matrix
        ## 3. cache inverse matrix
        ## 4. return inverse matrix
        data <- x$get()
        i <- solve(data, ...)
        message("caching inverse matrix")
        x$setInverse(i)
        i
}
