## Put comments here that give an overall description of what your
## functions do
## These functions create a special matrix object that can cache its inverse.
## makeCacheMatrix: creates the special matrix object with functions to set/get the matrix and its inverse.
## cacheSolve: computes the inverse of the matrix, or retrieves it from the cache if already calculated.

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix object that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
          x <<- y
          inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse.gaussian
        getInverse <- function() inv
        
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function
## Computes the inverse of the special "matrix" object created by makeCacheMatrix. If the inverse
## has already been calculated and the matrix has not changed, it retrieves the cached inverse to 
## avoid recomputation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
