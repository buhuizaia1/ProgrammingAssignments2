## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # If inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, compute inverse, cache it, and return it
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
