## Put comments here that give an overall description of what your
## functions do

## Created matrix for inverson

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL  # Initialize the inverse as NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse when matrix changes
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getInverse <- function() inv
    
    # Return a list of the above functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Created function for cachesolve

cacheSolve <- function(x, ...) {
        
   inv <- x$getInverse()
    
    # If inverse is already cached, return it
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    
    # Otherwise, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)  # Cache the inverse
    inv

        ## Return a matrix that is the inverse of 'x'
}
