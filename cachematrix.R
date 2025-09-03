## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL  # Initialize the inverse to NULL
  
  # Set the matrix and clear any previously cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getinverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  
  # If inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Use solve to compute the inverse
  x$setinverse(inv)       # Cache the inverse
  inv
}
