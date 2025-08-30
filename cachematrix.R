## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: create a matrix that can store itself and a cached copy of its inverse once its computed
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                        # cache for inverse 
  
  set <- function(y) {               # replace matrix and reset cache
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x                # return current matrix
  
  setinverse <- function(inverse)    # store computed inverse
    inv <<- inverse
  
  getinverse <- function() inv       # return cached inverse (or NULL)
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
# cacheSolve: returns the cached inverse or computes & caches it
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()              # try cache first
  if (!is.null(inv)) {
    message("getting the cached inverse")
    return(inv)
  }
  mat <- x$get()                     # fetch matrix
  inv <- solve(mat, ...)             # compute inverse (assumes invertible)
  x$setinverse(inv)                  # cache result
  inv                                 # return inverse
}

