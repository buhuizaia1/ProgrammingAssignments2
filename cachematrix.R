


## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # sets the variables x and y
    x <<- y
    m <<- NULL
  }
  get <- function() x #gets x
  setInverse <- function(inverse) m <<- inverse #sets the inverse to m
  getInverse <- function() m #gets m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#This function computes the inverse of the matrix, 
#if it has not been computed before or 
#retrieves the answer that has been already calculated
cacheSolve <- function(x, ...) {
  m <- x$getInverse()#get inverse or NULL if it has not been computed before
  
  if(!is.null(m)) {# if the inverse has been computed before, return that inverse
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)#compute inverse if it has not been computed before
  x$setInverse(m)
  m
}


