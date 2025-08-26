# Takes a matrix (a grid of numbers) as input.
# Creates a little toolbox with four tools:
#         
# set           :  Updates the matrix with a new one and clears the old inverse.
# get           : Shows you the matrix.
# setinverse    : Saves the inverse in the memory box.
# getinverse    : Lets you peek at the saved inverse.
# 
# 
# Uses a list to hold these tools so you can call them anytime.

makeCacheMatrix <- function(x = matrix()) {  # x is your matrix, starts empty
        inv <- NULL  # This is where we’ll save the inverse, starts as nothing
        set <- function(y) {  # Change the matrix to a new one (y)
                x <<- y    # Save the new matrix in the magic box
                inv <<- NULL  # Clear the old inverse
        }
        get <- function() x  # Show me the matrix
        setinverse <- function(inverse) inv <<- inverse  # Save the inverse
        getinverse <- function() inv  # Show me the saved inverse
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)  # Put all tools in a box
}


# Looks at the saved inverse in the magic box.
# If it’s there (not NULL), says “Hey, I found it!” and gives it to you.
# If it’s not there, gets the matrix, calculates the inverse using a math trick (solve), saves it, and hands it over.

cacheSolve <- function(x, ...) {  # x is the magic box from makeCacheMatrix
        inv <- x$getinverse()  # Peek at the saved inverse
        if(!is.null(inv)) {    # If there’s something saved
                message("Getting cached data!")  # Say we found it
                return(inv)        # Give the saved inverse
        }
        data <- x$get()        # Get the matrix since we need to calculate
        inv <- solve(data, ...) # Use a math trick to find the inverse
        x$setinverse(inv)      # Save the inverse in the box
        inv                    # Give the inverse
}

# Make the magic box with a matrix
my_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# First time - calculates inverse
cacheSolve(my_matrix)
# Output might be: [-2, 1; 1.5, -0.5]

# Second time - uses cached version
cacheSolve(my_matrix)
# Output: "Getting cached data!" then [-2, 1; 1.5, -0.5]
