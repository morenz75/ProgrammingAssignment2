## Put comments here that give an overall description of what your
## functions do

# This function get a matrix (supposed to be reversible) and return it's reversed ones
# The reversed matrix is calculated just one time, so if we call cacheSolve it take 
# the cached version of the reversed matric if exists, othewise it calculate and cache it

## Write a short comment describing this function
# Take a matrix as input and create a list of function to get and set the original matrix
# and get / set the reversed version

makeCacheMatrix <- function(x = matrix()) {
        # Take a matrix in input 
        # First of all reset reversed Matrix m
        m <- NULL
        # Create function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setReverse <- function(solve) m <<- solve
        getReverse <- function() m
        
        # Return a list of function
        list(set = set, get = get,
             setReverse = setReverse,
             getReverse = getReverse)
}


## Write a short comment describing this function
# Take a makeCaccheMatrix object as input, and try to get a reversed matrix from it
# If there's a cache version of it, it juts return it without rereun solve function
# Otherwise, it run solve function, cache and return the reversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Get Revered matrix
        m <- x$getReverse()
        # If m is not null it means that we already know reversed matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Get original Matrix
        data <- x$get()
        # calculate the reversed one
        m <- solve(data, ...)
        # Save reversed Matrix
        x$setReverse(m)
        # return it
        m
}
