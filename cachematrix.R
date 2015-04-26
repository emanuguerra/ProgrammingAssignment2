## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# Returns a list of 2 getters and 2 setters functions to access to :
# - a matrix
# - its inverse
makeCacheMatrix <- function(x = matrix()) {

  # Initializes the cached variable
  cachedInvertedMatrix <- NULL
  
  # Define the functions.
  # Set a new value and erase the cached inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get simply returns the value
  get <- function() x
  # Sets the inverse
  setInverse <- function(invertedMatrix) cachedInvertedMatrix <<- invertedMatrix
  # Get the inverse
  getInverse <- function() cachedInvertedMatrix
  
  # Return the list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# Returns the inverse of a matrix.
# The inverse is either retrieved from cache or computed if not available.
cacheSolve <- function(x, ...) {
  ## Get the inverse of x
  invertedMatrix <- x$getInverse()
  
  # If the inverse has already been calculated the return it
  if(!is.null(invertedMatrix)) {
    message("getting cached result")
    return(invertedMatrix)
  }
  # Otherwise
  data <- x$get()
  # Calculate the inverse
  invertedMatrix <- solve(data, ...)
  # Cache the result
  x$setInverse(invertedMatrix)
  # And return it
  invertedMatrix
  
}
