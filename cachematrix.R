## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when a new matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and stored in cache, it retrieves it instead of recomputing.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()  # Retrieve the matrix
  inv <- solve(mat, ...)  # Compute the inverse using `solve()`
  x$setInverse(inv)  # Cache the inverse
  
  return(inv)  # Return the inverse
}
