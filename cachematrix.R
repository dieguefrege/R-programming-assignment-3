##FUNCTION 1: Function that creates a matrix object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  # Make a function to set the matrix
  setMatrix <- function(newMat) {
    mat <<- newMat
    inverse <<- NULL  # Invalidate the cached inverse when the matrix is changed
  }
  
  # Make a function to get the matrix
  getMatrix <- function() {
    mat
  }
  
  # Make a function to set the inverse
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  
  # Make a function to get the inverse
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions to interact with the matrix object
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

# FUNCTION 2: Function to compute the inverse of the special matrix
cacheSolve <- function(cacheMatrix) {
  # Check if the cached inverse is available
  if (!is.null(cacheMatrix$getInverse())) {
    message("Getting cached data.")
    return(cacheMatrix$getInverse())
  }
  
  # If not cached, compute the inverse using solve function
  mat <- cacheMatrix$getMatrix()
  inverse <- solve(mat)
  
  # Cache the computed inverse using `<<-`
  cacheMatrix$setInverse(inverse)
  
  inverse
}
