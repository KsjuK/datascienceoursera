## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # Set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  #Get the matrix
  get <- function() {
    x
  }
  
  #Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  #Get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  #Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  
  #Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix
  data <- x$get()
  
  # The inverse 
  m <- solve(data) %*% data
  
  # Set the inverse
  x$setInverse(m)
  
  # Return the matrix
  m
}