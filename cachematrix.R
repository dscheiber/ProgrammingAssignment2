## Using makeCacheMatrix() and cacheSolve(), users can create a matrix
## and store its inverse in a cache.

## This function creates a special matrix and subsequently holds its
## inversion which is provided by the cacheSolve function.

makeCacheMatrix <- function(x = numeric()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function provides the inverse to the matrix provided by makeCacheMatrix
## The newly created inverse is stored within makeCacheMatrix.
## If there is already an inverse stored within makeCasheMatrix, it will
## not create the inverse but instead call the already stored inverse.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
