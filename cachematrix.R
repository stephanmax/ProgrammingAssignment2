## Programming assignment 2 by Stephan Max
## for Coursera's "R Programming"
## Caching expensive computations by example of the matrix inverse
## by Stephan Max

## Factory function that creates a special matrix comprising get, set,
## getInverse, and setInverse methods

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() {
    x
  }
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getInverse <- function() {
    inv
  }
  setInverse <- function(newInv) {
    inv <<- newInv
  }
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## Function that returns the cached inverse of our special matrix
## (if present and the matrix has not changed) or otherwise recalculates
## it from fresh data

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message('using cached data')
  }
  else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
  }
  inv
}
