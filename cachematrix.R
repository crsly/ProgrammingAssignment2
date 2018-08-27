## Functions to create and work with inverse-caching matrices.
## Potentially useful when you need to repeatedly invert a large matrix.

## Given a matrix, returns a list than can be used with cacheSolve() to
## compute the inverse, caching the result.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Given an inverse-caching matrix produced by makeCacheMatrix, returns the
## inverse of the matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
