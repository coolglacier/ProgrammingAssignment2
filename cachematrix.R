## These helper functions speed up repeated computations of
## matrix inverses by caching the computed inverse.
## Example usage:
##     mat <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
##     cmat <- makeCacheMatrix(mat)
##     inv <- cacheSolve(cmat)

## Matrix wrapper with get/set functions for the underlying
## matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse of the matrix, using the cached inverse if
## available, otherwise computing and then caching the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
