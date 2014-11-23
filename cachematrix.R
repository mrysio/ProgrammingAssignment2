## My functions will cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(invmatrix) inv <<- invmatrix
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above or it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Try to return an inverse matrix from cache 
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      ## Compute and return inverse of matrix 
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
