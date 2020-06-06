## Below are two functions that are used to create a special object that stores 
## a matrix and caches the inverse of the matrix.

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x) == nrow(x) && det(x) != 0) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <-function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
  else {
    return(message("The matrix is not invertible."))
  }
}

## The second function computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the 
## matrix is unchanged), then 'cachesolve' will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
      message("Getting cached data...")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}