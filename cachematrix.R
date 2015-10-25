## These functions aim to caching the inverse of a matrix rather than computing it repeatedly.

## The function makeCacheMatrix creates a special "matrix", which is a list of 4 functions, including:
## set: set the value of the matrix
## get: get the value of the matrix
## setsolve: set the value of the inverse matrix
## getsolve: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function cacheSolve returns a matrix that is the inverse of 'x' or the inverse matrix from 
## cache, if it was already calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
