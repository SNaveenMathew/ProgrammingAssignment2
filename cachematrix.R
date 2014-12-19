## The objective of these functions is to facilitate caching of inverse
## of a matrix if it is already calculated. Else the inverse is computed
## and returned. It is then cached for future use

## makeCacheMatrix creates a "special" matrix that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() return(x)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes inverse of "special" matrix returned by
## makeCacheMatrix function. If inverse already exists, this function
## will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    print("Getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}
