## The objective of these functions is to facilitate caching of inverse
## of a matrix if it is already calculated. Else the inverse is computed
## and returned. It is then cached for future use

## makeCacheMatrix creates a matrix that can cache the inverse. It creates
## a list containing function to
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse
## * get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() return(x)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    print("Getting cahced data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}
