## Write a pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  ## Return a matrix that is the inverse of 'x'
  inverse
}

A <- cbind(c(1, 3, 3), c(4, 6, 6), c(7, 8, 9))
x <- makeCacheMatrix(A)
x$get()
cacheSolve(x)
