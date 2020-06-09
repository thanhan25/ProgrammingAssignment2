## Write a pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}

# Trying out
A <- cbind(c(1, 3, 3), c(4, 6, 6), c(7, 8, 9))
x <- makeCacheMatrix(A)

x$get()

cacheSolve(x)

x$getInverse()

# Now if we run cacheSolve(x) again the function will get the result from cache
cacheSolve(x)
