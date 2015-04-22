# The first function, makeCacheMatrix creates a list containing 4 functions:
# a function to 'set()' the matrix; a function to 'get()' the matrix;
# a function 'setinv()' to set the inverse matrix;
# and a function 'getinv()' to get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

# The following function returns a matrix that is the inverse of the
# matrix 'x' created with the makeCacheMatrix() function.
# It first checks to see if the matrix has already been inverted.
# If so, it gets the inverse matrix from the cache and skips the computation. # Otherwise, it returns the inverse of the matrix using the 'solve()' and
# sets this inverted matrix in the cache via the 'setinv()' function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
