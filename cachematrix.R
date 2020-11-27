# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set the default inverse for x
  x_inv <- NULL
  if(ncol(x) != nrow(x)) {
    stop("Not a square matrix!")
  }
  set <- function(y) {
    x <<- y
    x_inv <<- matrix()
  }
  get <- function() x
  setinv <- function(matrixinv) x_inv <<- matrixinv
  getinv <- function() x_inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  # calculate the inverse of x
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}
