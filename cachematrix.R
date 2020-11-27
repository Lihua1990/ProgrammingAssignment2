makeCacheMatrix <- function(x = matrix()) {
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



cacheSolve <- function(x, ...) {
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}
