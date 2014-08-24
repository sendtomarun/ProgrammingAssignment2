## These functions aids in caching the inverse of a matrix rather than computing it repeatedly


## This function takes a matrix input and creates a special list containing functions to
## set/get the value of the matrix & set/get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the special list returned by the above function and computes(caches)
## the inverse of the matrix with which the special list was created

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}
