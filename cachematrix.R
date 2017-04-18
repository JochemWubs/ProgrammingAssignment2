## Matrix inversion is a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to respectively create an object that
## can store a matrix and it's inverse, and a function that can set or retrieve
## the matrix from the aforementioned object.

## makeCacheMatrix creates a list containing four functions to
## $set: set the value of the matrix
## $get: get the value of the matrix
## $setinv: set the value of inverse of the matrix
## $getinv: get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve computes and returns the inverse of a matrix that has been created
## with the makeCacheMatrix function. If the inverse has already been calculated before, 
## the cached inverse is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}
