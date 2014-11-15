## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # initialize inv with Null
  inv <- NULL
  # Set the value of a (new) matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the inverse
  setinverse <- function(inverse) inv <<- inverse
  # get the inverse
  getinverse <- function() inv
  # create a list that contains these 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # x is a matrix object created by makeCacheMatrix
  # First get the inverse of x
  inv <- x$getinverse()
  # Check is the inverse is Null; if it is not, get cached inverse 
  # (inverse has already been computed)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # only if the inverse is Null (not yet computed):
  # get the matrix
  data <- x$get()
  # compute the inverse
  inv <- solve(data, ...)
  # store the inverse
  x$setinverse(inv)
  inv
}
