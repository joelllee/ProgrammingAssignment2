## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than computing it repeatedly. These are a pair
## of functions that cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # set i to be NULL
  set <- function(y) { 
    x <<- y # assigns value y to object x in a different environment
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse # sets the value of i by assigning it to inverse
  getinverse <- function() i # returns the value of i
  list(set = set, get = get, # gets the list of the functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above
## If the inverse has already been calculated and the matrix has not been changed, then
## cacheSolve should return the inverse from the cache
## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse() # retrieves the value of the inverse, if it's available
  if(!is.null(i)) { 
    message("getting cached data")
    return(i) # returns the cached data if value of the inverse is not null
  }
  data <- x$get() # retrieves value of x and assigns it to data
  i <- solve(data, ...) # solve returns the inverse of the matrix
  x$setinverse(i) # set the value of the inverse
  i # prints out the inverse of the matrix
}
