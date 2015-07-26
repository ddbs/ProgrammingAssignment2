#R Programming Assignment 2

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
# of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we 
# will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# Write the following functions:
# * makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# * cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.


## Function that creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Create a special "matrix" object that can cache its inverse.
  m <- NULL #initialize the value of the matrix inverse as NULL
  set <- function(y) {
    # This function stores values in cache
    x <<- y
    m <<- NULL #change the value of the matrix inverse if changes ocurred
  }
  get <- function() x #get the value of the inverse
  setmatrixinverse <- function(solve) m <<- solve  #calculate the inverse using solve
  getmatrixinverse <- function() m  #get the inverse of the matrix
  list(set = set, get = get, setmatrixinverse = setmatrixinverse, getmatrixinverse = getmatrixinverse)  #passes the value of the function
}


## Function that returns the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #gets the inverse if it exists
  }
  #if the inverse does not exist, calculate
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinverse(m)
  m
}