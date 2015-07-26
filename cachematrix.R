#R Programming Assignment 2

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