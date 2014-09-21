## The following file is forked from rdpeng/ProgrammingAssignment2
## repository on github.
## The code in the functions below is heavily based on makeVector
## and cachemean provided in the README.md.
## Only a very few slight modifications needed to be made to adapt
## the code to solve the inverse of a matrix, instead of calcula-
## ting the mean of a vector.

## This function creates a special "matrix" object that can cache 
## its inverse.
## Creates a special "matrix", which is really a list containing
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the inverse
## 4. set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # This function is nearly the same as makeVector, only:
  # * m has been changed to i 
  # * setmean has been changed to setinverse
  # * getmean has been changed to getinverse
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates the inverse of the special CacheMatrix created with
## the above function
## If the inverse has already been calculated, and the matrix has
## not changed, then the cachesolve retrieves the inverse from cache

cacheSolve <- function(x, ...) {
  # This function is nearly the same as cachemean, only
  # * m has been changed to i
  # * setmean has been changed to setinverse
  # * getmean has been changed to getinverse
  # * data has been changed to cached_matrix, a little more explicit
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  cached_matrix <- x$get()
  i <- solve(cached_matrix)
  x$setinverse(i)
  i
}
