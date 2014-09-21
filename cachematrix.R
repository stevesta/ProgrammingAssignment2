## The following file is forked from rdpeng/ProgrammingAssignment2
## repository on github.
## The code in the functions below is heavily based on makeVector
## and cachemean provided in the README.md.
## Only a very few slight modifications needed to be made to adapt
## the code to solve the inverse of a matrix, instead of calcula-
## ting the mean of a vector, and extensive comments are added.

## This function creates a special "matrix" object that can cache 
## its inverse.
## Creates a special "matrix", which is really a list containing
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the inverse
## 4. set the value of the inverse
makeCacheMatrix <- function(x = matrix()) {  
  # sets the i to NULL, so if one attempts to access
  # the inverse through getinverse before the inverse
  # has been calculated, null will be returned instead
  i <- NULL
  
  # re-sets the matrix represented by this object, overwriting
  # the original argument of makeCacheMatrix, and the inverse
  # i is set to NULL until cacheSolve is called again.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # returns the matrix represented by this object
  get <- function() x
  
  # this is called by cacheSolve and the result of 
  # solve(x) is passed as the argument of setinverse
  setinverse <- function(inverse) i <<- inverse
  
  # returns i, which is the inverse if it has been 
  # calculated, or NULL if ithas not yet been calculated
  getinverse <- function() i
  
  # returns a labeled list of set, get, setinverse, getinverse
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
  
  # gets i from CacheMatrix x, may 
  i <- x$getinverse()
  
  # if i is not null, meaning the solution has been calculated already
  # then pass a message that i is retrieved from cache, and return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # this part is only reached if i is null,
  # x$get() returns the actual matrix we need to invert,
  # then the inverse is calculated,
  # it is set in the CacheMatrix object with setinverse,
  # and finally it is returned
  cached_matrix <- x$get()
  i <- solve(cached_matrix)
  x$setinverse(i)
  i
}
