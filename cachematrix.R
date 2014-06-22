## Put comments here that give an overall description of what your
## functions do

##The two functions create a special object, stores a matrix value and returns its corresponding inverse matrix.

## makeCacheMatrix caches the value of the inverse matrix.
## The steps are - 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

## Initialized the variable which would store the inverse matrix values
  m <- x$getInverse()
  
## checks if the inverse matrix value is cached or not
## if YES - then the value is return and the function cacheSolve exits

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
## if NO - then the matrix is solved for its inverse values and return
## then the value of the inverse matrix is stored for future access
## the function cacheSolve returns the value of m and exits.

  m <- x$get()
  m <- solve(m, ...)
  x$setInverse(m)
  m
}

