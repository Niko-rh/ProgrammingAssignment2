## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## Caching the Inverse of a Matrix:
## It is not always a good idea to compute Matrix inversion everytime again. 
## Instead it can be usefull to store or cache the inverse of that matrix. 
## There fore the below functions are used. 

## The following function is written to create an object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The following function calculates inverse of a matrix that was created by the above function. 
## In case it was already calculated the inverse should be taken from the cache.  


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


