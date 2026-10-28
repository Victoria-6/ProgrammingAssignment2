## Overall Description of Assignment:
## These functions optimise matrix inversion by caching the calculated inverse. 
## The general aim is to avoid repetition of calculation if the underlying
## matrix remains unchanged.

## The makeCacheMatrix function provides an object that caches its inverse.
## It gives four functions that set and get the matrix value
## and then set and get the inverse value.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL ## This initialiZes the cache inverse property to NULL
  set <- function(y){ ## This sets the matrix value
  x <<- y
  j <<- NULL ## Resets cache
  }
  get <- function()x # returns the stored matrix
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j ## retrieves stored inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix obtained from 
## makeCacheMatrix above. It retrieves the inverse from cache, check if the
## inverse is cached. Then it calculates the matrix inverse using solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat, ...)
  x$setInverse(j)
  j
}
