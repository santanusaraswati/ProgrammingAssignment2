## Put comments here that give an overall description of what your
## functions do
## The two methods work in tandem
## makeCacheMatrix creates a cachable version of invertible matrix 
## cacheSolve solve function solves a matrix and caches the result  

## Write a short comment describing this function
## Provides a wrapper over the matrix object and exposes a list of functions to
## operate on that
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Replaces the current matrix with the one passed and also creas the cache
  ## This sets the passed matrix to the variable at the higher scope to make it
  ## accessible by the other functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Returns the current matrix
  get <- function() x
  ## sets the inverse of the matrix. This also like set function uses assignment
  ## at a higher scope to make it available for the other functions
  setinverse <- function(inv) m <<- inv
  ## Returns the cached inverse of the matrix 'x'
  getinverse <- function() m
  ## The operations we support
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Hits the cache for the inverse, if found return that, else calulates, stores
## in cache and then return
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check with the cache first to see if we are lucky
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ## Got it in the cahce! Nothing to do, just return whatever we got
    message("cache hit")
    return(inv)
  }
  ## Solve and cache the result, then return the calculated result
  message("cache miss")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
