## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## define the function 'set' which sets the matrix x to the new value given
  set <- function(y) {
    ## matrix x is getting changed. 
    x <<- y

    ##Reset the inverse to null so it is recalculated 
    inverse <<- NULL
  }
  
  ## define the function 'get' which returns the matrix x
  get <- function() x
  
  
  ## define the function 'setinverse' which sets the inverse to new value
  setinverse <- function(inv) {
    inverse <<- inv
  }
  
  ## define the function 'getinverse' which returns the inverse
  getinverse <- function() {
    inverse
  }

  ## special "vector" which is really a list of all functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("inverse has already been calculated. so getting it from cached data")
    return(inverse)
  }
  
  ## calculate inverse of the matrix as it hasn't been calculated yet
  data <- x$get()
  inverse <- solve(data, ...)
  
  ## set the inverse value on the matrix
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}