## The following pair of functions provide caching the inverse of a Matrix. 
## As matrix inversion computation is normally a time-consuming operation, 
## it might be beneficial to cache it in some cases.

## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  
  setter <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  
  getter <- function() {
    x
  }
  
  inverseSetter <- function(newInverseM) {
    inverseM <<- newInverseM
  }
  
  inverseGetter <- function() {
    inverseM
  }
  
  list(
    set = setter,
    get = getter,
    setInverse = inverseSetter,
    getInverse = inverseGetter
  )
}


## The function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been computed, it simply return the value from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverse()
  if(!is.null(inverseM)) {
    return(inverseM)
  }
  
  m <- x$get()
  inverseM <- solve(m)
  x$setInverse(inverseM)
  
  inverseM
}
