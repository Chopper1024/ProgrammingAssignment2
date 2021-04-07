## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## This function creates the special object to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function takes the above matrix inverse and computes it.  If the inverse has already been 
## calculated, then it should retrieve it from the cache.

cacheSolve <- function(x, ...) {
  inv <-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <-solve(data, ...)
  x$setInverse(inv)
  inv
}

