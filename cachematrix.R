## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.
##
## The following functions are defined:
  
##  1.  `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
##  2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` should retrieve the inverse from the cache.

## Creastes a special "matrix" object that can cache its inverse.
## This function returns an object with defined functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Computes the inverse of the special "matrix" returned by "makeCacheMatrix",
## (re-)calculating it if necessary (i.e. if the inverse has not been calculated
## or if the matrix has changed since last calculated)
## This function uses the functions in the object 'x' to get and set persistent data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
