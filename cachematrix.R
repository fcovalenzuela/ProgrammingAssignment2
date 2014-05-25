## Put comments here that give an overall description of what your
## functions do

##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      setmatrix <- function(y) {                  ###Set the matrix
      x <<- y
      m <<- NULL
    }
    getmatrix <- function() x                     ###Get the matrix
    setinverse <- function(inverse) m <<- inverse ###Set the value of the inverse
    getinverse <- function() m                    ###Get the value of the inverse matrix
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
