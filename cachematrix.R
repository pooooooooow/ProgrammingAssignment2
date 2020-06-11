## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #set the value of the matrix
  setmatinverse <- function(inv) m <<- inv #set the value of the matrix inverse
  getmatinverse <- function() m #get the value of the matrix inverse
  list(set = set, get = get,
       setmatinverse = setmatinverse,
       getmatinverse = getmatinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatinverse() 
  if(!is.null(m)) {
    message("getting cached data")
    B <- matrix(c(1,2,3,4),2,2)return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setmatinverse(m)
  m
}
