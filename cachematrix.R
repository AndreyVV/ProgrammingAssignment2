## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Inverse property
  i <- NULL
  
  ## Set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get matrix
  get <- function() x
  
  ## Set inverse
  setInverse <- function(inv) {
    i <<- inv
  }
  
  ## Get inverce
  getInverse <- function() i
  
  ## Return list of the function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## if matrix cached return inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## else
  ## get matrix
  data <- x$get()
  ## calculate inverse
  m <- solve(data, ...)
  ## set inverce
  x$setInverse(m)
  ## return result
  m
}
