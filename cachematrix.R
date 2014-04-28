## Program Assignment 2

## Creates a CacheMatrix from a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set= set, get= get, setinverse= setinverse, getinverse= getinverse)
}


## Checks cache before returning the inverse of a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  tryCatch(
    m <- solve(data, ...), 
    error = function(e) warning(e))
    
  x$setinverse(m)
  m
}
