

## Creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y)
  {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function makes the inverse of the above-created matrix 

cacheSolve <- function(x, ...) {
  ## inverse matrix of 'x'
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
