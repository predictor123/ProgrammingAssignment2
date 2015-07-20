## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse.matrix <<- inverse.matrix
  getinverse <- function() inverse.matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse.matrix <- x$getinverse()
  if(!is.null(inverse.matrix)) {
    message("getting cached data")
    return(inverse.matrix)
  }
  data <- x$get()
  inverse.matrix <- solve(data, ...)
  x$setinverse(inverse.matrix)
  inverse.matrix
}
