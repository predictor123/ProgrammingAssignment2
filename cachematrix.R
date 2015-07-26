##cachematrix can set and cache a matrix and its inverse matrix if there is one.
##If no inverse matrix has been found, cachematrix can inverse the matrix by useing
##solve() and cache the result.

##makeCacheMatrix can creat a list to set and get a matrix, set and get 
##the inverse matrix of the first matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse.matrix) inverse.matrix <<- inverse.matrix
  getinverse <- function() inverse.matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve try to find out if there is a inverse matrix cached in makeCacheMatrix firstly. 
##If it find one, cacheSolve return that matrix with the message "getting cached inverse matrix".
##If not, cacheSolve will get the matrix cached in the makeCacheMatrix and inverse it useing solve().

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
